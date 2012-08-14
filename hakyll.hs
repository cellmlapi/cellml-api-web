{-# LANGUAGE OverloadedStrings, PatternGuards #-}
import Hakyll
import Network.HTTP.Conduit
import qualified Text.HTML.DOM as HDOM
import qualified Text.XML as DOM
import Text.XML (Element(..), Node(..), Document(..), Prologue(..), Miscellaneous(..), Instruction(..), Name(..), Doctype(..), ExternalID(..))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy.Search as LBS
import Control.Applicative
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Arrow
import Text.Hamlet.XML
import Data.DeriveTH
import Data.Derive.Data
import Data.Data
import Data.Monoid
import Text.Pandoc
import Data.Maybe
import Data.Word
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec hiding ((<|>), many)
import Data.List
import Data.Char

data PackType = TarBall | ZipFile | SelfExtracting | MacDMG deriving (Eq, Ord, Show)
data APIFileType = FTSourceCode | FTWindowsMinGW | FTWindowsMSVC2010 | FTWindowsMSVC2008 | FTMacOSX | FTLinuxX86 | FTLinuxX86_64 deriving (Eq, Ord, Show)
data BuildType = BTProduction | BTDebug deriving (Eq, Ord, Show)

data APIRelease = APIRelease {
  releaseVersion :: String,
  releaseDocumentation :: Maybe String,
  releaseFiles :: [(APIFileType, [(BuildType, [(PackType, String)])])]
                             } deriving (Eq, Ord, Show)

releaseDescriptionParser :: Parser (String, [APIRelease])
releaseDescriptionParser =
  (,) <$> (string "NextPlanned: " *> many1 (noneOf " \r\n:") <* string "\n") <*>
  (
  many $ string "Release " *>
    (APIRelease <$> (many1 (noneOf " \r\n") <* string " ") <*>
                   (((string "Documented At " *> (Just <$> many1 (noneOf " \r\n:"))) <|>
                     (string "No Documentation" *> return Nothing)) <* string ":" <* many1 (oneOf " \t\r\n")) <*>
                   (many $ ((,) <$>
                    ((string "Source " *> return FTSourceCode) <|> (try $ string "MinGW " *> return FTWindowsMinGW) <|>
                     (try $ string "MSVC2008 " *> return FTWindowsMSVC2008) <|>
                     (try $ string "MSVC2010 " *> return FTWindowsMSVC2010) <|>
                     (try $ string "MacOSX " *> return FTMacOSX) <|> (try $ string "Linux x86 " *> return FTLinuxX86) <|>
                     (string "Linux x86_64 " *> return FTLinuxX86_64)
                    ) <*>
                    (many $ (,) <$> ((string "Production " *> return BTProduction) <|> (string "Debug " *> return BTDebug)) <*>
                      (many $ (,) <$>
                       ((string "Tarball " *> return TarBall) <|> (string "Zip " *> return ZipFile) <|>
                        (try (string "Self-extracting ") *> return SelfExtracting) <|>
                        (try (string "DMG ") *> return MacDMG)) <*>
                       (many1 (noneOf " \r\n") <* many1 (oneOf " \t\r\n")))
                    ))
                   ))
  )
    
getVersions = require "versions.txt" $ \_ v -> (either (error . show) id $ parse releaseDescriptionParser "versions.txt" v)
  
buildTemplate :: IO (Template LBS.ByteString)
buildTemplate = do
  doc <- HDOM.parseLBS <$> simpleHttp "http://dev.physiomeproject.org/"
  let scripts = [NodeElement $ Element "script" attr' (if null content then [NodeContent ""] else content) |
                 Element "script" attr content <- universeBi doc,
                 Just src <- [M.lookup "src" attr],
                 attr' <- [M.insert "src" (fixupURI src) attr]] ++
                [NodeElement $ Element "script" (M.fromList [("src", "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.17/jquery-ui.min.js"),
                                                             ("type", "text/javascript")]) [NodeContent ""],
                 NodeElement $ Element "script" (M.fromList [("src", "/js/custom-api.js"),
                                                             ("type", "text/javascript")]) [NodeContent ""],
                 NodeContent "$script$"]
      fixupURI l
        | not (T.isPrefixOf "/" l), not (T.isPrefixOf "http" l) = T.cons '/' l
        | otherwise = l
      doc' = transformBi fixElementsForTemplate $ transformBi removeScripts $ doc
      removeScripts ((NodeElement (Element "script" attr nodes)):l) = l
      removeScripts x = x
      fixElementsForTemplate (Element name attr nodes)
        | name == "section" && M.lookup "id" attr == Just "content" =
          Element name attr [NodeContent "$body$"]
        | name == "a", Just l <- M.lookup "href" attr, T.isPrefixOf "/" l =
            Element name (M.insert "href" (T.append "//dev.physiomeproject.org" l) attr) nodes
        | name == "title", (NodeContent title):_ <- nodes =
            Element name attr [NodeContent $ "$title$ " `T.append` (snd $ T.breakOn "|" title)]
        | name == "link", Just l <- M.lookup "href" attr =
              Element name (M.insert "href" (fixupURI l) attr) nodes
        | name == "head" = 
          Element name attr (nodes ++ scripts ++
                             [NodeElement $ Element "link" (M.fromList [("rel", "stylesheet"), ("href", "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.17/themes/blitzer/jquery-ui.css")]) [NodeContent ""],
                              NodeElement $ Element "link" (M.fromList [("rel", "stylesheet"), ("href", "/css/style-api.css")]) [NodeContent ""],
                              NodeContent "$css$"
                             ])
      fixElementsForTemplate el = el
  return $ readTemplate $ "<!DOCTYPE html>\r\n" <> (LBS.drop 38 $ DOM.renderLBS def doc')

ensureStdMetadata = M.alter (\x -> x `mplus` (Just "")) "css" .
                    M.alter (\x -> x `mplus` (Just "")) "script"

stdPageCompiler :: Compiler (Page LBS.ByteString) (Page LBS.ByteString)
stdPageCompiler = (applyTemplateCompiler "templates/api-header.html" >>^ (\p -> p { pageMetadata = ensureStdMetadata (pageMetadata p) }))
                   >>> applyTemplateCompiler "templates/main.html"

extractDoxygenContents d =
  let a1 = fst . LBS.breakAfter "<!-- start footer part -->" . snd . LBS.breakOn "<!-- end header part -->" $ d
      a2 = fst . LBS.breakAfter "<!-- contents -->" . snd . LBS.breakOn "<!--header-->" $ d
  in
   if LBS.null a1 then a2 else a1

prependDoxygenScripts :: LBS.ByteString -> LBS.ByteString
prependDoxygenScripts d =
  (mconcat . map (\js -> "<script type='text/javascript' src='" <> js <> "'></script>") $
   [
     "dynsections.js",
     "resize.js",
     "navtree.js",
     "search/search.js"
   ]) <> d

-- This takes a Doxygen file and turns it into a page ready to go into our template.
doxygenCompiler =
  (getResourceLBS >>>
   ( (arr $ (\x -> M.fromList [("description", x), ("title", x)]) . LBS.unpack . fst .
                   LBS.breakOn "</title>" . snd . LBS.breakAfter "<title>") &&&
     (arr extractDoxygenContents >>^ prependDoxygenScripts)
   ) >>^ uncurry Page
  ) >>> applyTemplateCompiler "templates/doxygen.html" >>> stdPageCompiler

isStableRelease = null . dropWhile (\x -> isDigit x || x == '.')

makeDownloadCentre (nextPlanned, releases) =
  let
    (stableReleases, nonStableReleases) = partition (isStableRelease . releaseVersion) releases
    showFileType FTSourceCode = "Source code (all platforms)"
    showFileType FTWindowsMinGW = "Windows binaries for use with MinGW"
    showFileType FTWindowsMSVC2010 = "Windows binaries (32 bit) for use with Microsoft Visual C++ 2010"
    showFileType FTWindowsMSVC2008 = "Windows binaries (32 bit) for use with Microsoft Visual C++ 2008"
    showFileType FTMacOSX = "Mac OS X universal binaries"
    showFileType FTLinuxX86 = "Linux binaries (x86, 32 bit)"
    showFileType FTLinuxX86_64 = "Linux binaries (x86_64, 64 bit)"
    showBuildType BTProduction = "Production build"
    showBuildType BTDebug = "Build with debug symbols"
    showPackType TarBall = "Tarball"
    showPackType ZipFile = "Zip file"
    showPackType SelfExtracting = "Self-extracting exectuable"
    showPackType MacDMG = "Mac OS X Disk Image"
    showBuild (buildType, packings) = mconcat (map (showPacking buildType) packings)
    showPacking buildType (packType, url) =
      "<li><a href='" <> LBS.pack url <> "'>" <> (showBuildType buildType) <> ", " <> (showPackType packType) <> "</a></li>"
    showFile (fileType, buildList) = "<li>" <> (showFileType fileType) <> "<ul class='buildlist'>" <>
                                     (mconcat (map showBuild buildList)) <>
                                     "</ul></li>"
    showRelease r = "<div class='release'><h3 class='release'>" <> (LBS.pack . releaseVersion $ r) <> "</h3><ul class='filelist'>" <>
                    mconcat (map showFile (releaseFiles r)) <> "</ul></div>"
  in
   Page (M.fromList [("nextplanned", nextPlanned), ("title", "Download"), ("description", "Download the CellML API")]) $
     "<h2 id='stablereleaselist-title'>Stable Releases</h2>" <>
     (mconcat (map showRelease stableReleases)) <>
     "<h2 id='releasecandidatelist-title'>All Releases and Release Candidates</h2>" <>
     (mconcat (map showRelease releases))

main :: IO ()
main = do
  builtTemplate <- buildTemplate
  
  hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "doxygen/*/*.png" $ do
      route (gsubRoute "doxygen/" (const ""))
      compile copyFileCompiler
    match "doxygen/*/*.js" $ do
      route (gsubRoute "doxygen/" (const ""))
      compile copyFileCompiler
    match "doxygen/*/search/*.js" $ do
      route (gsubRoute "doxygen/" (const ""))
      compile copyFileCompiler
    match "doxygen/*/search/*.html" $ do
      route (gsubRoute "doxygen/" (const ""))
      compile copyFileCompiler
    match "doxygen/*/*.html" $ do
      route (gsubRoute "doxygen/" (const ""))
      compile doxygenCompiler

    -- Create the main template resource...
    create "templates/main.html" (constA builtTemplate)
  
    match "versions.txt" (compile $ getResourceString)
    
    match "templates/*" (compile (templateCompiler :: Compiler Resource (Template LBS.ByteString)))

    match "index.html" $ do
      route idRoute
      compile ((readPageCompiler >>^ (\page -> page { pageBody = LBS.pack $ pageBody page })) >>> stdPageCompiler)
    match "applications.html" $ do
      route idRoute
      compile ((readPageCompiler >>^ (\page -> page { pageBody = LBS.pack $ pageBody page })) >>> stdPageCompiler)
    match "support.html" $ do
      route idRoute
      compile ((readPageCompiler >>^ (\page -> page { pageBody = LBS.pack $ pageBody page })) >>> stdPageCompiler)

    create "doc-chooser.html" $
      getVersions >>>
      (arr $ \(cv, rs) -> Page { pageMetadata = M.singleton "currentVersion" cv,
                                 pageBody = mconcat $ flip mapMaybe rs $ \r -> do
                                   doc <- LBS.pack <$> releaseDocumentation r
                                   return $ "<li><a target='_top' href='" <> doc <> "'>CellML API " <> (LBS.pack $ releaseVersion r)
                                     `LBS.append` "</a></li>"
                               }
      ) >>> applyTemplateCompiler "templates/doc-chooser.html"
    match "doc-chooser.html" $ route idRoute

    create "doc-chooser-nojs.html" $ require_ "doc-chooser.html" >>>
      arr (setField "description" "See API documentation for...") >>>
      arr (setField "title" "Select API version to see documentation for") >>> stdPageCompiler
    match "doc-chooser-nojs.html" $ route idRoute

    create ".htaccess" $ (getVersions >>^ (\(nextv, _) -> Page (M.fromList [("nextplanned", nextv)]) LBS.empty)) >>>
                         applyTemplateCompiler "templates/htaccess"
    match ".htaccess" $ route idRoute
    create "download.html" $ (getVersions >>^ makeDownloadCentre) >>>
      applyTemplateCompiler "templates/download.html" >>> stdPageCompiler
    match "download.html" $ route idRoute

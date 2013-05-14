{-# LANGUAGE OverloadedStrings, PatternGuards #-}
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Core.File
import Hakyll.Core.Metadata
import Hakyll.Web.CompressCss
import Hakyll.Main
import Hakyll.Web.TemplateBS
import Hakyll.Web.TemplateBS.Read
import Hakyll.Web.TemplateBS.Context
import Network.HTTP.Conduit
import qualified Text.HTML.DOM as HDOM
import qualified Text.XML as DOM
import Text.XML (Element(..), Node(..), Document(..), Prologue(..), Miscellaneous(..), Instruction(..), Name(..), Doctype(..), ExternalID(..))
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Lazy.Search as LBS
import Control.Applicative
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Arrow
import Text.Hamlet.XML
-- import Data.DeriveTH
-- import Data.Derive.Data
-- import Data.Data
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
  releaseDate :: String,
  releaseDocumentation :: Maybe String,
  releaseFiles :: [(APIFileType, [(BuildType, [(PackType, String)])])]
                             } deriving (Eq, Ord, Show)


releaseDescriptionParser :: Parser (String, [APIRelease])
releaseDescriptionParser =
  (,) <$> (string "NextPlanned: " *> many1 (noneOf " \r\n:") <* string "\n") <*>
  (
  many $ string "Release " *>
    (APIRelease <$> (many1 (noneOf " \r\n") <* string " ") <*>
                    (many1 (noneOf " \r\n") <* string " ") <*>
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
    
getVersions :: Compiler (String, [APIRelease])
getVersions = do
  v <- load "versions.txt"
  either (error . show) return $
    parse releaseDescriptionParser "versions.txt"
      (itemBody v)
  
buildTemplate :: IO Template
buildTemplate = do
  doc <- HDOM.parseLBS <$> simpleHttp "http://www.physiomeproject.org/"
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
        | not (T.isPrefixOf "http" l)  &&
          not (T.isPrefixOf "/" l) = "//physiomeproject.org/" <> l
        | not (T.isPrefixOf "http" l ||
               T.isPrefixOf "//" l) = "//physiomeproject.org/" <> l
        | otherwise = l
      doc' = transformBi fixElementsForTemplate $ transformBi removeScripts $ doc
      removeScripts ((NodeElement (Element "script" attr nodes)):l) = l
      removeScripts x = x
      fixElementsForTemplate (Element name attr nodes)
        | name == "section" && M.lookup "id" attr == Just "content" =
          Element name attr [NodeContent "$body$"]
        | name == "a", Just l <- M.lookup "href" attr, T.isPrefixOf "/" l =
            Element name (M.insert "href" (T.append "//www.physiomeproject.org" l) attr) nodes
        | name == "img", Just l <- M.lookup "src" attr =
          Element name (M.insert "src" (fixupURI l) attr) nodes
        | name == "title", (NodeContent title):_ <- nodes =
          Element name attr [NodeContent $ (fst $ T.breakOn ":" title) <> ": $title$"]
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

myDefaultContext =
  metadataField `mappend`
  constField "css" "" `mappend`
  constField "script" "" `mappend`
  defaultContext

stdPageCompiler :: Context LBS.ByteString -> Item LBS.ByteString -> Compiler (Item LBS.ByteString)
stdPageCompiler ctx p = do
  apiHeaderWrapped <- loadAndApplyTemplate "templates/api-header.html" ctx p
  loadAndApplyTemplate "templates/main.html" ctx apiHeaderWrapped

barePageCompiler :: Context LBS.ByteString -> Item LBS.ByteString -> Compiler (Item LBS.ByteString)
barePageCompiler ctx p = do
  apiHeaderWrapped <- loadAndApplyTemplate "templates/api-header-nosidebar.html" ctx p
  loadAndApplyTemplate "templates/main.html" ctx apiHeaderWrapped

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
doxygenCompiler = do
  dgc <- itemBody <$> getResourceLBS
  pageContents <- makeItem (prependDoxygenScripts (extractDoxygenContents dgc))
  let title = (fst . LBS.breakOn "</title>" . snd . LBS.breakAfter "<title>") dgc
  stdPageCompiler (constField "title" title <>
                   constField "description" (LBS.drop 8 title) <>
                   myDefaultContext) pageContents

isStableRelease = null . dropWhile (\x -> isDigit x || x == '.')

makeDownloadCentre :: (String, [APIRelease]) -> (LBS.ByteString,
                                                 Context LBS.ByteString)
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
    showPackType SelfExtracting = "Self-extracting executable"
    showPackType MacDMG = "Mac OS X Disk Image"
    showBuild :: (BuildType, [(PackType, String)]) -> LBS.ByteString
    showBuild (buildType, packings) = mconcat (map (showPacking buildType) packings)
    showPacking buildType (packType, url) =
      "<li><a href='" <> LBSC.pack url <> "'>" <> (showBuildType buildType) <> ", " <> (showPackType packType) <> "</a></li>"
    showFile :: (APIFileType, [(BuildType, [(PackType, String)])]) -> LBS.ByteString
    showFile (fileType, buildList) = "<li>" <> (showFileType fileType) <> "<ul class='buildlist'>" <>
                                     (mconcat (map showBuild buildList)) <>
                                     "</ul></li>"
    showRelease r = "<div id='" <> (LBSC.pack . releaseVersion $ r) <>
                    "' class='release'><h3 class='release'>" <> (LBSC.pack . releaseVersion $ r) <> "(" <>
                    (LBSC.pack . releaseDate $ r) <> ")" <> "</h3><ul class='filelist'>" <>
                    mconcat (map showFile (releaseFiles r)) <> "</ul></div>"
  in
   ("<h2 id='stablereleaselist-title'>Stable Releases</h2>" <>
    (mconcat (map showRelease stableReleases)) <>
    "<h2 id='releasecandidatelist-title'>All Releases and Release Candidates</h2>" <>
    (mconcat (map showRelease releases)),
    (mconcat [constField "nextplanned" (LBSC.pack nextPlanned),
              constField "title" "Download | CellML API"]) <> myDefaultContext
   )

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
    match "doxygen/*/*/*.png" $ do
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

    match "versions.txt" (compile $ getResourceString)
    match "templates/*" (compile templateCompiler)
    create ["templates/main.html"] (compile . makeItem $ builtTemplate)
    
    match "index.html" $ do
       route idRoute
       compile $ do
         p <- fmap LBSC.pack <$> getResourceBody
         thisID <- getUnderlying
         barePageCompiler myDefaultContext p

    match "*.html" $ do
       route idRoute
       compile $ do
         p <- fmap LBSC.pack <$> getResourceBody
         thisID <- getUnderlying
         stdPageCompiler myDefaultContext p

    create ["doc-chooser.html"] $ do
      route idRoute
      compile $ do
        (curVersion, releases) <- getVersions
        let verString =
              mconcat $ flip mapMaybe releases $ \r -> do
                doc <- LBSC.pack <$> releaseDocumentation r
                return $ "<li><a target='_top' href='" <> doc <> "'>CellML API " <>
                  (LBSC.pack $ releaseVersion r) `LBS.append` "</a></li>"
        let ctx = (constField "currentVersion" (LBSC.pack curVersion)) `mappend`
                  myDefaultContext
        item <- makeItem verString
        loadAndApplyTemplate "templates/doc-chooser.html" ctx item
    create ["doc-chooser-nojs.html"] $ do
      route idRoute      
      compile $ do
        dc <- makeItem =<< (itemBody <$> load "doc-chooser.html")
        stdPageCompiler (constField "description" "See API documentation for..."
                         <> constField "title" "Select API documentation for"
                         <> myDefaultContext) dc

    create [".htaccess"] $ do
      route idRoute
      compile $ do
        (nextv, _) <- getVersions
        blankItem <- makeItem ""
        loadAndApplyTemplate "templates/htaccess"
          (constField "nextplanned" (LBSC.pack nextv))
          blankItem
    create ["download.html"] $ do
      route idRoute
      compile $ do
        (centre, ctx) <- makeDownloadCentre <$> getVersions
        stdPageCompiler (ctx `mappend`
                         constField "description" "Downloads" `mappend`
                         constField "title" "Downloads"
                        ) =<< 
          loadAndApplyTemplate "templates/download.html" ctx =<<
          makeItem centre

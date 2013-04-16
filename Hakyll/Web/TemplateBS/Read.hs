{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- | Read templates in Hakyll's native format
module Hakyll.Web.TemplateBS.Read
    ( readTemplate
    ) where


--------------------------------------------------------------------------------
import           Data.List                    (isPrefixOf)
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LBSC


--------------------------------------------------------------------------------
import           Hakyll.Web.TemplateBS.Internal


--------------------------------------------------------------------------------
-- | Construct a @Template@ from a string.
readTemplate :: LBS.ByteString -> Template
readTemplate = Template . readTemplate'
  where
    readTemplate' string
        | LBS.null string = []
        | "$$" `LBS.isPrefixOf` string =
            Escaped : readTemplate' (LBS.drop 2 string)
        | "$" `LBS.isPrefixOf` string =
            case readKey (LBS.drop 1 string) of
                Just (key, rest) -> Key (LBSC.unpack key) : readTemplate' rest
                Nothing          -> Chunk "$" : readTemplate' (LBS.drop 1 string)
        | otherwise =
            let (chunk, rest) = LBSC.break (== '$') string
            in Chunk chunk : readTemplate' rest

    -- Parse an key into (key, rest) if it's valid, and return
    -- Nothing otherwise
    readKey string =
        let (key, rest) = LBSC.span validKeyChar string
        in if not (LBS.null key) && "$" `LBS.isPrefixOf` rest
            then Just (key, LBS.drop 1 rest)
            else Nothing

    validKeyChar x = x `notElem` ['$', '\n', '\r']

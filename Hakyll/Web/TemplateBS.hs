{-# LANGUAGE OverloadedStrings #-}
-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert items into a string. They are perfectly suited
-- for laying out your site.
--
-- Let's look at an example template:
--
-- > <html>
-- >     <head>
-- >         <title>My crazy homepage - $title$</title>
-- >     </head>
-- >     <body>
-- >         <div id="header">
-- >             <h1>My crazy homepage - $title$</h1>
-- >         </div>
-- >         <div id="content">
-- >             $body$
-- >         </div>
-- >         <div id="footer">
-- >             By reading this you agree that I now own your soul
-- >         </div>
-- >     </body>
-- > </html>
--
-- As you can see, the format is very simple -- @$key$@ is used to render the
-- @$key$@ field from the page, everything else is literally copied. If you want
-- to literally insert @\"$key$\"@ into your page (for example, when you're
-- writing a Hakyll tutorial) you can use
--
-- > <p>
-- >     A literal $$key$$.
-- > </p>
--
-- Because of it's simplicity, these templates can be used for more than HTML:
-- you could make, for example, CSS or JS templates as well.
module Hakyll.Web.TemplateBS
    ( Template
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , applyTemplateWith
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                (forM, liftM)
import           Data.Monoid                  (mappend)
import           Prelude                      hiding (id)
import qualified Data.ByteString.Lazy          as LBS


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Web.TemplateBS.Context
import           Hakyll.Web.TemplateBS.Internal
import           Hakyll.Web.TemplateBS.Read

--------------------------------------------------------------------------------
-- | Read a template.
templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Hakyll.Web.TemplateBS.templateCompiler" $ do
    item <- getResourceLBS
    return $ fmap readTemplate item


--------------------------------------------------------------------------------
applyTemplate :: Template                -- ^ Template
              -> Context a               -- ^ Context
              -> Item a                  -- ^ Page
              -> Compiler (Item LBS.ByteString)  -- ^ Resulting item
applyTemplate tpl context item = do
    -- Appending missingField gives better error messages
    let context' k x = unContext (context `mappend` missingField) k x
    body <- applyTemplateWith context' tpl item
    return $ itemSetBody body item


--------------------------------------------------------------------------------
-- | The following pattern is so common:
--
-- > tpl <- loadBody "templates/foo.html"
-- > someCompiler
-- >     >>= applyTemplate tpl context
--
-- That we have a single function which does this:
--
-- > someCompiler
-- >     >>= loadAndApplyTemplate "templates/foo.html" context
loadAndApplyTemplate :: Identifier              -- ^ Template identifier
                     -> Context a               -- ^ Context
                     -> Item a                  -- ^ Page
                     -> Compiler (Item LBS.ByteString)  -- ^ Resulting item
loadAndApplyTemplate identifier context item = do
    tpl <- loadBody identifier
    applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | It is also possible that you want to substitute @$key$@s within the body of
-- an item. This function does that by interpreting the item body as a template,
-- and then applying it to itself.
applyAsTemplate :: Context LBS.ByteString          -- ^ Context
                -> Item LBS.ByteString             -- ^ Item and template
                -> Compiler (Item LBS.ByteString)  -- ^ Resulting item
applyAsTemplate context item =
    let tpl = readTemplate $ itemBody item
    in applyTemplate tpl context item


--------------------------------------------------------------------------------
-- | Overloaded apply template function to work in an arbitrary Monad.
applyTemplateWith :: Monad m
                  => (String -> a -> m LBS.ByteString)
                  -> Template -> a -> m LBS.ByteString
applyTemplateWith context tpl x = liftM LBS.concat $
    forM (unTemplate tpl) $ \e -> case e of
        Chunk c -> return c
        Escaped -> return "$"
        Key k   -> context k x

{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 as BL
import Control.Monad

-- | Replace dollar signs by '\(' and '\)'. Dirty trick.
correctDollarSign :: String -> String
correctDollarSign s = helper s (0 :: Int)
    where helper "" _ = ""
          helper ('$':'$':x) n = '$':(helper ('$':x) 3)
          helper ('$':x) 0 = '\\':'(':(helper x 1)
          helper ('$':x) 1 = '\\':')':(helper x 0)
          helper ('$':x) 2 = '$':(helper x 0)
          helper (a:x) 3 = a:(helper x 2)
          helper (a:x) n = a:(helper x n)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe Text
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "PropoList"
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just $ T.pack $ correctDollarSign $ T.unpack $ unTextarea res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "PropoList"
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "homepage")

sampleForm :: Form Textarea
sampleForm = renderDivs $
    areq textareaField "New Theorem:" Nothing

-- entryForm :: Form Thm
-- areqMaybe field fs mdef = fmap Just (areq field fs $ join mdef)

--entryForm :: RenderMessage master FormMessage =>
--         Maybe Thm -> Html ->
--         Form sub master Thm

--entryForm thm = renderDivs $ Thm
--    <$> areqMaybe textField "Type" (thmType <$> thm)
--    <*> areqMaybe textField "Content" (thmContent <$> thm)
--    <*> aopt textField "Proof" Nothing
--    <*> aopt textField "Name" Nothing
--    <*> aopt textField "Signature" Nothing
--    <*> aopt textField "Ref" Nothing

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
            FormSuccess res -> Just $ Textarea . T.pack $ correctDollarSign $ T.unpack $ unTextarea res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "PropoList"
        addStylesheet $ StaticR css_bootstrap_css
        addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        $(widgetFile "homepage")

sampleForm :: Form Textarea
sampleForm = renderDivs $
    areq textareaField "New Proposition:" Nothing

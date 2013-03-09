{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Model

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Control.Applicative

-- import Control.Arrow (&&&)

-- | Replace dollar signs by '\(' and '\)'. Dirty trick.
correctDollarSign :: Text -> Text
correctDollarSign s = T.pack $ helper (T.unpack s) (0 :: Int)
    where helper "" _ = ""
          helper ('$':'$':x) _ = '$':(helper ('$':x) 3)
          helper ('$':x) 0 = '\\':'(':(helper x 1)
          helper ('$':x) 1 = '\\':')':(helper x 0)
          helper ('$':x) 2 = '$':(helper x 0)
          helper (a:x) 3 = a:(helper x 2)
          helper (a:x) n = a:(helper x n)

overallWidget :: Widget
overallWidget = do
    setTitle "PropoList"
    addStylesheet $ StaticR css_bootstrap_css
    addScriptRemote "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    toWidget [hamlet|
<div .page-header>
    <h1> PropoList :: [Proposition]
    <p .lead>List & review your propositions, definitions, theorems, all things crazy.
|]

footerWidget :: Widget
footerWidget = do
    toWidget [hamlet|
<div #footer>
    <div .container>
        <p .muted .credit> Powered by <a href="http://www.yesodweb.com/">Yesod</a>, the magnificent <a href="http://www.haskell.org/haskellwiki/Haskell">Haskell</a>, and <a href="http://www.mathjax.org/">MathJax</a>.<br> Hacked together by <a href="https://github.com/lambdaloop">Pierre Karashchuk</a> and <a href="https://github.com/concretevitamin">Zongheng Yang</a>.
|]

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost thmForm
    let submission = Nothing :: Maybe Thm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        overallWidget
        $(widgetFile "homepage")
        footerWidget


correctThmDollars :: Thm -> Thm
correctThmDollars thm = thm { thmContent = correctDollarSign (thmContent thm) }
--                            , thmProof = fmap correctDollarSign (thmProof thm) }


postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost thmForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
          FormSuccess res -> Just $ correctThmDollars res
          _ -> Nothing

    case submission of
      Just s -> runDB $ insert s
      _ -> undefined

    defaultLayout $ do
        aDomId <- lift newIdent
        overallWidget
        $(widgetFile "homepage")
        footerWidget

sampleForm :: Form Textarea
sampleForm = renderDivs $
    areq textareaField "New Proposition:" Nothing

thmForm :: Form Thm
thmForm = renderDivs $ Thm
          <$> areq (selectFieldList categories) "Category" Nothing
          <*> (unTextarea <$> areq textareaField "Content" Nothing)
          -- <*> (liftA unTextarea <$> aopt textareaField "Proof" Nothing)
          -- <*> aopt textField "Name" Nothing
          <*> (ThmSignature
               <$> (areq textField "Signature From" Nothing)
               <*> (areq textField "Signature To" Nothing))
          -- <*> aopt textField "Reference" Nothing
          -- <*> aopt textField "Note" Nothing
  where categories = Import.map (\ x -> (T.pack $ show x, x)) $ [minBound .. maxBound]


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

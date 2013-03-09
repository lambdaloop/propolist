{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Types

import Control.Monad

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
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Propolist: list, catalog, manage your crazy propositions "
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing


-- entryForm :: Form Thm
areqMaybe field fs mdef = fmap Just (areq field fs $ join mdef)

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

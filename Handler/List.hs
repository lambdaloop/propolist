{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.List where

import Import

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Control.Applicative

import Handler.Home (overallWidget, footerWidget)

-- import Control.Arrow (&&&)

-- listWidget :: Widget
-- listWidget = do
--     toWidget [hamlet|
-- #{thmCategory $ head thms}
-- |]

-- | Handler for /list, the list of theorems added.
getListR :: Handler RepHtml
getListR = do
    thms <- runDB $ selectList ([] :: [Filter Thm]) [LimitTo 10]
    defaultLayout $ do
        aDomId <- lift newIdent
        overallWidget
        toWidget [whamlet|
<h1> Added Theorem <br>
$forall Entity thmId thm <- thms
    <p>#{show $ thmCategory thm}
    <p>#{thmContent thm}
|]
        footerWidget
        $(widgetFile "list")

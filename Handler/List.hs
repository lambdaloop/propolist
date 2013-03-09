{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.List where

import Import

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Control.Applicative

import Handler.Home (overallWidget, footerWidget, listNavWidget)

-- import Control.Arrow (&&&)

-- listWidget :: Widget
-- listWidget = do
--     toWidget [hamlet|
-- #{thmCategory $ head thms}
-- ]

-- | Handler for /list, the list of theorems added.
getListR :: Handler RepHtml
getListR = do
    thms <- runDB $ selectList ([] :: [Filter Thm]) [LimitTo 10]
    defaultLayout $ do
        aDomId <- lift newIdent
        overallWidget
        listNavWidget
        toWidget [whamlet|
<h3 .text-success> Your [Proposition] <br>
$forall Entity thmId thm <- thms
    <p><p style="font-size:large;">#{show $ thmCategory thm}. <p style="font-size:medium;"> #{thmContent thm}
                                           <p style="font-size:small;"> #{show $ thmSign thm}<br> <br>
|]
        footerWidget
        $(widgetFile "list")

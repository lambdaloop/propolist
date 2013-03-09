{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.List where

import Import

import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Control.Applicative
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
    defaultLayout [whamlet|
<h1> [Added Theorem] <br>
$forall Entity thmId thm <- thms
    <p>#{thmCategory thm}
    <p>#{thmContent thm}
|]

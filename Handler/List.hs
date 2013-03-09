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
-- ]

-- | Handler for /list, the list of theorems added.
getListR :: Handler RepHtml
getListR = do
    thms <- runDB $ selectList ([] :: [Filter Thm]) [LimitTo 10]
    defaultLayout $
       [whamlet|
 <h1> [Theorem] <br>
 $forall Entity thmId thm <- thms
    <p>
      <h4> #{show $ thmName thm}
      <h5> #{show $ thmCategory thm}
      <p style="font-size:medium;"> #{thmContent thm}
      <p> #{show $ thmProof thm}
      <p> #{show $ thmName thm}
      <p> #{show $ thmSign thm}
      <p> #{show $ thmRef thm}
      <p> #{show $ thmNote thm}
 |]

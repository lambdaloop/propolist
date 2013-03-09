module Types where

import qualified Data.Text as T (Text)
import Prelude

-- | Internal Thm Type, representing a theorem entity.
data Thm = Thm
  { thmType ::          Maybe ThmType
  , thmContent ::       Maybe ThmContent
  , thmProof ::         Maybe ThmProof
  , thmName ::          Maybe ThmName
  , thmSignature ::     Maybe ThmSignature
  , thmRef ::           Maybe ThmRef -- references; e.g. urls, paper title, book section
  , thmNote ::          Maybe ThmNote -- notes
  } deriving (Eq, Show)

data ThmType = Corrolary
             | Definition
             | Theorem
             | Proposition
             | Lemma
             | Algorithm
             | Remark
             deriving (Show, Eq)

                    -- ThmSignature  From To
                    -- Examples:
                    -- Reals -> ModEquation
                    -- Equation -> Equation
data ThmSignature = ThmSignature [ThmStatement] [ThmStatement] deriving (Show, Eq)

type ThmContent = T.Text
type ThmName = T.Text
type ThmProof = T.Text
type ThmRef = T.Text
type ThmNote = T.Text

type ThmStatement = T.Text

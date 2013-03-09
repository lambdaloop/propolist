module Types where

import qualified Data.Text as T (Text)

-- | Internal Thm Type, representing a theorem entity.
data Thm = Thm
  { thmType ::          ThmType
  , thmContent ::       ThmContent
  , thmName ::          Maybe ThmName
  , thmSignature ::     Maybe ThmSignature
  , thmProof ::         Maybe ThmProof
  , thmRef ::           Maybe ThmRef -- references; e.g. urls, paper title, book section
  , thmNote ::          Maybe ThmNote -- notes
  }

data ThmType = Corrolary
             | Definition
             | Theorem
             | Proposition
             | Lemma
             | Algorithm
             | Remark

                    -- ThmSignature  From To
                    -- Examples:
                    -- Reals -> ModEquation
                    -- Equation -> Equation
data ThmSignature = ThmSignature [ThmStatement] [ThmStatement]

type ThmContent = T.Text
type ThmName = T.Text
type ThmProof = T.Text
type ThmRef = T.Text
type ThmNote = T.Text

type ThmStatement = T.Text

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time (Day)


-- | Internal Thm Type, representing a theorem entity.
-- data Thm = Thm
--   { category ::          ThmType
--   , content ::       ThmContent
--   , proof ::         Maybe ThmProof
--   , name ::          Maybe ThmName
--   , sign ::     Maybe ThmSignature
--   , ref ::           Maybe ThmRef -- references; e.g. urls, paper title, book section
--   , note ::          Maybe ThmNote -- notes
--   } deriving (Eq, Show)

data ThmCategory = Corrolary | Definition
             | Theorem
             | Proposition
             | Lemma
             | Algorithm
             | Remark
             deriving (Show, Eq, Read, Enum, Bounded)
derivePersistField "ThmCategory"

-- ThmSignature  From To
-- Examples:
-- Reals -> ModEquation
-- Equation -> Equation
-- data ThmSignature = ThmSignature [ThmStatement] [ThmStatement] deriving (Show, Eq)

type ThmContent = Text
type ThmName = Text
type ThmProof = Text
type ThmRef = Text
type ThmNote = Text

type ThmStatement = Text

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

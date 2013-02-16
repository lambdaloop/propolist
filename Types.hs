data ThmType = Corrolary | Definition | Theorem | Proposition | Lemma | Algorithm
type ThmStatement = String

                    -- ThmSignature  From To
                    -- Examples:
                    -- Reals -> ModEquation
                    -- Equation -> Equation
data ThmSignature = ThmSignature [ThmStatement] [ThmStatement]
type ThmContent = String
type ThmProof = String
type ThmRef = String

data Thm =
  { thmType ::          ThmType
  , thmContent ::       ThmContent

  , thmName ::          Maybe String
  , thmSignature ::     Maybe ThmSignature
  , thmProof ::         Maybe ThmProof
  , thmRef ::           Maybe ThmRef
  }

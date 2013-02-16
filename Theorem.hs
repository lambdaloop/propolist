data ThmType = Corrolary | Definition | Theorem | Proposition | Lemma | Algorithm
type ThmStatement = String

                    -- ThmSignature  From To
                    -- Examples:
                    -- Reals -> ModEquation
                    -- Equation -> Equation
data ThmSignature = ThmSignature TheoremStatement TheoremStatement
type ThmContent = String
type ThmProof = String
type ThmRef = String

data Thm =
  { thmType :: TheoremType
  , thmName :: String
  , thmSignature :: ThmSignature
  , thmContent ::ThmContent
  , thmProof :: ThmProof
  , thmRef :: ThmRef}

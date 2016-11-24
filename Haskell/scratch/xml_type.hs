{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}


class ExpressionElementInner e
data ExpressionElement = forall e . (Show e, ExpressionElementInner e) => ExpressionElement e

instance Show ExpressionElement where
  show (ExpressionElement e) = "<ExpressionElement>" ++ show e ++ "</ExpressionElement>"

data Place = Place Name
instance ExpressionElementInner Place
instance Show Place where
  show (Place name) = "<Place>" ++ show name ++ "</Place>"

data Name = Name String
instance Show Name where
  show (Name s) = "<Name>" ++ s ++ "</Name>"

data IntegerLiteral = IntegerLiteral Value
instance ExpressionElementInner IntegerLiteral
instance Show IntegerLiteral where
  show (IntegerLiteral v) = "<IntegerLiteral>" ++ show v ++ "</IntegerLiteral>"

data Value = Value Int
instance Show Value where
  show (Value i) = "<Value>" ++ show i ++ "</Value>"

data Binary = Binary Operator ExpressionElement ExpressionElement
instance ExpressionElementInner Binary
instance Show Binary where
  show (Binary op left right) =
    "<Binary>" ++
      show op ++
      "<Left>" ++
        show left ++
      "</Left>" ++
      "<Right>" ++
        show right ++
      "</Right>" ++
    "</Binary>"

data Operator = Operator String
instance Show Operator where
  show (Operator s) = "<Operator>" ++ s ++ "</Operator>"

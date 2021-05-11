{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Map ((!), Map, empty, insert)

class Expression repr where
class Expression repr => Literal repr where literal :: Integer -> repr
infixl 1 `add`; class Expression repr => Add repr where add :: repr -> repr -> repr
infixl 2 `multiply`; class Expression repr => Multiply repr where multiply :: repr -> repr -> repr
infixl 1 `substract`; class Expression repr => Substract repr where substract :: repr -> repr -> repr

---

data IExpression where
  ILiteral :: Integer -> IExpression
  IAdd :: IExpression -> IExpression -> IExpression
  IMultiply :: IExpression -> IExpression -> IExpression
  ISubstract :: IExpression -> IExpression -> IExpression
  deriving (Eq, Show, Read)

getIExpression :: IExpression -> IExpression
getIExpression = id

instance Expression IExpression where
instance Literal IExpression where literal = ILiteral
instance Add IExpression where add = IAdd
instance Multiply IExpression where multiply = IMultiply
instance Substract IExpression where substract = ISubstract

---

newtype Depth = Depth { getDepth :: Integer } deriving (Eq, Ord, Enum, Show, Num)
binaryDepth l r = succ $ max l r
instance Expression Depth where
instance Literal Depth where literal = const 0
instance Add Depth where add = binaryDepth
instance Multiply Depth where multiply = binaryDepth
instance Substract Depth where substract = binaryDepth

---

newtype Evaluate = Evaluate { evaluate :: Integer } deriving (Eq, Ord, Enum, Show, Num)
instance Expression Evaluate where
instance Literal Evaluate where literal = Evaluate
instance Add Evaluate where add = (+)
instance Multiply Evaluate where multiply = (*)
instance Substract Evaluate where substract = (-)

---

class Expression repr => Variable repr where variable :: String -> repr
class Expression repr => Binding repr where binding :: String -> repr -> repr -> repr

type Context = Map String Integer

data EvaluateWithVariables
  = EvaluateWithVariables
    { evaluateWithContext :: Context -> Integer
    }

evaluateWithVariables = flip evaluateWithContext empty

liftEvaluateBinary f l r
  = EvaluateWithVariables lifting
  where
    lifting context
      = go l `f` go r
      where
        go = flip evaluateWithContext context

instance Variable EvaluateWithVariables
  where
    variable
      = EvaluateWithVariables
      . flip (!)

instance Binding EvaluateWithVariables
  where
    binding name value body
      = EvaluateWithVariables withBinding
      where
        withBinding outerContext
          = evaluateWithContext body innerContext
          where
            innerContext = insert name evaluatedValue outerContext
            evaluatedValue = evaluateWithContext value outerContext

instance Expression EvaluateWithVariables where
instance Literal EvaluateWithVariables where literal = EvaluateWithVariables . const
instance Add EvaluateWithVariables where add = liftEvaluateBinary (+)
instance Multiply EvaluateWithVariables where multiply = liftEvaluateBinary (*)
instance Substract EvaluateWithVariables where substract = liftEvaluateBinary (-)

instance Variable Depth where variable = const 0
instance Binding Depth where binding _ = binaryDepth

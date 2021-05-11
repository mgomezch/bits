{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M

data UnaryOperator
  = Factorial
  deriving (Eq, Show, Read)

data BinaryOperator
  = Add
  | Multiply
  | Substract
  deriving (Eq, Show, Read)

data Expression
  = UnaryOperator { unaryOperator :: UnaryOperator, body :: Expression }
  | BinaryOperator { binaryOperator :: BinaryOperator, left, right :: Expression }
  | LiteralNumber { literalNumber :: Result }
  | Binding { name :: String, value, body :: Expression }
  | Variable { name :: String }
  deriving (Eq, Show, Read)



depth :: Expression -> Word
depth = \ case

  LiteralNumber {} -> 0

  Variable {} -> 0

  UnaryOperator {..}
    -> succ $ depth body

  BinaryOperator {..}
    -> succ
    $ maximum
    $ depth <$> [left, right]

  Binding {..}
    -> succ
    $ maximum
    $ depth <$> [value, body]



type Result = Integer
type Context = M.Map String Result

evaluate :: Expression -> Result
evaluate
  = evaluateWithContext M.empty
  where

    evaluateUnaryOperator :: UnaryOperator -> Result -> Result
    evaluateUnaryOperator = \ case
      Factorial -> factorial
      where
        factorial 0 = 1
        factorial n = n * factorial (pred n)

    evaluateBinaryOperator :: BinaryOperator -> Result -> Result -> Result
    evaluateBinaryOperator = \ case
      Add -> (+)
      Multiply -> (*)
      Substract -> (-)

    evaluateWithContext :: Context -> Expression -> Result
    evaluateWithContext context
      = evaluateInCurrentContext
      where

        evaluateInCurrentContext :: Expression -> Result

        evaluateInCurrentContext LiteralNumber {..} = literalNumber

        evaluateInCurrentContext UnaryOperator {..}
          = evaluateUnaryOperator unaryOperator
            (evaluateInCurrentContext body)

        evaluateInCurrentContext BinaryOperator {..}
          = evaluateBinaryOperator binaryOperator
            (evaluateInCurrentContext left)
            (evaluateInCurrentContext right)

        evaluateInCurrentContext Variable {..} = context M.! name

        evaluateInCurrentContext Binding {..}
          = evaluateWithContext newContext body
          where
            newContext :: Context
            newContext
              = M.insert name boundValue context
              where
                boundValue :: Result
                boundValue = evaluateInCurrentContext value

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except

data LispVal 
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool deriving (Eq)

newtype IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

type EnvCtx = Map.Map T.Text LispVal

instance Show LispVal where
  show = T.unpack . showLispVal
    where
      showLispVal :: LispVal -> T.Text
      showLispVal v = 
        case v of
          (Atom atom) -> atom
          (String s) -> T.concat [ "\"", s, "\""]
          (Number n) -> T.pack $ show n
          (Bool True) -> "#t"
          (Bool False) -> "#f"
          Nil -> "Nil"
          (List xs) -> T.concat ["(", T.unwords $ showLispVal <$> xs, ")"]
          (Fun _) -> "(internal function)"
          (Lambda _ _ ) -> "(lambda function)"

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving ( Monad
           , Functor
           , Applicative 
           , MonadReader EnvCtx
           , MonadIO)





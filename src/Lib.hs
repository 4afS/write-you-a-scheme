{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except

run :: IO ()
run = putStrLn "main running..."

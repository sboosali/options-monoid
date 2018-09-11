--------------------------------------------------
--------------------------------------------------

{-|



-}

module Options.EnvironmentVariable

 ( module Options.EnvironmentVariable
 , module Options.EnvironmentVariable.Types
 ) where

--------------------------------------------------

import Options.EnvironmentVariable.Types

--------------------------------------------------

import           "transformers" Control.Monad.Trans.Except

--------------------------------------------------

import qualified "base" System.Environment as IO
import           "base" Data.Bifunctor

--------------------------------------------------

import Internal.Options.Monoid

import Prelude_options_monoid

--------------------------------------------------
--------------------------------------------------

{-| 

@
'readEnvironmentVariable' ≡ 'parseEnvironmentVariable' 'readEither'
@

-}

readEnvironmentVariable
  :: forall a. (Read a, Typeable a)
  => EnvironmentVariable 
  -> ExceptT EnvironmentError IO a

readEnvironmentVariable = parseEnvironmentVariable go
  where
  go = readEither > bimap addError id

  addError e = "{ :: " <> expected <> " } " <> e

  expected = typeStringOf proxy
  proxy = (Proxy :: Proxy a)

--------------------------------------------------

{-| 

-}

parseEnvironmentVariable
  :: (String -> Either String a)
  -> EnvironmentVariable 
  -> ExceptT EnvironmentError IO a

parseEnvironmentVariable parse var = do
  s <- getEnvironmentVariable var
  let result = go s
  result

  where
  go = parse > bimap toError id

  toError s = EnvironmentError var (EnvironmentVariableDoesNotParse s)

--------------------------------------------------

{-| 

-}

getEnvironmentVariable
  :: EnvironmentVariable 
  -> ExceptT EnvironmentError IO String

getEnvironmentVariable var@(EnvironmentVariable x) = do
  y <- IO.lookupEnv x
  let result = go y
  return result

  where
  go = maybe toError (Right)

  toError = EnvironmentError var (EnvironmentVariableIsMissing)

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
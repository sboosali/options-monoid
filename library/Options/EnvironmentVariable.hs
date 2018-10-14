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

import           "mtl"          Control.Monad.Except
---import           "transformers" Control.Monad.Trans.Except

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
  :: forall a m.
     ( Read     a
     , Typeable a
     , MonadError EnvironmentError m
     , MonadIO                     m
     )
  => EnvironmentVariable 
  -> m a

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
  :: ( MonadIO                     m
     , MonadError EnvironmentError m
     )
  => (String -> Either String a)
  -> EnvironmentVariable 
  -> m a

parseEnvironmentVariable parse var = do
  s <- getEnvironmentVariable var
  let result = go s
  liftEither result

  where
  go = parse > bimap toError id

  toError s = EnvironmentError var (VariableDoesNotParse s)

--------------------------------------------------

{-| Wraps 'IO.lookupEnv'.

-}

getEnvironmentVariable
  :: ( MonadIO                     m
     , MonadError EnvironmentError m
     )
  => EnvironmentVariable
  -> m String

getEnvironmentVariable var@(EnvironmentVariable s) = do
  EnvironmentVariable x <- liftEither $ validateEnvironmentVariable s
  y <- liftIO $ IO.lookupEnv x
  let result = go y
  liftEither result

  where
  go = maybe (Left toError) (Right)

  toError = EnvironmentError var (VariableIsMissing)

--------------------------------------------------

{-| 

See 'IO.setEnv':

@
* System.Environment> setEnv "=A=" "1"
*** Exception: setEnv: invalid argument
* System.Environment> setEnv "A" "1"

* System.Environment> setEnv "" "1"
*** Exception: setEnv: invalid argument
* System.Environment> setEnv " " "1"

@

-}

validateEnvironmentVariable :: String -> Either EnvironmentError EnvironmentVariable
validateEnvironmentVariable s =
  if   isBad
  then Left $ EnvironmentError x VariableNameIsInvalid
  else Right x

  where
  x = EnvironmentVariable s
  
  isBad
     = "" == s
    || all isSpace s


--------------------------------------------------

{-| 

'readEnvironmentVariableIO' specializes 'readEnvironmentVariable'.

Example:

@
> :set -XOverloadedStrings
> import Prelude
> readEnvironmentVariableIO "DESKTOP_MODE" :: IO Int
1
@

-}

readEnvironmentVariableIO
  :: forall a.
     ( Read     a
     , Typeable a
     )
  => EnvironmentVariable 
  -> IO a

readEnvironmentVariableIO =
  readEnvironmentVariable > throwLeft

  where
  throwLeft :: ExceptT EnvironmentError IO a -> IO a
  throwLeft m = do
    e <- runExceptT m
    e & either throwIO return

--------------------------------------------------

{-| 

'getEnvironmentVariableIO' specializes 'getEnvironmentVariable'.

Example:

@
> :set -XOverloadedStrings
> import Prelude
> getEnvironmentVariableIO "EDITOR" >>= putStrLn
emacsclient
@

-}

getEnvironmentVariableIO
  :: EnvironmentVariable 
  -> IO String

getEnvironmentVariableIO =
  getEnvironmentVariable > throwLeft

  where
  throwLeft :: ExceptT EnvironmentError IO a -> IO a
  throwLeft m = do
    e <- runExceptT m
    e & either throwIO return

--------------------------------------------------

{-| 

-}

isEnvSetAndNotOnlyWhitespaace :: EnvironmentVariable -> IO Bool
isEnvSetAndNotOnlyWhitespaace = isEnvSetAnd (not . isOnlyWhitespaace)

  where
  isOnlyWhitespaace = all isSpace

--------------------------------------------------

{-| 

-}

isEnvSetAndNotEmpty :: EnvironmentVariable -> IO Bool
isEnvSetAndNotEmpty = isEnvSetAnd (/= "")

--------------------------------------------------

{-| 

-}

isEnvSetAnd :: (String -> Bool) -> EnvironmentVariable -> IO Bool
isEnvSetAnd predicate (EnvironmentVariable x) = do
  go <$> IO.lookupEnv x

  where
  go = maybe False predicate

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code


  where
  proxy = Nothing :: Maybe a





type IOError = IOException


data IOErrorType
  | NoSuchThing
  | InvalidArgument


doesNotExistErrorType :: IOErrorType 
I/O error where the operation failed because one of its arguments does not exist.


ioError :: IOError -> IO a
Raise an IOError in the IO monad.



ioeGetErrorString :: IOError -> String
ioeSetErrorString :: IOError -> String      -> IOError
ioeSetErrorType   :: IOError -> IOErrorType -> IOError


mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError

Construct an IOError of the given type where the second argument describes the error location and the third and fourth argument contain the file handle and file path of the file involved in the error if applicable.








Dhall.Bash

    $ dhall-to-bash --declare FOO <<< '[] : Optional Integer'
    unset FOO

    $ dhall-to-bash --declare FOO <<< '[1] : Optional Integer'
    declare -r -i FOO=1

    $ dhall-to-bash --declare FOO <<< '[1, 2, 3]'
    declare -r -a FOO=(1 2 3)

    $ dhall-to-bash --declare FOO <<< '{ bar = 1, baz = True }'
    declare -r -A FOO=([bar]=1 [baz]=true)

    $ dhall-to-bash --declare FOO <<< '[[] : Optional Integer] : Optional (Optional Integer)'
    unset FOO

    $ dhall-to-bash --declare FOO <<< '[[1] : Optional Integer] : Optional (Optional Integer)'
    declare -r -i FOO=1








-}
--------------------------------------------------
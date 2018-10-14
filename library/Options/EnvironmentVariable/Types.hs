--------------------------------------------------
--------------------------------------------------

{-|



-}

module Options.EnvironmentVariable.Types where

--------------------------------------------------



--------------------------------------------------

import Prelude_options_monoid

--------------------------------------------------
--------------------------------------------------

{-|

-}

newtype EnvironmentVariable = EnvironmentVariable

  String

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

instance IsString EnvironmentVariable where
  fromString = coerce

--------------------------------------------------

{-| 

-}

data EnvironmentError

  = EnvironmentError EnvironmentVariable EnvironmentVariableError

  deriving stock    (Show)
  deriving stock    (Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Exception)
  deriving anyclass (NFData,Hashable)

-- -- | custom and unlawful; for 'Exception'.
-- instance Show EnvironmentError where
--   showsPrec = _showsPrec

--------------------------------------------------

{-| 


-}

data EnvironmentVariableError

  = VariableNameIsInvalid
  | VariableIsMissing
  | VariableIsEmpty
  | VariableDoesNotParse String

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-|

NOTE:

* @'EnvironmentVariable 'Nothing'@ means: "the environment variable is unset".
* @'EnvironmentVariable ('Just' "")@ means: "the environment variable is set, but empty".

and:

* @'EnvironmentVariable ('Just' s)@ means: "the environment variable is set to the string @s@".

-}

newtype EnvironmentValue = EnvironmentValue
  { getEnvironmentValue ::
      (Maybe String)
  }

  deriving stock    (Show,Read,Lift,Generic)
  deriving newtype  (Eq,Ord,Semigroup,Monoid)
  deriving newtype  (NFData,Hashable)

-- | @= 'fromString_EnvironmentValue'
instance IsString EnvironmentValue where
  fromString = fromString_EnvironmentValue

--------------------------------------------------

{-| Parser for the string values of environment variables.

A dumb "parser" that just matches against @Nothing@ or a @String@ (i.e. doesn't consume input).

-}

newtype EnvironmentParser a = EnvironmentParser
  { fromEnvironmentParser ::
      (EnvironmentValue -> a)
  }

  deriving stock    (Generic)
  deriving newtype  (Functor,NFData)

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-|

e.g. this @haskell@ expression:

@
> :set -XOverloadedStrings
> 'EnvironmentBinding' "GHC_VER" ('EnvironmentValue' ('Just' "8.4.3"))
@

corresponds to this @bash@ statement:

@
GHC_VER=8.4.3
@


e.g. this @haskell@ expression:

@
> :set -XOverloadedStrings
> 'EnvironmentBinding' "GHC_VER" ('EnvironmentValue' 'Nothing')
@

corresponds to this @bash@ statement:

@
unset GHC_VER
@


-}

data EnvironmentBinding = EnvironmentBinding
  { name  :: EnvironmentVariable
  , value :: EnvironmentValue
  }

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------
--------------------------------------------------

{-|

@
= 'Just' > 'EnvironmentValue'
@

NOTE Does not normalize an empty string to a missing environment variable.

-}

fromString_EnvironmentValue :: String -> EnvironmentValue
fromString_EnvironmentValue = Just > EnvironmentValue

--------------------------------------------------
--------------------------------------------------
{- Notes / Old Code



data EnvironmentError

  = EnvironmentVariableIsMissing
  | EnvironmentVariableIsEmpty
  | EnvironmentVariableDoesNotParse String

  deriving (Show, Exception)



-}
--------------------------------------------------
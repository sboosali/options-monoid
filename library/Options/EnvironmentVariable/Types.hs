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

  deriving stock    (Read,Eq,Ord,Lift,Generic)
  deriving anyclass (Exception)
  deriving anyclass (NFData,Hashable)

-- | custom and unlawful; for 'Exception'.
instance Show EnvironmentError where
  showsPrec = _showsPrec

--------------------------------------------------

{-| 

-}

data EnvironmentVariableError

  = EnvironmentVariableIsMissing
  | EnvironmentVariableIsEmpty
  | EnvironmentVariableDoesNotParse String

  deriving stock    (Show,Read,Eq,Ord,Lift,Generic)
  deriving anyclass (NFData,Hashable)

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

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
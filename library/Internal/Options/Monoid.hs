--------------------------------------------------
--------------------------------------------------

{-|



-}

module Internal.Options.Monoid where

--------------------------------------------------

import Data.Typeable

--------------------------------------------------

import Prelude_options_monoid

--------------------------------------------------
--------------------------------------------------

{-| Calls 'show' on 'typeRep'.

e.g.

@
>>> :set -XTypeApplications
>>> import Prelude
>>> import Data.Typeable
>>> import Data.Proxy
>>> typeStringOf (Proxy @(Either String Int))
"Either [Char] Int"
@

-}

typeStringOf
  :: forall a proxy. (Typeable a)
  => proxy a
  -> String

typeStringOf proxy = string
  where
  string         = show representation
  representation = typeRep proxy

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------
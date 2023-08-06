module Deku.DOM.Attr.Async where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Script (Script_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Async = Async

instance Attr Script_ Async  String  where
  attr Async value = unsafeAttribute $ Left $  
    { key: "async", value: prop' value }
instance Attr Script_ Async (Event.Event  String ) where
  attr Async eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "async", value: prop' value }


instance Attr everything Async  Unit  where
  attr Async _ = unsafeAttribute $ Left $  { key: "async", value: unset' }
instance Attr everything Async (Event.Event  Unit ) where
  attr Async eventValue = unsafeAttribute $ Right $ eventValue <#> \_ ->
    { key: "async", value: unset' }

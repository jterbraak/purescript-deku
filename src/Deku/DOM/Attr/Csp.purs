module Deku.DOM.Attr.Csp where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Iframe (Iframe_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Csp = Csp

instance Attr Iframe_ Csp  String  where
  attr Csp value = unsafeAttribute $ Left $  { key: "csp", value: prop' value }
instance Attr Iframe_ Csp (Event.Event  String ) where
  attr Csp eventValue = unsafeAttribute $ Right $ eventValue <#> \value ->
    { key: "csp", value: prop' value }


instance Attr everything Csp  Unit  where
  attr Csp _ = unsafeAttribute $ Left $  { key: "csp", value: unset' }
instance Attr everything Csp (Event.Event  Unit ) where
  attr Csp eventValue = unsafeAttribute $ Right $ eventValue <#> \_ ->
    { key: "csp", value: unset' }

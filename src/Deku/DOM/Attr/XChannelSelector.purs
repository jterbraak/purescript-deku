module Deku.DOM.Attr.XChannelSelector where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeDisplacementMap (FeDisplacementMap_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data XChannelSelector = XChannelSelector

instance Attr FeDisplacementMap_ XChannelSelector  String  where
  attr XChannelSelector value = unsafeAttribute $ Left $  
    { key: "xChannelSelector", value: prop' value }
instance Attr FeDisplacementMap_ XChannelSelector (Event.Event  String ) where
  attr XChannelSelector eventValue = unsafeAttribute $ Right $ eventValue
    <#> \value -> { key: "xChannelSelector", value: prop' value }


instance Attr everything XChannelSelector  Unit  where
  attr XChannelSelector _ = unsafeAttribute $ Left $  
    { key: "xChannelSelector", value: unset' }
instance Attr everything XChannelSelector (Event.Event  Unit ) where
  attr XChannelSelector eventValue = unsafeAttribute $ Right $ eventValue
    <#> \_ -> { key: "xChannelSelector", value: unset' }

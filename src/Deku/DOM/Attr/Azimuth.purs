module Deku.DOM.Attr.Azimuth where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.FeDistantLight (FeDistantLight_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Azimuth = Azimuth

instance Attr FeDistantLight_ Azimuth  String  where
  attr Azimuth value = unsafeAttribute (  
    { key: "azimuth", value: prop' value  } <$ _)
instance Attr FeDistantLight_ Azimuth (Event.Event  String ) where
  attr Azimuth eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "azimuth", value: prop' value }


instance Attr everything Azimuth  Unit  where
  attr Azimuth _ = unsafeAttribute (  { key: "azimuth", value: unset'  } <$ _)
instance Attr everything Azimuth (Event.Event  Unit ) where
  attr Azimuth eventValue = unsafeAttribute \_ -> eventValue <#> \_ ->
    { key: "azimuth", value: unset' }

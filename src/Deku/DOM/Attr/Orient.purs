module Deku.DOM.Attr.Orient where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.Marker (Marker_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Orient = Orient

instance Attr Marker_ Orient  String  where
  attr Orient value = unsafeAttribute (  
    { key: "orient", value: prop' value  } <$ _)
instance Attr Marker_ Orient (Event.Event  String ) where
  attr Orient eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "orient", value: prop' value }


instance Attr everything Orient  Unit  where
  attr Orient _ = unsafeAttribute (  { key: "orient", value: unset'  } <$ _)
instance Attr everything Orient (Event.Event  Unit ) where
  attr Orient eventValue = unsafeAttribute \_ -> eventValue <#> \_ ->
    { key: "orient", value: unset' }

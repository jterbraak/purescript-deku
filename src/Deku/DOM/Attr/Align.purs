module Deku.DOM.Attr.Align where


import Prelude

import FRP.Event as Event
import Deku.DOM.Elt.Applet (Applet_)
import Deku.DOM.Elt.Caption (Caption_)
import Deku.DOM.Elt.Col (Col_)
import Deku.DOM.Elt.Colgroup (Colgroup_)
import Deku.DOM.Elt.Hr (Hr_)
import Deku.DOM.Elt.Iframe (Iframe_)
import Deku.DOM.Elt.Img (Img_)
import Deku.DOM.Elt.Table (Table_)
import Deku.DOM.Elt.Tbody (Tbody_)
import Deku.DOM.Elt.Td (Td_)
import Deku.DOM.Elt.Tfoot (Tfoot_)
import Deku.DOM.Elt.Th (Th_)
import Deku.DOM.Elt.Thead (Thead_)
import Deku.DOM.Elt.Tr (Tr_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Align = Align

instance Attr Applet_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Applet_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Caption_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Caption_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Col_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Col_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Colgroup_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Colgroup_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Hr_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Hr_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Iframe_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Iframe_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Img_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Img_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Table_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Table_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Tbody_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Tbody_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Td_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Td_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Tfoot_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Tfoot_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Th_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Th_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Thead_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Thead_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr Tr_ Align  String  where
  attr Align value = unsafeAttribute (  
    { key: "align", value: prop' value  } <$ _)
instance Attr Tr_ Align (Event.Event  String ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#>
    \value -> { key: "align", value: prop' value }


instance Attr everything Align  Unit  where
  attr Align _ = unsafeAttribute (  { key: "align", value: unset'  } <$ _)
instance Attr everything Align (Event.Event  Unit ) where
  attr Align eventValue = unsafeAttribute \_ -> eventValue <#> \_ ->
    { key: "align", value: unset' }

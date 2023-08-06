module Deku.DOM.Attr.AriaValuetext where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.View (View_)
import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Circle (Circle_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data AriaValuetext = AriaValuetext

instance Attr Circle_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Circle_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Ellipse_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Ellipse_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr ForeignObject_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr ForeignObject_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr G_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr G_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Line_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Line_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Marker_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Marker_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Path_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Path_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Polygon_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Polygon_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Polyline_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Polyline_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Rect_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Rect_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Svg_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Svg_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Symbol_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Symbol_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Text_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Text_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr TextPath_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr TextPath_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Tspan_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Tspan_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr Use_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr Use_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr View_ AriaValuetext  String  where
  attr AriaValuetext value = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: prop' value }
instance Attr View_ AriaValuetext (Event.Event  String ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-valuetext", value: prop' value }


instance Attr everything AriaValuetext  Unit  where
  attr AriaValuetext _ = unsafeAttribute $ Left $  
    { key: "aria-valuetext", value: unset' }
instance Attr everything AriaValuetext (Event.Event  Unit ) where
  attr AriaValuetext eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "aria-valuetext", value: unset' }

module Deku.DOM.Attr.AriaDropeffect where

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

data AriaDropeffect = AriaDropeffect

instance Attr Circle_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Circle_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Ellipse_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Ellipse_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr ForeignObject_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr ForeignObject_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr G_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr G_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Line_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Line_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Marker_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Marker_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Path_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Path_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Polygon_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Polygon_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Polyline_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Polyline_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Rect_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Rect_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Svg_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Svg_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Symbol_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Symbol_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Text_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Text_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr TextPath_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr TextPath_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Tspan_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Tspan_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr Use_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr Use_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr View_ AriaDropeffect  String  where
  attr AriaDropeffect value = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: prop' value }
instance Attr View_ AriaDropeffect (Event.Event  String ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "aria-dropeffect", value: prop' value }


instance Attr everything AriaDropeffect  Unit  where
  attr AriaDropeffect _ = unsafeAttribute $ Left $  
    { key: "aria-dropeffect", value: unset' }
instance Attr everything AriaDropeffect (Event.Event  Unit ) where
  attr AriaDropeffect eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "aria-dropeffect", value: unset' }

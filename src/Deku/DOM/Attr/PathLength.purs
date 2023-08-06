module Deku.DOM.Attr.PathLength where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Circle (Circle_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data PathLength = PathLength

instance Attr Circle_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Circle_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Ellipse_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Ellipse_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Line_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Line_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Path_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Path_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Polygon_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Polygon_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Polyline_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Polyline_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr Rect_ PathLength  String  where
  attr PathLength value = unsafeAttribute $ Left $  
    { key: "pathLength", value: prop' value }
instance Attr Rect_ PathLength (Event.Event  String ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "pathLength", value: prop' value }


instance Attr everything PathLength  Unit  where
  attr PathLength _ = unsafeAttribute $ Left $  
    { key: "pathLength", value: unset' }
instance Attr everything PathLength (Event.Event  Unit ) where
  attr PathLength eventValue = unsafeAttribute $ Right $ eventValue <#>
    \_ -> { key: "pathLength", value: unset' }

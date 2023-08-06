module Deku.DOM.Attr.PointsAtX where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeSpotLight (FeSpotLight_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data PointsAtX = PointsAtX

instance Attr FeSpotLight_ PointsAtX (NonEmpty.NonEmpty Event.Event  String ) where
  attr PointsAtX bothValues = unsafeAttribute $ Both (pure 
    { key: "pointsAtX", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "pointsAtX", value: prop' value })
instance Attr FeSpotLight_ PointsAtX (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr PointsAtX (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "pointsAtX", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "pointsAtX", value: prop' value })
instance Attr FeSpotLight_ PointsAtX  String  where
  attr PointsAtX value = unsafeAttribute $ This $ pure $
    { key: "pointsAtX", value: prop' value }
instance Attr FeSpotLight_ PointsAtX (Event.Event  String ) where
  attr PointsAtX eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "pointsAtX", value: prop' value }

instance Attr FeSpotLight_ PointsAtX (ST.ST Global.Global  String ) where
  attr PointsAtX iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "pointsAtX", value: prop' value }

instance Attr everything PointsAtX (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr PointsAtX bothValues = unsafeAttribute $ Both (pure 
    { key: "pointsAtX", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "pointsAtX", value: unset' })
instance Attr everything PointsAtX (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr PointsAtX (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "pointsAtX", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "pointsAtX", value: unset' })
instance Attr everything PointsAtX  Unit  where
  attr PointsAtX _ = unsafeAttribute $ This $ pure $
    { key: "pointsAtX", value: unset' }
instance Attr everything PointsAtX (Event.Event  Unit ) where
  attr PointsAtX eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "pointsAtX", value: unset' }

instance Attr everything PointsAtX (ST.ST Global.Global  Unit ) where
  attr PointsAtX iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "pointsAtX", value: unset' }

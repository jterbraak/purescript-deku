module Deku.DOM.Attr.Values where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.FeColorMatrix (FeColorMatrix_)
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.DOM.Elt.AnimateMotion (AnimateMotion_)
import Deku.DOM.Elt.Animate (Animate_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Values = Values

instance Attr Animate_ Values (NonEmpty.NonEmpty Event.Event  String ) where
  attr Values bothValues = unsafeAttribute $ Both (pure 
    { key: "values", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr Animate_ Values (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Values (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "values", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr Animate_ Values  String  where
  attr Values value = unsafeAttribute $ This $ pure $
    { key: "values", value: prop' value }
instance Attr Animate_ Values (Event.Event  String ) where
  attr Values eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "values", value: prop' value }

instance Attr Animate_ Values (ST.ST Global.Global  String ) where
  attr Values iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "values", value: prop' value }

instance Attr AnimateMotion_ Values (NonEmpty.NonEmpty Event.Event  String ) where
  attr Values bothValues = unsafeAttribute $ Both (pure 
    { key: "values", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr AnimateMotion_ Values (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Values (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "values", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr AnimateMotion_ Values  String  where
  attr Values value = unsafeAttribute $ This $ pure $
    { key: "values", value: prop' value }
instance Attr AnimateMotion_ Values (Event.Event  String ) where
  attr Values eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "values", value: prop' value }

instance Attr AnimateMotion_ Values (ST.ST Global.Global  String ) where
  attr Values iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "values", value: prop' value }

instance Attr AnimateTransform_ Values (NonEmpty.NonEmpty Event.Event  String ) where
  attr Values bothValues = unsafeAttribute $ Both (pure 
    { key: "values", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr AnimateTransform_ Values (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Values (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "values", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr AnimateTransform_ Values  String  where
  attr Values value = unsafeAttribute $ This $ pure $
    { key: "values", value: prop' value }
instance Attr AnimateTransform_ Values (Event.Event  String ) where
  attr Values eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "values", value: prop' value }

instance Attr AnimateTransform_ Values (ST.ST Global.Global  String ) where
  attr Values iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "values", value: prop' value }

instance Attr FeColorMatrix_ Values (NonEmpty.NonEmpty Event.Event  String ) where
  attr Values bothValues = unsafeAttribute $ Both (pure 
    { key: "values", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr FeColorMatrix_ Values (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Values (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "values", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "values", value: prop' value })
instance Attr FeColorMatrix_ Values  String  where
  attr Values value = unsafeAttribute $ This $ pure $
    { key: "values", value: prop' value }
instance Attr FeColorMatrix_ Values (Event.Event  String ) where
  attr Values eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "values", value: prop' value }

instance Attr FeColorMatrix_ Values (ST.ST Global.Global  String ) where
  attr Values iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "values", value: prop' value }

instance Attr everything Values (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Values bothValues = unsafeAttribute $ Both (pure 
    { key: "values", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "values", value: unset' })
instance Attr everything Values (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Values (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "values", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "values", value: unset' })
instance Attr everything Values  Unit  where
  attr Values _ = unsafeAttribute $ This $ { key: "values", value: unset' }
instance Attr everything Values (Event.Event  Unit ) where
  attr Values eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "values", value: unset' }

instance Attr everything Values (ST.ST Global.Global  Unit ) where
  attr Values iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "values", value: unset' }

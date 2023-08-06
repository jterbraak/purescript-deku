module Deku.DOM.Attr.MaskContentUnits where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Mask (Mask_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data MaskContentUnits = MaskContentUnits

instance Attr Mask_ MaskContentUnits (NonEmpty.NonEmpty Event.Event  String ) where
  attr MaskContentUnits bothValues = unsafeAttribute $ Both (pure 
    { key: "maskContentUnits", value: prop' (NonEmpty.head bothValues) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "maskContentUnits", value: prop' value }
    )
instance Attr Mask_ MaskContentUnits (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr MaskContentUnits (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "maskContentUnits", value: prop' (value) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "maskContentUnits", value: prop' value }
    )
instance Attr Mask_ MaskContentUnits  String  where
  attr MaskContentUnits value = unsafeAttribute $ This $ pure $
    { key: "maskContentUnits", value: prop' value }
instance Attr Mask_ MaskContentUnits (Event.Event  String ) where
  attr MaskContentUnits eventValue = unsafeAttribute $ That $ eventValue
    <#> \value -> { key: "maskContentUnits", value: prop' value }

instance Attr Mask_ MaskContentUnits (ST.ST Global.Global  String ) where
  attr MaskContentUnits iValue = unsafeAttribute $ This $ iValue
    <#> \value -> { key: "maskContentUnits", value: prop' value }

instance Attr everything MaskContentUnits (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr MaskContentUnits bothValues = unsafeAttribute $ Both (pure 
    { key: "maskContentUnits", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "maskContentUnits", value: unset' })
instance Attr everything MaskContentUnits (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr MaskContentUnits (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "maskContentUnits", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "maskContentUnits", value: unset' })
instance Attr everything MaskContentUnits  Unit  where
  attr MaskContentUnits _ = unsafeAttribute $ This $ pure $
    { key: "maskContentUnits", value: unset' }
instance Attr everything MaskContentUnits (Event.Event  Unit ) where
  attr MaskContentUnits eventValue = unsafeAttribute $ That $ eventValue
    <#> \_ -> { key: "maskContentUnits", value: unset' }

instance Attr everything MaskContentUnits (ST.ST Global.Global  Unit ) where
  attr MaskContentUnits iValue = unsafeAttribute $ This $ iValue
    <#> \_ -> { key: "maskContentUnits", value: unset' }

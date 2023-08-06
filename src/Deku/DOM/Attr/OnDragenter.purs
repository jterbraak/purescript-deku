module Deku.DOM.Attr.OnDragenter where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty
import Effect (Effect)
import Deku.Attribute (class Attr, Cb(..), cb', unsafeAttribute, unset')

data OnDragenter = OnDragenter

instance Attr anything OnDragenter (NonEmpty.NonEmpty Event.Event  Cb ) where
  attr OnDragenter bothValues = unsafeAttribute $ Both (pure 
    { key: "dragenter", value: cb' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "dragenter", value: cb' value })
instance Attr anything OnDragenter (Product.Product (ST.ST Global.Global) Event.Event  Cb ) where
  attr OnDragenter (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "dragenter", value: cb' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "dragenter", value: cb' value })
instance Attr anything OnDragenter  Cb  where
  attr OnDragenter value = unsafeAttribute $ This $ pure $
    { key: "dragenter", value: cb' value }
instance Attr anything OnDragenter (Event.Event  Cb ) where
  attr OnDragenter eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "dragenter", value: cb' value }

instance Attr anything OnDragenter (ST.ST Global.Global  Cb ) where
  attr OnDragenter iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "dragenter", value: cb' value }

instance Attr anything OnDragenter (NonEmpty.NonEmpty Event.Event  (Effect Unit) ) where
  attr OnDragenter bothValues = unsafeAttribute $ Both (pure 
    { key: "dragenter", value: cb' (Cb (const ((NonEmpty.head bothValues) $> true))) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "dragenter", value: cb' (Cb (const (value $> true))) }
    )
instance Attr anything OnDragenter (Product.Product (ST.ST Global.Global) Event.Event  (Effect Unit) ) where
  attr OnDragenter (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "dragenter", value: cb' (Cb (const ((value) $> true))) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "dragenter", value: cb' (Cb (const (value $> true))) }
    )
instance Attr anything OnDragenter  (Effect Unit)  where
  attr OnDragenter value = unsafeAttribute $ This $ pure $
    { key: "dragenter", value: cb' (Cb (const (value $> true))) }
instance Attr anything OnDragenter (Event.Event  (Effect Unit) ) where
  attr OnDragenter eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "dragenter", value: cb' (Cb (const (value $> true))) }

instance Attr anything OnDragenter (ST.ST Global.Global  (Effect Unit) ) where
  attr OnDragenter iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "dragenter", value: cb' (Cb (const (value $> true))) }

instance Attr anything OnDragenter (NonEmpty.NonEmpty Event.Event  (Effect Boolean) ) where
  attr OnDragenter bothValues = unsafeAttribute $ Both (pure 
    { key: "dragenter", value: cb' (Cb (const (NonEmpty.head bothValues))) })
    ( NonEmpty.tail bothValues <#> \value ->
        { key: "dragenter", value: cb' (Cb (const value)) }
    )
instance Attr anything OnDragenter (Product.Product (ST.ST Global.Global) Event.Event  (Effect Boolean) ) where
  attr OnDragenter (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "dragenter", value: cb' (Cb (const (value))) })
    ( Tuple.snd bothValues <#> \value ->
        { key: "dragenter", value: cb' (Cb (const value)) }
    )
instance Attr anything OnDragenter  (Effect Boolean)  where
  attr OnDragenter value = unsafeAttribute $ This $ pure $
    { key: "dragenter", value: cb' (Cb (const value)) }
instance Attr anything OnDragenter (Event.Event  (Effect Boolean) ) where
  attr OnDragenter eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "dragenter", value: cb' (Cb (const value)) }

instance Attr anything OnDragenter (ST.ST Global.Global  (Effect Boolean) ) where
  attr OnDragenter iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "dragenter", value: cb' (Cb (const value)) }

instance Attr everything OnDragenter (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr OnDragenter bothValues = unsafeAttribute $ Both (pure 
    { key: "dragenter", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "dragenter", value: unset' })
instance Attr everything OnDragenter (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr OnDragenter (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "dragenter", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "dragenter", value: unset' })
instance Attr everything OnDragenter  Unit  where
  attr OnDragenter _ = unsafeAttribute $ This $ pure $
    { key: "dragenter", value: unset' }
instance Attr everything OnDragenter (Event.Event  Unit ) where
  attr OnDragenter eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "dragenter", value: unset' }

instance Attr everything OnDragenter (ST.ST Global.Global  Unit ) where
  attr OnDragenter iValue = unsafeAttribute $ This $ iValue #
    \_ -> { key: "dragenter", value: unset' }

module Deku.DOM.Attr.Csp where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Iframe (Iframe_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Csp = Csp

instance Attr Iframe_ Csp (NonEmpty.NonEmpty Event.Event  String ) where
  attr Csp bothValues = unsafeAttribute $ Both (pure 
    { key: "csp", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "csp", value: prop' value })
instance Attr Iframe_ Csp (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Csp (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "csp", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "csp", value: prop' value })
instance Attr Iframe_ Csp  String  where
  attr Csp value = unsafeAttribute $ This $ { key: "csp", value: prop' value }
instance Attr Iframe_ Csp (Event.Event  String ) where
  attr Csp eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "csp", value: prop' value }

instance Attr Iframe_ Csp (ST.ST Global.Global  String ) where
  attr Csp iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "csp", value: prop' value }

instance Attr everything Csp (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Csp bothValues = unsafeAttribute $ Both (pure  { key: "csp", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "csp", value: unset' })
instance Attr everything Csp (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Csp (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->   { key: "csp", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "csp", value: unset' })
instance Attr everything Csp  Unit  where
  attr Csp _ = unsafeAttribute $ This $ { key: "csp", value: unset' }
instance Attr everything Csp (Event.Event  Unit ) where
  attr Csp eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "csp", value: unset' }

instance Attr everything Csp (ST.ST Global.Global  Unit ) where
  attr Csp iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "csp", value: unset' }

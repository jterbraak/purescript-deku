module Deku.DOM.Attr.Codebase where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Applet (Applet_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Codebase = Codebase

instance Attr Applet_ Codebase (NonEmpty.NonEmpty Event.Event  String ) where
  attr Codebase bothValues = unsafeAttribute $ Both (pure 
    { key: "codebase", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "codebase", value: prop' value })
instance Attr Applet_ Codebase (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Codebase (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "codebase", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "codebase", value: prop' value })
instance Attr Applet_ Codebase  String  where
  attr Codebase value = unsafeAttribute $ This $ pure $
    { key: "codebase", value: prop' value }
instance Attr Applet_ Codebase (Event.Event  String ) where
  attr Codebase eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "codebase", value: prop' value }

instance Attr Applet_ Codebase (ST.ST Global.Global  String ) where
  attr Codebase iValue = unsafeAttribute $ This $ iValue #
    \value -> { key: "codebase", value: prop' value }

instance Attr everything Codebase (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Codebase bothValues = unsafeAttribute $ Both (pure 
    { key: "codebase", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "codebase", value: unset' })
instance Attr everything Codebase (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Codebase (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->  
    { key: "codebase", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "codebase", value: unset' })
instance Attr everything Codebase  Unit  where
  attr Codebase _ = unsafeAttribute $ This $ pure $
    { key: "codebase", value: unset' }
instance Attr everything Codebase (Event.Event  Unit ) where
  attr Codebase eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "codebase", value: unset' }

instance Attr everything Codebase (ST.ST Global.Global  Unit ) where
  attr Codebase iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "codebase", value: unset' }

module Deku.DOM.Attr.Pointerout where

import Control.Semigroupoid ((<<<))
import Deku.Attribute as Deku.Attribute
import Data.Unit as Data.Unit

data Pointerout = Pointerout

instance Deku.Attribute.Attr everything Pointerout Data.Unit.Unit where
  attr _ _ = Deku.Attribute.unsafeAttribute { key: "pointerout", value: Deku.Attribute.unset' }

instance Deku.Attribute.Attr everything Pointerout String where
  attr _ = Deku.Attribute.unsafeAttribute <<< { key: "pointerout", value: _ } <<<
    Deku.Attribute.prop'

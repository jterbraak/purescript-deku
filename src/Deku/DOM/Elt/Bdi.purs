module Deku.DOM.Elt.Bdi where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Bdi_

bdi
  :: Array (Attribute Bdi_)
  -> Array Nut
  -> Nut
bdi = DC.elementify2 "bdi"

bdi_
  :: Array Nut
  -> Nut
bdi_ = bdi empty

bdi__
  :: String
  -> Nut
bdi__ t = bdi_ [ DC.text_ t ]

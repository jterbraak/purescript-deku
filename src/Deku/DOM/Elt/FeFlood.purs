module Deku.DOM.Elt.FeFlood where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data FeFlood_

feFlood
  :: Array (Attribute FeFlood_)
  -> Array Nut
  -> Nut
feFlood = DC.elementify2 "feFlood"

feFlood_
  :: Array Nut
  -> Nut
feFlood_ = feFlood empty

feFlood__
  :: String
  -> Nut
feFlood__ t = feFlood_ [ DC.text t ]

module Deku.DOM.Elt.Details where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Details_

details
  :: Array (Attribute Details_)
  -> Array Nut
  -> Nut
details = DC.elementify2 "details"

details_
  :: Array Nut
  -> Nut
details_ = details empty

details__
  :: String
  -> Nut
details__ t = details_ [ DC.text t ]

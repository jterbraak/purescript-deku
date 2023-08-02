module Deku.DOM.Elt.Rp where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Rp_

rp
  :: Array (Attribute Rp_)
  -> Array Nut
  -> Nut
rp = DC.elementify2 "rp"

rp_
  :: Array Nut
  -> Nut
rp_ = rp empty

rp__
  :: String
  -> Nut
rp__ t = rp_ [ DC.text t ]

module Deku.DOM.Elt.Samp where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Samp_

samp
  :: Array (Attribute Samp_)
  -> Array Nut
  -> Nut
samp = DC.elementify2 "samp"

samp_
  :: Array Nut
  -> Nut
samp_ = samp empty

samp__
  :: String
  -> Nut
samp__ t = samp_ [ DC.text t ]

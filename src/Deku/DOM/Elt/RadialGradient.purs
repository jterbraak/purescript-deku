module Deku.DOM.Elt.RadialGradient where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data RadialGradient_

radialGradient
  :: Array (Attribute RadialGradient_)
  -> Array Nut
  -> Nut
radialGradient = DC.elementify2 "radialGradient"

radialGradient_
  :: Array Nut
  -> Nut
radialGradient_ = radialGradient empty

radialGradient__
  :: String
  -> Nut
radialGradient__ t = radialGradient_ [ DC.text_ t ]

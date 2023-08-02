module Deku.DOM.Elt.FeFuncG where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data FeFuncG_

feFuncG
  :: Array (Attribute FeFuncG_)
  -> Array Nut
  -> Nut
feFuncG = DC.elementify2 "feFuncG"

feFuncG_
  :: Array Nut
  -> Nut
feFuncG_ = feFuncG empty

feFuncG__
  :: String
  -> Nut
feFuncG__ t = feFuncG_ [ DC.text t ]

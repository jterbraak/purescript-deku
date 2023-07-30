module Deku.DOM.Elt.G where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data G_

g
  :: Array (Attribute G_)
  -> Array Nut
  -> Nut
g = DC.elementify2 "g"

g_
  :: Array Nut
  -> Nut
g_ = g empty

g__
  :: String
  -> Nut
g__ t = g_ [ DC.text_ t ]

module Deku.DOM.Elt.U where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data U_

u
  :: Array (Attribute U_)
  -> Array Nut
  -> Nut
u = DC.elementify2 "u"

u_
  :: Array Nut
  -> Nut
u_ = u empty

u__
  :: String
  -> Nut
u__ t = u_ [ DC.text_ t ]

module Deku.DOM.Elt.Wbr where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Wbr_

wbr
  :: Array (Attribute Wbr_)
  -> Array Nut
  -> Nut
wbr = DC.elementify2 "wbr"

wbr_
  :: Array Nut
  -> Nut
wbr_ = wbr empty

wbr__
  :: String
  -> Nut
wbr__ t = wbr_ [ DC.text_ t ]

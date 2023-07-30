module Deku.DOM.Elt.Tt where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Tt_

tt
  :: Array (Attribute Tt_)
  -> Array Nut
  -> Nut
tt = DC.elementify2 "tt"

tt_
  :: Array Nut
  -> Nut
tt_ = tt empty

tt__
  :: String
  -> Nut
tt__ t = tt_ [ DC.text_ t ]

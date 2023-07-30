module Deku.DOM.Elt.Img where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Img_

img
  :: Array (Attribute Img_)
  -> Array Nut
  -> Nut
img = DC.elementify2 "img"

img_
  :: Array Nut
  -> Nut
img_ = img empty

img__
  :: String
  -> Nut
img__ t = img_ [ DC.text_ t ]

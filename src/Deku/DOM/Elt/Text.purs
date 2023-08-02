module Deku.DOM.Elt.Text where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Text_

text
  :: Array (Attribute Text_)
  -> Array Nut
  -> Nut
text = DC.elementify2 "text"

text_
  :: Array Nut
  -> Nut
text_ = text empty

text__
  :: String
  -> Nut
text__ t = text_ [ DC.text t ]

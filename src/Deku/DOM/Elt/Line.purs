module Deku.DOM.Elt.Line where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Line_

line
  :: Array (Attribute Line_)
  -> Array Nut
  -> Nut
line = DC.elementify2 "line"

line_
  :: Array Nut
  -> Nut
line_ = line empty

line__
  :: String
  -> Nut
line__ t = line_ [ DC.text_ t ]

module Deku.DOM.Elt.Use where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Use_

use
  :: Array (Attribute Use_)
  -> Array Nut
  -> Nut
use = DC.elementify2 "use"

use_
  :: Array Nut
  -> Nut
use_ = use empty

use__
  :: String
  -> Nut
use__ t = use_ [ DC.text_ t ]

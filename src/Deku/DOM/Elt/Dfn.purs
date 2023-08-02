module Deku.DOM.Elt.Dfn where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Dfn_

dfn
  :: Array (Attribute Dfn_)
  -> Array Nut
  -> Nut
dfn = DC.elementify2 "dfn"

dfn_
  :: Array Nut
  -> Nut
dfn_ = dfn empty

dfn__
  :: String
  -> Nut
dfn__ t = dfn_ [ DC.text t ]

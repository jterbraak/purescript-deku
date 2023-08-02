module Deku.DOM.Elt.Fieldset where

import Control.Plus (empty)
import Deku.Attribute (Attribute)
import Deku.Control as DC
import Deku.Core (Nut)

data Fieldset_

fieldset
  :: Array (Attribute Fieldset_)
  -> Array Nut
  -> Nut
fieldset = DC.elementify2 "fieldset"

fieldset_
  :: Array Nut
  -> Nut
fieldset_ = fieldset empty

fieldset__
  :: String
  -> Nut
fieldset__ t = fieldset_ [ DC.text t ]

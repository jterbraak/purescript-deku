module Deku.DOM.Attr.ClipPathUnits where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.ClipPath (ClipPath_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data ClipPathUnits = ClipPathUnits

instance Attr ClipPath_ ClipPathUnits String where
  attr ClipPathUnits bothValues  = unsafeAttribute $ Both { key: "clipPathUnits", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "clipPathUnits", value:  prop' value  })
  pureAttr ClipPathUnits value  = unsafeAttribute $ This { key: "clipPathUnits", value:  prop' value  }
  unpureAttr ClipPathUnits eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "clipPathUnits", value:  prop' value  }

instance Attr everything ClipPathUnits Unit where
  attr ClipPathUnits bothValues  = unsafeAttribute $ Both { key: "clipPathUnits", value:  unset'  } (snd bothValues <#> \_ -> { key: "clipPathUnits", value:  unset'  })
  pureAttr ClipPathUnits _  = unsafeAttribute $ This { key: "clipPathUnits", value:  unset'  }
  unpureAttr ClipPathUnits eventValue  = unsafeAttribute $ That $ eventValue <#> \_ -> { key: "clipPathUnits", value:  unset'  }

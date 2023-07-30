module Deku.DOM.Attr.Reversed where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.Ol (Ol_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Reversed = Reversed

instance Attr Ol_ Reversed String where
  attr Reversed bothValues  = unsafeAttribute $ Both { key: "reversed", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "reversed", value:  prop' value  })
  pureAttr Reversed value  = unsafeAttribute $ This { key: "reversed", value:  prop' value  }
  unpureAttr Reversed eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "reversed", value:  prop' value  }

instance Attr everything Reversed Unit where
  attr Reversed bothValues  = unsafeAttribute $ Both { key: "reversed", value:  unset'  } (snd bothValues <#> \_ -> { key: "reversed", value:  unset'  })
  pureAttr Reversed _  = unsafeAttribute $ This { key: "reversed", value:  unset'  }
  unpureAttr Reversed eventValue  = unsafeAttribute $ That $ eventValue <#> \_ -> { key: "reversed", value:  unset'  }

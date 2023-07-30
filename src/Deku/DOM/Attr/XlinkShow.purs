module Deku.DOM.Attr.XlinkShow where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.Mpath (Mpath_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.Filter (Filter_)
import Deku.DOM.Elt.FeImage (FeImage_)
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data XlinkShow = XlinkShow

instance Attr AnimateTransform_ XlinkShow String where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "xlink:show", value:  prop' value  })
  pureAttr XlinkShow value  = unsafeAttribute $ This { key: "xlink:show", value:  prop' value  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "xlink:show", value:  prop' value  }

instance Attr FeImage_ XlinkShow String where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "xlink:show", value:  prop' value  })
  pureAttr XlinkShow value  = unsafeAttribute $ This { key: "xlink:show", value:  prop' value  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "xlink:show", value:  prop' value  }

instance Attr Filter_ XlinkShow String where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "xlink:show", value:  prop' value  })
  pureAttr XlinkShow value  = unsafeAttribute $ This { key: "xlink:show", value:  prop' value  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "xlink:show", value:  prop' value  }

instance Attr Image_ XlinkShow String where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "xlink:show", value:  prop' value  })
  pureAttr XlinkShow value  = unsafeAttribute $ This { key: "xlink:show", value:  prop' value  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "xlink:show", value:  prop' value  }

instance Attr Mpath_ XlinkShow String where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "xlink:show", value:  prop' value  })
  pureAttr XlinkShow value  = unsafeAttribute $ This { key: "xlink:show", value:  prop' value  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "xlink:show", value:  prop' value  }

instance Attr everything XlinkShow Unit where
  attr XlinkShow bothValues  = unsafeAttribute $ Both { key: "xlink:show", value:  unset'  } (snd bothValues <#> \_ -> { key: "xlink:show", value:  unset'  })
  pureAttr XlinkShow _  = unsafeAttribute $ This { key: "xlink:show", value:  unset'  }
  unpureAttr XlinkShow eventValue  = unsafeAttribute $ That $ eventValue <#> \_ -> { key: "xlink:show", value:  unset'  }

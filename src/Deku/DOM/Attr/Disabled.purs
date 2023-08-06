module Deku.DOM.Attr.Disabled where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.Either (Either(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.Button (Button_)
import Deku.DOM.Elt.Fieldset (Fieldset_)
import Deku.DOM.Elt.Input (Input_)
import Deku.DOM.Elt.Optgroup (Optgroup_)
import Deku.DOM.Elt.Option (Option_)
import Deku.DOM.Elt.Select (Select_)
import Deku.DOM.Elt.Textarea (Textarea_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Disabled = Disabled

instance Attr Button_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Button_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Fieldset_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Fieldset_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Input_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Input_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Optgroup_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Optgroup_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Option_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Option_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Select_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Select_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr Textarea_ Disabled  String  where
  attr Disabled value = unsafeAttribute $ Left $  
    { key: "disabled", value: prop' value }
instance Attr Textarea_ Disabled (Event.Event  String ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#>
    \value -> { key: "disabled", value: prop' value }


instance Attr everything Disabled  Unit  where
  attr Disabled _ = unsafeAttribute $ Left $  
    { key: "disabled", value: unset' }
instance Attr everything Disabled (Event.Event  Unit ) where
  attr Disabled eventValue = unsafeAttribute $ Right $ eventValue <#> \_ ->
    { key: "disabled", value: unset' }

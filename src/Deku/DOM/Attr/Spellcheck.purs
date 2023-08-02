module Deku.DOM.Attr.Spellcheck where

import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.A (A_)
import Deku.DOM.Elt.Abbr (Abbr_)
import Deku.DOM.Elt.Acronym (Acronym_)
import Deku.DOM.Elt.Address (Address_)
import Deku.DOM.Elt.Applet (Applet_)
import Deku.DOM.Elt.Area (Area_)
import Deku.DOM.Elt.Article (Article_)
import Deku.DOM.Elt.Aside (Aside_)
import Deku.DOM.Elt.Audio (Audio_)
import Deku.DOM.Elt.B (B_)
import Deku.DOM.Elt.Base (Base_)
import Deku.DOM.Elt.Basefont (Basefont_)
import Deku.DOM.Elt.Bdi (Bdi_)
import Deku.DOM.Elt.Bdo (Bdo_)
import Deku.DOM.Elt.Big (Big_)
import Deku.DOM.Elt.Blockquote (Blockquote_)
import Deku.DOM.Elt.Body (Body_)
import Deku.DOM.Elt.Br (Br_)
import Deku.DOM.Elt.Button (Button_)
import Deku.DOM.Elt.Canvas (Canvas_)
import Deku.DOM.Elt.Caption (Caption_)
import Deku.DOM.Elt.Center (Center_)
import Deku.DOM.Elt.Cite (Cite_)
import Deku.DOM.Elt.Code (Code_)
import Deku.DOM.Elt.Col (Col_)
import Deku.DOM.Elt.Colgroup (Colgroup_)
import Deku.DOM.Elt.Xdata (Xdata_)
import Deku.DOM.Elt.Datalist (Datalist_)
import Deku.DOM.Elt.Dd (Dd_)
import Deku.DOM.Elt.Del (Del_)
import Deku.DOM.Elt.Details (Details_)
import Deku.DOM.Elt.Dfn (Dfn_)
import Deku.DOM.Elt.Dialog (Dialog_)
import Deku.DOM.Elt.Dir (Dir_)
import Deku.DOM.Elt.Div (Div_)
import Deku.DOM.Elt.Dl (Dl_)
import Deku.DOM.Elt.Dt (Dt_)
import Deku.DOM.Elt.Em (Em_)
import Deku.DOM.Elt.Embed (Embed_)
import Deku.DOM.Elt.Fieldset (Fieldset_)
import Deku.DOM.Elt.Figcaption (Figcaption_)
import Deku.DOM.Elt.Figure (Figure_)
import Deku.DOM.Elt.Font (Font_)
import Deku.DOM.Elt.Footer (Footer_)
import Deku.DOM.Elt.Form (Form_)
import Deku.DOM.Elt.Frame (Frame_)
import Deku.DOM.Elt.Frameset (Frameset_)
import Deku.DOM.Elt.H1 (H1_)
import Deku.DOM.Elt.H2 (H2_)
import Deku.DOM.Elt.H3 (H3_)
import Deku.DOM.Elt.H4 (H4_)
import Deku.DOM.Elt.H5 (H5_)
import Deku.DOM.Elt.H6 (H6_)
import Deku.DOM.Elt.Head (Head_)
import Deku.DOM.Elt.Header (Header_)
import Deku.DOM.Elt.Hr (Hr_)
import Deku.DOM.Elt.Html (Html_)
import Deku.DOM.Elt.I (I_)
import Deku.DOM.Elt.Iframe (Iframe_)
import Deku.DOM.Elt.Img (Img_)
import Deku.DOM.Elt.Input (Input_)
import Deku.DOM.Elt.Ins (Ins_)
import Deku.DOM.Elt.Kbd (Kbd_)
import Deku.DOM.Elt.Label (Label_)
import Deku.DOM.Elt.Legend (Legend_)
import Deku.DOM.Elt.Li (Li_)
import Deku.DOM.Elt.Link (Link_)
import Deku.DOM.Elt.Main (Main_)
import Deku.DOM.Elt.Map (Map_)
import Deku.DOM.Elt.Mark (Mark_)
import Deku.DOM.Elt.Meta (Meta_)
import Deku.DOM.Elt.Meter (Meter_)
import Deku.DOM.Elt.Nav (Nav_)
import Deku.DOM.Elt.Noframes (Noframes_)
import Deku.DOM.Elt.Noscript (Noscript_)
import Deku.DOM.Elt.Object (Object_)
import Deku.DOM.Elt.Ol (Ol_)
import Deku.DOM.Elt.Optgroup (Optgroup_)
import Deku.DOM.Elt.Option (Option_)
import Deku.DOM.Elt.Output (Output_)
import Deku.DOM.Elt.P (P_)
import Deku.DOM.Elt.Param (Param_)
import Deku.DOM.Elt.Picture (Picture_)
import Deku.DOM.Elt.Pre (Pre_)
import Deku.DOM.Elt.Progress (Progress_)
import Deku.DOM.Elt.Q (Q_)
import Deku.DOM.Elt.Rp (Rp_)
import Deku.DOM.Elt.Rt (Rt_)
import Deku.DOM.Elt.Ruby (Ruby_)
import Deku.DOM.Elt.S (S_)
import Deku.DOM.Elt.Samp (Samp_)
import Deku.DOM.Elt.Script (Script_)
import Deku.DOM.Elt.Section (Section_)
import Deku.DOM.Elt.Select (Select_)
import Deku.DOM.Elt.Small (Small_)
import Deku.DOM.Elt.Source (Source_)
import Deku.DOM.Elt.Span (Span_)
import Deku.DOM.Elt.Strike (Strike_)
import Deku.DOM.Elt.Strong (Strong_)
import Deku.DOM.Elt.Style (Style_)
import Deku.DOM.Elt.Sub (Sub_)
import Deku.DOM.Elt.Summary (Summary_)
import Deku.DOM.Elt.Sup (Sup_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Table (Table_)
import Deku.DOM.Elt.Tbody (Tbody_)
import Deku.DOM.Elt.Td (Td_)
import Deku.DOM.Elt.Template (Template_)
import Deku.DOM.Elt.Textarea (Textarea_)
import Deku.DOM.Elt.Tfoot (Tfoot_)
import Deku.DOM.Elt.Th (Th_)
import Deku.DOM.Elt.Thead (Thead_)
import Deku.DOM.Elt.Time (Time_)
import Deku.DOM.Elt.Title (Title_)
import Deku.DOM.Elt.Tr (Tr_)
import Deku.DOM.Elt.Track (Track_)
import Deku.DOM.Elt.Tt (Tt_)
import Deku.DOM.Elt.U (U_)
import Deku.DOM.Elt.Ul (Ul_)
import Deku.DOM.Elt.Var (Var_)
import Deku.DOM.Elt.Video (Video_)
import Deku.DOM.Elt.Wbr (Wbr_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Spellcheck = Spellcheck

instance Attr A_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr A_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr A_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Abbr_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Abbr_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Abbr_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Acronym_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Acronym_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Acronym_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Address_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Address_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Address_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Applet_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Applet_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Applet_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Area_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Area_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Area_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Article_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Article_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Article_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Aside_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Aside_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Aside_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Audio_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Audio_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Audio_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr B_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr B_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr B_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Base_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Base_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Base_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Basefont_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Basefont_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Basefont_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Bdi_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Bdi_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Bdi_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Bdo_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Bdo_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Bdo_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Big_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Big_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Big_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Blockquote_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Blockquote_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Blockquote_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Body_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Body_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Body_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Br_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Br_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Br_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Button_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Button_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Button_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Canvas_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Canvas_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Canvas_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Caption_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Caption_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Caption_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Center_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Center_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Center_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Cite_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Cite_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Cite_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Code_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Code_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Code_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Col_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Col_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Col_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Colgroup_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Colgroup_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Colgroup_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Xdata_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Xdata_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Xdata_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Datalist_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Datalist_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Datalist_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dd_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dd_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dd_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Del_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Del_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Del_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Details_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Details_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Details_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dfn_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dfn_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dfn_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dialog_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dialog_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dialog_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dir_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dir_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dir_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Div_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Div_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Div_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dl_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dl_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dl_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Dt_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Dt_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Dt_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Em_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Em_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Em_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Embed_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Embed_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Embed_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Fieldset_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Fieldset_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Fieldset_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Figcaption_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Figcaption_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Figcaption_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Figure_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Figure_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Figure_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Font_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Font_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Font_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Footer_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Footer_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Footer_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Form_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Form_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Form_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Frame_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Frame_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Frame_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Frameset_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Frameset_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Frameset_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H1_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H1_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H1_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H2_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H2_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H2_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H3_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H3_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H3_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H4_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H4_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H4_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H5_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H5_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H5_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr H6_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr H6_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr H6_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Head_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Head_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Head_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Header_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Header_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Header_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Hr_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Hr_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Hr_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Html_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Html_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Html_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr I_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr I_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr I_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Iframe_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Iframe_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Iframe_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Img_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Img_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Img_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Input_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Input_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Input_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Ins_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Ins_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Ins_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Kbd_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Kbd_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Kbd_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Label_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Label_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Label_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Legend_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Legend_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Legend_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Li_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Li_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Li_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Link_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Link_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Link_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Main_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Main_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Main_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Map_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Map_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Map_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Mark_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Mark_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Mark_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Meta_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Meta_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Meta_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Meter_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Meter_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Meter_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Nav_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Nav_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Nav_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Noframes_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Noframes_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Noframes_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Noscript_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Noscript_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Noscript_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Object_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Object_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Object_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Ol_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Ol_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Ol_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Optgroup_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Optgroup_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Optgroup_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Option_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Option_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Option_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Output_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Output_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Output_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr P_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr P_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr P_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Param_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Param_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Param_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Picture_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Picture_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Picture_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Pre_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Pre_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Pre_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Progress_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Progress_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Progress_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Q_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Q_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Q_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Rp_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Rp_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Rp_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Rt_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Rt_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Rt_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Ruby_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Ruby_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Ruby_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr S_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr S_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr S_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Samp_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Samp_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Samp_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Script_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Script_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Script_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Section_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Section_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Section_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Select_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Select_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Select_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Small_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Small_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Small_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Source_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Source_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Source_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Span_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Span_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Span_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Strike_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Strike_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Strike_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Strong_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Strong_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Strong_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Style_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Style_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Style_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Sub_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Sub_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Sub_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Summary_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Summary_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Summary_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Sup_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Sup_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Sup_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Svg_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Svg_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Svg_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Table_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Table_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Table_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Tbody_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Tbody_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Tbody_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Td_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Td_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Td_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Template_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Template_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Template_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Textarea_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Textarea_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Textarea_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Tfoot_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Tfoot_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Tfoot_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Th_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Th_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Th_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Thead_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Thead_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Thead_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Time_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Time_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Time_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Title_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Title_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Title_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Tr_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Tr_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Tr_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Track_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Track_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Track_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Tt_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Tt_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Tt_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr U_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr U_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr U_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Ul_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Ul_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Ul_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Var_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Var_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Var_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Video_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Video_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Video_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr Wbr_ Spellcheck (NonEmpty.NonEmpty Event.Event  String ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: prop' (NonEmpty.head bothValues) }
    (NonEmpty.tail bothValues <#> \value -> { key: "spellcheck", value: prop' value })
instance Attr Wbr_ Spellcheck  String  where
  attr Spellcheck value = unsafeAttribute $ This
    { key: "spellcheck", value: prop' value }
instance Attr Wbr_ Spellcheck (Event.Event  String ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \value -> { key: "spellcheck", value: prop' value }

instance Attr everything Spellcheck (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Spellcheck bothValues = unsafeAttribute $ Both
    { key: "spellcheck", value: unset' }
    (NonEmpty.tail bothValues <#> \_ -> { key: "spellcheck", value: unset' })
instance Attr everything Spellcheck  Unit  where
  attr Spellcheck _ = unsafeAttribute $ This
    { key: "spellcheck", value: unset' }
instance Attr everything Spellcheck (Event.Event  Unit ) where
  attr Spellcheck eventValue = unsafeAttribute $ That $ eventValue <#>
    \_ -> { key: "spellcheck", value: unset' }

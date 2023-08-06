module Deku.DOM.Attr.Id where

import Data.Tuple as Tuple
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Global
import Data.Functor.Product as Product
import Prelude
import Data.These (These(..))
import FRP.Event as Event
import Data.NonEmpty as NonEmpty

import Deku.DOM.Elt.View (View_)
import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.Title (Title_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Switch (Switch_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Stop (Stop_)
import Deku.DOM.Elt.Set (Set_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.RadialGradient (RadialGradient_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Pattern (Pattern_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Mpath (Mpath_)
import Deku.DOM.Elt.Metadata (Metadata_)
import Deku.DOM.Elt.Mask (Mask_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.LinearGradient (LinearGradient_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.Image (Image_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Filter (Filter_)
import Deku.DOM.Elt.FeTurbulence (FeTurbulence_)
import Deku.DOM.Elt.FeTile (FeTile_)
import Deku.DOM.Elt.FeSpotLight (FeSpotLight_)
import Deku.DOM.Elt.FeSpecularLighting (FeSpecularLighting_)
import Deku.DOM.Elt.FePointLight (FePointLight_)
import Deku.DOM.Elt.FeOffset (FeOffset_)
import Deku.DOM.Elt.FeMorphology (FeMorphology_)
import Deku.DOM.Elt.FeMergeNode (FeMergeNode_)
import Deku.DOM.Elt.FeMerge (FeMerge_)
import Deku.DOM.Elt.FeImage (FeImage_)
import Deku.DOM.Elt.FeGaussianBlur (FeGaussianBlur_)
import Deku.DOM.Elt.FeFuncR (FeFuncR_)
import Deku.DOM.Elt.FeFuncG (FeFuncG_)
import Deku.DOM.Elt.FeFuncB (FeFuncB_)
import Deku.DOM.Elt.FeFuncA (FeFuncA_)
import Deku.DOM.Elt.FeFlood (FeFlood_)
import Deku.DOM.Elt.FeDropShadow (FeDropShadow_)
import Deku.DOM.Elt.FeDistantLight (FeDistantLight_)
import Deku.DOM.Elt.FeDisplacementMap (FeDisplacementMap_)
import Deku.DOM.Elt.FeDiffuseLighting (FeDiffuseLighting_)
import Deku.DOM.Elt.FeConvolveMatrix (FeConvolveMatrix_)
import Deku.DOM.Elt.FeComposite (FeComposite_)
import Deku.DOM.Elt.FeComponentTransfer (FeComponentTransfer_)
import Deku.DOM.Elt.FeColorMatrix (FeColorMatrix_)
import Deku.DOM.Elt.FeBlend (FeBlend_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Discard (Discard_)
import Deku.DOM.Elt.Desc (Desc_)
import Deku.DOM.Elt.Defs (Defs_)
import Deku.DOM.Elt.ClipPath (ClipPath_)
import Deku.DOM.Elt.Circle (Circle_)
import Deku.DOM.Elt.AnimateTransform (AnimateTransform_)
import Deku.DOM.Elt.AnimateMotion (AnimateMotion_)
import Deku.DOM.Elt.Animate (Animate_)
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
import Deku.DOM.Elt.Table (Table_)
import Deku.DOM.Elt.Tbody (Tbody_)
import Deku.DOM.Elt.Td (Td_)
import Deku.DOM.Elt.Template (Template_)
import Deku.DOM.Elt.Textarea (Textarea_)
import Deku.DOM.Elt.Tfoot (Tfoot_)
import Deku.DOM.Elt.Th (Th_)
import Deku.DOM.Elt.Thead (Thead_)
import Deku.DOM.Elt.Time (Time_)
import Deku.DOM.Elt.Tr (Tr_)
import Deku.DOM.Elt.Track (Track_)
import Deku.DOM.Elt.Tt (Tt_)
import Deku.DOM.Elt.U (U_)
import Deku.DOM.Elt.Ul (Ul_)
import Deku.DOM.Elt.Var (Var_)
import Deku.DOM.Elt.Video (Video_)
import Deku.DOM.Elt.Wbr (Wbr_)
import Deku.Attribute (class Attr, prop', unsafeAttribute, unset')

data Id = Id

instance Attr A_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr A_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr A_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr A_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr A_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Abbr_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Abbr_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Abbr_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Abbr_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Abbr_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Acronym_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Acronym_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Acronym_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Acronym_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Acronym_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Address_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Address_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Address_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Address_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Address_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Applet_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Applet_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Applet_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Applet_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Applet_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Area_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Area_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Area_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Area_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Area_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Article_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Article_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Article_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Article_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Article_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Aside_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Aside_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Aside_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Aside_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Aside_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Audio_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Audio_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Audio_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Audio_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Audio_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr B_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr B_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr B_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr B_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr B_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Base_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Base_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Base_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Base_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Base_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Basefont_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Basefont_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Basefont_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Basefont_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Basefont_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Bdi_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Bdi_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Bdi_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Bdi_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Bdi_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Bdo_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Bdo_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Bdo_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Bdo_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Bdo_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Big_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Big_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Big_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Big_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Big_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Blockquote_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Blockquote_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Blockquote_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Blockquote_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Blockquote_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Body_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Body_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Body_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Body_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Body_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Br_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Br_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Br_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Br_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Br_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Button_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Button_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Button_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Button_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Button_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Canvas_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Canvas_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Canvas_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Canvas_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Canvas_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Caption_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Caption_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Caption_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Caption_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Caption_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Center_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Center_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Center_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Center_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Center_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Cite_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Cite_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Cite_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Cite_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Cite_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Code_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Code_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Code_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Code_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Code_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Col_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Col_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Col_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Col_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Col_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Colgroup_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Colgroup_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Colgroup_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Colgroup_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Colgroup_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Xdata_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Xdata_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Xdata_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Xdata_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Xdata_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Datalist_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Datalist_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Datalist_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Datalist_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Datalist_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dd_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dd_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dd_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dd_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dd_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Del_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Del_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Del_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Del_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Del_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Details_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Details_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Details_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Details_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Details_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dfn_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dfn_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dfn_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dfn_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dfn_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dialog_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dialog_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dialog_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dialog_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dialog_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dir_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dir_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dir_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dir_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dir_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Div_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Div_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Div_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Div_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Div_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dl_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dl_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dl_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dl_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dl_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Dt_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dt_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Dt_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Dt_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Dt_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Em_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Em_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Em_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Em_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Em_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Embed_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Embed_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Embed_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Embed_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Embed_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Fieldset_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Fieldset_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Fieldset_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Fieldset_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Fieldset_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Figcaption_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Figcaption_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Figcaption_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Figcaption_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Figcaption_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Figure_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Figure_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Figure_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Figure_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Figure_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Font_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Font_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Font_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Font_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Font_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Footer_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Footer_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Footer_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Footer_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Footer_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Form_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Form_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Form_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Form_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Form_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Frame_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Frame_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Frame_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Frame_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Frame_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Frameset_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Frameset_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Frameset_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Frameset_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Frameset_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H1_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H1_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H1_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H1_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H1_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H2_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H2_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H2_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H2_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H2_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H3_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H3_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H3_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H3_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H3_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H4_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H4_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H4_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H4_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H4_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H5_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H5_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H5_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H5_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H5_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr H6_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H6_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr H6_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr H6_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr H6_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Head_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Head_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Head_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Head_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Head_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Header_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Header_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Header_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Header_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Header_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Hr_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Hr_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Hr_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Hr_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Hr_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Html_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Html_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Html_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Html_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Html_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr I_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr I_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr I_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr I_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr I_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Iframe_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Iframe_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Iframe_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Iframe_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Iframe_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Img_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Img_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Img_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Img_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Img_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Input_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Input_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Input_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Input_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Input_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Ins_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ins_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ins_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Ins_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Ins_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Kbd_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Kbd_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Kbd_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Kbd_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Kbd_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Label_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Label_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Label_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Label_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Label_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Legend_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Legend_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Legend_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Legend_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Legend_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Li_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Li_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Li_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Li_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Li_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Link_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Link_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Link_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Link_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Link_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Main_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Main_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Main_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Main_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Main_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Map_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Map_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Map_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Map_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Map_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Mark_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mark_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mark_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Mark_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Mark_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Meta_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Meta_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Meta_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Meta_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Meta_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Meter_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Meter_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Meter_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Meter_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Meter_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Nav_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Nav_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Nav_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Nav_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Nav_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Noframes_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Noframes_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Noframes_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Noframes_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Noframes_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Noscript_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Noscript_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Noscript_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Noscript_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Noscript_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Object_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Object_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Object_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Object_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Object_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Ol_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ol_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ol_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Ol_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Ol_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Optgroup_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Optgroup_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Optgroup_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Optgroup_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Optgroup_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Option_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Option_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Option_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Option_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Option_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Output_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Output_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Output_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Output_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Output_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr P_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr P_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr P_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr P_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr P_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Param_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Param_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Param_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Param_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Param_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Picture_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Picture_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Picture_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Picture_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Picture_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Pre_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Pre_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Pre_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Pre_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Pre_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Progress_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Progress_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Progress_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Progress_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Progress_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Q_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Q_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Q_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Q_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Q_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Rp_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rp_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rp_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Rp_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Rp_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Rt_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rt_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rt_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Rt_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Rt_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Ruby_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ruby_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ruby_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Ruby_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Ruby_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr S_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr S_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr S_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr S_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr S_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Samp_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Samp_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Samp_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Samp_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Samp_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Script_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Script_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Script_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Script_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Script_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Section_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Section_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Section_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Section_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Section_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Select_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Select_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Select_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Select_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Select_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Small_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Small_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Small_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Small_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Small_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Source_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Source_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Source_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Source_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Source_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Span_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Span_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Span_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Span_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Span_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Strike_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Strike_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Strike_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Strike_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Strike_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Strong_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Strong_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Strong_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Strong_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Strong_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Style_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Style_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Style_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Style_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Style_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Sub_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Sub_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Sub_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Sub_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Sub_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Summary_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Summary_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Summary_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Summary_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Summary_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Sup_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Sup_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Sup_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Sup_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Sup_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Svg_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Svg_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Svg_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Svg_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Svg_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Table_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Table_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Table_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Table_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Table_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Tbody_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tbody_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tbody_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Tbody_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Tbody_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Td_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Td_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Td_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Td_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Td_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Template_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Template_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Template_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Template_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Template_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Textarea_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Textarea_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Textarea_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Textarea_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Textarea_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Tfoot_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tfoot_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tfoot_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Tfoot_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Tfoot_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Th_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Th_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Th_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Th_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Th_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Thead_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Thead_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Thead_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Thead_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Thead_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Time_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Time_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Time_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Time_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Time_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Title_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Title_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Title_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Title_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Title_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Tr_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tr_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tr_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Tr_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Tr_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Track_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Track_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Track_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Track_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Track_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Tt_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tt_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tt_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Tt_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Tt_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr U_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr U_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr U_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr U_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr U_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Ul_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ul_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ul_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Ul_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Ul_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Var_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Var_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Var_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Var_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Var_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Video_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Video_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Video_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Video_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Video_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Wbr_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Wbr_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Wbr_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Wbr_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Wbr_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Animate_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Animate_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Animate_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Animate_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Animate_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr AnimateMotion_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr AnimateMotion_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr AnimateMotion_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr AnimateMotion_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr AnimateMotion_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr AnimateTransform_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr AnimateTransform_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr AnimateTransform_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr AnimateTransform_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr AnimateTransform_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Circle_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Circle_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Circle_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Circle_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Circle_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr ClipPath_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr ClipPath_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr ClipPath_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr ClipPath_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr ClipPath_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Defs_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Defs_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Defs_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Defs_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Defs_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Desc_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Desc_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Desc_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Desc_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Desc_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Discard_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Discard_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Discard_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Discard_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Discard_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Ellipse_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ellipse_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Ellipse_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Ellipse_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Ellipse_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeBlend_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeBlend_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeBlend_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeBlend_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeBlend_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeColorMatrix_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeColorMatrix_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeColorMatrix_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeColorMatrix_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeColorMatrix_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeComponentTransfer_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeComponentTransfer_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeComponentTransfer_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeComponentTransfer_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeComponentTransfer_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeComposite_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeComposite_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeComposite_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeComposite_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeComposite_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeConvolveMatrix_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeConvolveMatrix_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeConvolveMatrix_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeConvolveMatrix_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeConvolveMatrix_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeDiffuseLighting_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDiffuseLighting_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDiffuseLighting_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeDiffuseLighting_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeDiffuseLighting_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeDisplacementMap_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDisplacementMap_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDisplacementMap_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeDisplacementMap_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeDisplacementMap_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeDistantLight_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDistantLight_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDistantLight_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeDistantLight_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeDistantLight_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeDropShadow_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDropShadow_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeDropShadow_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeDropShadow_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeDropShadow_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeFlood_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFlood_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFlood_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeFlood_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeFlood_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncA_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncA_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncA_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeFuncA_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncA_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncB_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncB_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncB_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeFuncB_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncB_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncG_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncG_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncG_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeFuncG_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncG_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncR_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncR_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeFuncR_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeFuncR_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeFuncR_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeGaussianBlur_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeGaussianBlur_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeGaussianBlur_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeGaussianBlur_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeGaussianBlur_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeImage_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeImage_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeImage_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeImage_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeImage_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeMerge_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMerge_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMerge_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeMerge_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeMerge_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeMergeNode_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMergeNode_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMergeNode_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeMergeNode_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeMergeNode_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeMorphology_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMorphology_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeMorphology_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeMorphology_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeMorphology_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeOffset_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeOffset_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeOffset_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeOffset_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeOffset_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FePointLight_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FePointLight_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FePointLight_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FePointLight_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FePointLight_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeSpecularLighting_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeSpecularLighting_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeSpecularLighting_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeSpecularLighting_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeSpecularLighting_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeSpotLight_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeSpotLight_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeSpotLight_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeSpotLight_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeSpotLight_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeTile_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeTile_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeTile_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeTile_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeTile_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr FeTurbulence_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeTurbulence_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr FeTurbulence_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr FeTurbulence_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr FeTurbulence_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Filter_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Filter_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Filter_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Filter_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Filter_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr ForeignObject_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr ForeignObject_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr ForeignObject_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr ForeignObject_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr ForeignObject_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr G_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr G_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr G_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr G_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr G_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Image_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Image_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Image_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Image_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Image_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Line_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Line_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Line_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Line_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Line_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr LinearGradient_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr LinearGradient_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr LinearGradient_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr LinearGradient_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr LinearGradient_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Marker_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Marker_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Marker_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Marker_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Marker_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Mask_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mask_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mask_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Mask_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Mask_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Metadata_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Metadata_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Metadata_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Metadata_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Metadata_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Mpath_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mpath_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Mpath_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Mpath_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Mpath_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Path_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Path_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Path_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Path_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Path_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Pattern_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Pattern_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Pattern_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Pattern_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Pattern_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Polygon_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Polygon_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Polygon_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Polygon_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Polygon_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Polyline_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Polyline_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Polyline_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Polyline_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Polyline_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr RadialGradient_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr RadialGradient_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr RadialGradient_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr RadialGradient_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr RadialGradient_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Rect_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rect_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Rect_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Rect_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Rect_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Set_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Set_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Set_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Set_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Set_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Stop_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Stop_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Stop_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Stop_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Stop_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Switch_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Switch_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Switch_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Switch_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Switch_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Symbol_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Symbol_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Symbol_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Symbol_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Symbol_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Text_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Text_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Text_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Text_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Text_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr TextPath_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr TextPath_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr TextPath_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr TextPath_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr TextPath_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Tspan_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tspan_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Tspan_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Tspan_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Tspan_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr Use_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Use_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr Use_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr Use_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr Use_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr View_ Id (NonEmpty.NonEmpty Event.Event  String ) where
  attr Id bothValues = unsafeAttribute $ Both (pure 
    { key: "id", value: prop' (NonEmpty.head bothValues) })
    (NonEmpty.tail bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr View_ Id (Product.Product (ST.ST Global.Global) Event.Event  String ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \value ->  
    { key: "id", value: prop' (value) })
    (Tuple.snd bothValues <#> \value -> { key: "id", value: prop' value })
instance Attr View_ Id  String  where
  attr Id value = unsafeAttribute $ This $ { key: "id", value: prop' value }
instance Attr View_ Id (Event.Event  String ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \value ->
    { key: "id", value: prop' value }

instance Attr View_ Id (ST.ST Global.Global  String ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \value ->
    { key: "id", value: prop' value }

instance Attr everything Id (NonEmpty.NonEmpty Event.Event  Unit ) where
  attr Id bothValues = unsafeAttribute $ Both (pure  { key: "id", value: unset' })
    (NonEmpty.tail bothValues <#> \_ -> { key: "id", value: unset' })
instance Attr everything Id (Product.Product (ST.ST Global.Global) Event.Event  Unit ) where
  attr Id (Product.Product bothValues) = unsafeAttribute $ Both (Tuple.fst bothValues # \_ ->   { key: "id", value: unset' })
    (Tuple.snd bothValues <#> \_ -> { key: "id", value: unset' })
instance Attr everything Id  Unit  where
  attr Id _ = unsafeAttribute $ This $ { key: "id", value: unset' }
instance Attr everything Id (Event.Event  Unit ) where
  attr Id eventValue = unsafeAttribute $ That $ eventValue <#> \_ ->
    { key: "id", value: unset' }

instance Attr everything Id (ST.ST Global.Global  Unit ) where
  attr Id iValue = unsafeAttribute $ This $ iValue # \_ ->
    { key: "id", value: unset' }

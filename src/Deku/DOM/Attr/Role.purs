module Deku.DOM.Attr.Role where

import Prelude
import Data.These (These(..))
import Data.Tuple (fst, snd)

import Deku.DOM.Elt.View (View_)
import Deku.DOM.Elt.Use (Use_)
import Deku.DOM.Elt.Tspan (Tspan_)
import Deku.DOM.Elt.TextPath (TextPath_)
import Deku.DOM.Elt.Text (Text_)
import Deku.DOM.Elt.Symbol (Symbol_)
import Deku.DOM.Elt.Svg (Svg_)
import Deku.DOM.Elt.Rect (Rect_)
import Deku.DOM.Elt.Polyline (Polyline_)
import Deku.DOM.Elt.Polygon (Polygon_)
import Deku.DOM.Elt.Path (Path_)
import Deku.DOM.Elt.Marker (Marker_)
import Deku.DOM.Elt.Line (Line_)
import Deku.DOM.Elt.G (G_)
import Deku.DOM.Elt.ForeignObject (ForeignObject_)
import Deku.DOM.Elt.Ellipse (Ellipse_)
import Deku.DOM.Elt.Circle (Circle_)
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

data Role = Role

instance Attr A_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Abbr_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Acronym_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Address_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Applet_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Area_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Article_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Aside_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Audio_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr B_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Base_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Basefont_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Bdi_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Bdo_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Big_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Blockquote_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Body_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Br_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Button_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Canvas_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Caption_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Center_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Cite_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Code_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Col_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Colgroup_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Xdata_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Datalist_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dd_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Del_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Details_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dfn_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dialog_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dir_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Div_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dl_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Dt_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Em_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Embed_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Fieldset_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Figcaption_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Figure_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Font_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Footer_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Form_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Frame_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Frameset_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H1_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H2_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H3_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H4_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H5_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr H6_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Head_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Header_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Hr_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Html_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr I_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Iframe_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Img_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Input_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Ins_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Kbd_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Label_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Legend_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Li_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Link_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Main_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Map_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Mark_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Meta_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Meter_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Nav_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Noframes_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Noscript_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Object_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Ol_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Optgroup_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Option_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Output_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr P_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Param_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Picture_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Pre_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Progress_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Q_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Rp_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Rt_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Ruby_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr S_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Samp_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Script_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Section_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Select_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Small_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Source_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Span_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Strike_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Strong_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Style_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Sub_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Summary_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Sup_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Svg_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Table_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Tbody_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Td_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Template_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Textarea_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Tfoot_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Th_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Thead_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Time_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Title_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Tr_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Track_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Tt_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr U_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Ul_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Var_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Video_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Wbr_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Circle_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Ellipse_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr ForeignObject_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr G_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Line_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Marker_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Path_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Polygon_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Polyline_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Rect_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Symbol_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Text_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr TextPath_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Tspan_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr Use_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr View_ Role String where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  prop' (fst bothValues)  } (snd bothValues <#> \value -> { key: "role", value:  prop' value  })
  pureAttr Role value  = unsafeAttribute $ This { key: "role", value:  prop' value  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \value -> { key: "role", value:  prop' value  }

instance Attr everything Role Unit where
  attr Role bothValues  = unsafeAttribute $ Both { key: "role", value:  unset'  } (snd bothValues <#> \_ -> { key: "role", value:  unset'  })
  pureAttr Role _  = unsafeAttribute $ This { key: "role", value:  unset'  }
  unpureAttr Role eventValue  = unsafeAttribute $ That $ eventValue <#> \_ -> { key: "role", value:  unset'  }

module Deku.Example.Docs.Portals1 where

import Prelude

import Control.Alt (alt)
import Data.Foldable (oneOfMap)
import Data.Profunctor (lcmap)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((+>), index)
import Data.Vec as V
import Deku.Attribute (cb, (:=))
import Deku.Control (blank, plant, portal, switcher, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Example.Docs.Types (Page)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Event (bang, bus, fold)
import Type.Proxy (Proxy(..))

data MainUIAction
  = UIShown
  | AddTodo
  | ChangeText String

data TodoAction = Prioritize | Delete

px =
  Proxy    :: Proxy      """<div>
  <h1>Portals</h1>

  <h2>Zapping from place to place</h2>
  <p>
    Sometimes, we have referentially opaque nodes like videos or audio that we can't destroy and recreate when they need to move around the DOM. In these cases, we can use portals to "zap" nodes from one part of the DOM to another.
  </p>

  <p>
    The code below uses portals to flip two videos across a boundary. If you press play on either video, you'll see that it keeps playing uninterrupted, event when it is removed and reinserted.
  </p>

  ~code~

  <p>And here's what it produces:</p>

  <blockquote> ~result~ </blockquote>

  <h2>Events of portals</h2>

  <p>Portals work by sending a referentially-opaque reference to a DOM element into a closure where it can be safely used. Whenever an event is emitted containing the element, the element will be rendered at that place in the DOM, as in the example above. Note that portals "hog the blanket", meaning that they end up in the last place in which they were called.</p>

  <p>Switcher is a small function (a few lines) included in Deku to switch between different elements. In general, I've tried not to write too many util functions, but switching between things (tabs, pages) is such a common pattern that I've included it in the library. You can feel free to roll your own switcher, though, if you choose!</p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out Deku! I had a blast writing it, and I hope you enjoy using it for your projects!</p>
</div>"""



portals1 :: forall lock payload. (Page -> Effect Unit) -> Element lock payload
portals1 _ = px ~~
  { code: nut
      ( D.pre_
          [ D.code_
              [ text_
                  """module Main where

import Prelude

import Control.Alt (alt)
import Data.Foldable (oneOfMap)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (d0, d1)
import Data.Vec (index, (+>))
import Data.Vec as V
import Deku.Attribute (cb, (:=))
import Deku.Control (blank, plant, portal, switcher, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Event (Event, bang, bus, fold, mapAccum)

counter :: forall a. Event a → Event (a /\ Int)
counter event = mapAccum f event 0
  where
  f a b = (b + 1) /\ (a /\ b)

main :: Effect Unit
main = runInBody1
  ( bus \push -> lcmap (alt (bang unit)) \event -> do
      portal
        ( map
            ( \i -> D.video
                (oneOfMap bang [ D.Controls := "true", D.Width := "250" ])
                ( D.source
                    (oneOfMap bang [ D.Src := i, D.Xtype := "video/mp4" ])
                    blank
                )
            )
            ( "https://interactive-examples.mdn.mozilla.net/media/cc0-videos/flower.mp4"
                +> "https://www.w3schools.com/jsref/movie.mp4"
                +> V.empty
            )
        )
        \v _ -> do
          let
            p0 = index v d0
            p1 = index v d1
            ev = fold (const not) event
            flips = switcher (if _ then p0 else p1) <<< ev
          plant $ D.div_
            [ D.button (bang $ D.OnClick := cb (const $ push unit))
                [ text_ "Switch videos" ]
            , D.div_ [ D.span_ (flips true), D.span_ (flips false) ]
            ]
  )
"""
              ]
          ]
      )
  , result: nut

   ( bus \push -> lcmap (alt (bang unit)) \event -> do
      portal
        ( map
            ( \i -> D.video
                (oneOfMap bang [ D.Controls := "true", D.Width := "250" ])
                ( D.source
                    (oneOfMap bang [ D.Src := i, D.Xtype := "video/mp4" ])
                    blank
                )
            )
            ( "https://interactive-examples.mdn.mozilla.net/media/cc0-videos/flower.mp4"
                +> "https://www.w3schools.com/jsref/movie.mp4"
                +> V.empty
            )
        )
        \v _ -> do
          let
            p0 = index v d0
            p1 = index v d1
            ev = fold (const not) event
            flips = switcher (if _ then p0 else p1) <<< ev
          plant $ D.div_
            [ D.button (bang $ D.OnClick := cb (const $ push unit))
                [ text_ "Switch videos" ]
            , D.div_ [ D.span_ (flips true), D.span_ (flips false) ]
            ]
  )
  }
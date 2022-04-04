module Deku.Subgraph where

import Prelude

import Control.Alt ((<|>))
import Data.Hashable (class Hashable, hash)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Core (AudioInterpret(..), Node(..), Subgraph)
import Deku.Core as C
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)

data SubgraphAction env
  = InsertOrUpdate env
  | SendToTop
  | Remove


-- subgraph
--   :: forall index env push event payload
--    . Hashable index
--   => IsEvent event
--   => event (index /\ SubgraphAction env)
--   -> Subgraph index env push event payload
--   -> Element event payload
-- subgraph mods scenes = Element go
--   where
--   go
--     parent
--     ( DOMInterpret
--         { makeSubgraph
--         , insertOrUpdateSubgraph
--         , sendSubgraphToTop
--         , removeSubgraph
--         , ids
--         }
--     ) =
--     keepLatest $ map
--       ( \id -> pure (makeSubgraph { id, parent, scenes: scenes }) <|>
--           map
--             ( \(index /\ instr) -> case instr of
--                 Remove -> removeSubgraph { id, pos: hash index, index }
--                 SendToTop -> sendSubgraphToTop { id, pos: hash index, index }
--                 InsertOrUpdate env -> insertOrUpdateSubgraph
--                   { id, pos: hash index, index, env }
--             )
--             mods
--       )
--       (sample_ ids (pure unit))

-- delay

subgraph
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => Hashable index
  => event (index /\ SubgraphAction env)
  -> ({|produced} -> Subgraph index env outputChannels event payload)
  -> C.Node outputChannels produced consumed event payload
subgraph i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeDelay, setDelay }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeDelay
                { id: me, parent: parent, delayTime: i.delayTime }
            )
            <|> map
              ( \(C.Delay e) -> match
                  { delayTime: \delayTime -> setDelay
                      { id: me, delayTime }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

subgraph'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeDelay
  -> event C.Delay
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
subgraph' _ i atts elts = let C.Node n = delay i atts elts in C.Node n


infixr 6 subgraph as @@
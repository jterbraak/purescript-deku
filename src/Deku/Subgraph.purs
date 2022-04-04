module Deku.Subgraph where

import Prelude

import Control.Alt ((<|>))
import Data.Hashable (class Hashable, hash)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Core (Input(..), Subgraph)
import Deku.Core as C
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

data SubgraphAction env
  = InsertOrUpdate env
  | Remove

class MakeInputs :: forall k. k -> Row Type -> Constraint
class MakeInputs producedRL produced | producedRL -> produced where
  inputs :: forall proxy. proxy producedRL -> { | produced }

instance inputsNil :: MakeInputs (RL.Nil) () where
  inputs _ = {}

instance inputsCons ::
  ( IsSymbol key
  , Row.Cons key Input produced' produced
  , Row.Lacks key produced'
  , MakeInputs rest produced'
  ) =>
  MakeInputs (RL.Cons key Input rest) produced where
  inputs _ = let px = (Proxy :: _ key) in Record.insert px (Input (reflectSymbol px)) (inputs (Proxy :: _ rest))

subgraph
  :: forall index env outputChannels produced producedRL consumed event payload
   . IsEvent event
  => Hashable index
  => RowToList produced producedRL
  => MakeInputs producedRL produced
  => event (index /\ SubgraphAction env)
  -> ({ | produced } -> Subgraph index env outputChannels event payload)
  -> C.Node outputChannels produced consumed event payload
subgraph mods elt = C.Node go
  where
  subg = elt (inputs (Proxy :: _ producedRL))
  go parent (C.AudioInterpret { ids, makeSubgraph, insertOrUpdateSubgraph, removeSubgraph }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSubgraph
                { id: me, parent: parent, scenes: subg }
            )
            <|> map
              ( \(index /\ instr) -> case instr of
                  Remove -> removeSubgraph { id: me, pos: hash index, index }
                  InsertOrUpdate env -> insertOrUpdateSubgraph
                    { id: me, pos: hash index, index, env }
              )
              mods
      )

subgraph'
  :: forall proxy sym index env outputChannels produced' produced producedRL consumed event payload
   . IsEvent event
  => Hashable index
  => RowToList produced producedRL
  => MakeInputs producedRL produced
  => Row.Cons sym C.Input produced' produced
  => proxy sym
  -> event (index /\ SubgraphAction env)
  -> ({ | produced } -> Subgraph index env outputChannels event payload)
  -> C.Node outputChannels produced consumed event payload
subgraph' _ mods elt = let C.Node n = subgraph mods elt in C.Node n

infixr 6 subgraph as @@
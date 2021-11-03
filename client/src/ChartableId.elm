module ChartableId exposing (ChartableId, decode, encode)

import Json.Decode as D
import Json.Encode as E


type ChartableId
    = ChartableId Int


decode : D.Decoder ChartableId
decode =
    D.map ChartableId D.int


encode : ChartableId -> E.Value
encode (ChartableId id) =
    E.int id

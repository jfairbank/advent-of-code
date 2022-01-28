module Lens exposing (Lens, get, map, new, set)

import Html.Attributes exposing (value)


type alias GetF parent value =
    parent -> value


type alias SetF parent value =
    value -> parent -> parent


type alias MapF parent value =
    (value -> value) -> parent -> parent



-- type Lens parent value
--     = Lens (GetF parent value) (SetF parent value)


type alias Lens parent value =
    { get : GetF parent value
    , set : SetF parent value
    , map : MapF parent value
    }


new : GetF parent value -> SetF parent value -> Lens parent value
new get_ set_ =
    -- Lens
    let
        map_ f parent =
            set_ (f (get_ parent)) parent
    in
    { get = get_
    , set = set_
    , map = map_
    }


get : Lens parent value -> parent -> value



-- get (Lens get_ _) parent =


get lens parent =
    -- get_ parent
    lens.get parent


set : Lens parent value -> value -> parent -> parent



-- set (Lens _ set_) value parent =


set lens value parent =
    -- set_ value parent
    lens.set value parent


map : Lens parent value -> (value -> value) -> parent -> parent



-- map (Lens get_ set_) f parent =


map lens f parent =
    lens.map f parent

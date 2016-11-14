module Random.Extra exposing (..)

import Random exposing (Generator)
import Set


constant : x -> Generator x
constant x =
    Random.bool |> Random.map (\_ -> x)


distinctList : Int -> Generator comparable -> Generator (List comparable)
distinctList size generator =
    let
        isDistinct list =
            list |> Set.fromList |> Set.size |> (==) size
    in
        Random.list size generator
            `Random.andThen`
                (\l ->
                    if isDistinct l then
                        constant l
                    else
                        distinctList size generator
                )

module Random.Extra exposing (..)

import Random exposing (Generator, Seed)
import Set


constant : x -> Generator x
constant x =
    Random.bool |> Random.map (\_ -> x)


distinctList : Seed -> Int -> Generator comparable -> Generator (List comparable)
distinctList seed size generator =
    let
        isDistinct list =
            list |> Set.fromList |> Set.size |> (==) size

        f list =
            isDistinct list
                |> \v ->
                    if v then
                        Just (constant list)
                    else
                        Nothing

        listGenerator =
            Random.list size generator
    in
        loopAnd f seed listGenerator


loopAnd : (a -> Maybe (Generator b)) -> Seed -> Generator a -> Generator b
loopAnd f seed generator =
    let
        ( result, nextSeed ) =
            Random.step generator seed
    in
        case f result of
            Just nextGenerator ->
                nextGenerator

            Nothing ->
                loopAnd f nextSeed generator

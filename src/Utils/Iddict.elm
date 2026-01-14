module Utils.Iddict exposing (..)

import Iddict exposing (Iddict)



{- `dunion d1 d2` builds the disjoint union of the two dictionaries `d1` and `d2`.

   That is, if two keys are conflicting, we keep both entries and generate a fresh key.
-}


dunion : Iddict a -> Iddict a -> Iddict a
dunion d1 d2 =
    let
        maxKeyD1 =
            Iddict.foldl (\k _ acc -> max k acc) -1 d1

        maxKeyD2 =
            Iddict.foldl (\k _ acc -> max k acc) -1 d2

        startKey =
            max maxKeyD1 maxKeyD2 + 1

        insertAt k v dict =
            Iddict.update k (\_ -> Just v) dict

        ( result, _ ) =
            Iddict.merge
                (\k v ( acc, next ) -> ( insertAt k v acc, next ))
                (\k v1 v2 ( acc, next ) ->
                    ( acc
                        |> insertAt k v1
                        |> insertAt next v2
                    , next + 1
                    )
                )
                (\k v ( acc, next ) -> ( insertAt k v acc, next ))
                d1
                d2
                ( Iddict.empty, startKey )
    in
    result

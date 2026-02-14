module Utils.Func exposing (..)


fixpoint : (a -> a -> Bool) -> (a -> a) -> a -> a
fixpoint eq f x =
    let
        res =
            f x
    in
    if eq x res then
        res

    else
        fixpoint eq f res

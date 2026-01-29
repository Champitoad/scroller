module Utils.State exposing (..)

{-| A simple State Monad implementation to handle threading state (like sequential IDs).
-}


type alias State s a =
    s -> ( a, s )


{-| Wrap a value into a State context.
-}
return : a -> State s a
return value =
    \state -> ( value, state )


{-| Map over the value inside the State.
-}
map : (a -> b) -> State s a -> State s b
map func stateFn =
    \state ->
        let
            ( value, newState ) =
                stateFn state
        in
        ( func value, newState )


{-| Chain stateful computations.
-}
andThen : (a -> State s b) -> State s a -> State s b
andThen callback stateFn =
    \state ->
        let
            ( value, newState ) =
                stateFn state
        in
        callback value newState


{-| Flatten a list of stateful computations into a stateful computation of a list.
Preserves order: left-to-right processing.
-}
sequence : List (State s a) -> State s (List a)
sequence list =
    case list of
        [] ->
            return []

        head :: tail ->
            andThen
                (\h ->
                    andThen
                        (\t -> return (h :: t))
                        (sequence tail)
                )
                head


{-| Map a stateful function over a list.
-}
traverse : (a -> State s b) -> List a -> State s (List b)
traverse func list =
    sequence (List.map func list)


{-| Run the state computation starting with an initial state and return the result value.
-}
eval : s -> State s a -> a
eval initialState stateFn =
    Tuple.first (stateFn initialState)


{-| Run the state computation starting with an initial state and return the final state.
-}
exec : s -> State s a -> s
exec initialState stateFn =
    Tuple.second (stateFn initialState)


{-| Get the current state.
-}
get : State s s
get =
    \state -> ( state, state )


{-| Set the current state.
-}
put : s -> State s ()
put newState =
    \_ -> ( (), newState )


{-| Modify the current state.
-}
modify : (s -> s) -> State s ()
modify func =
    \state -> ( (), func state )

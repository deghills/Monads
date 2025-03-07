namespace Monads

module State =
    type State<'S, 'a> = 'S -> ('S * 'a)

    let ret : 'a -> (State<'S, 'a>) = 
        fun x s -> (s, x)

    let bind : ('a -> State<'S, 'b>) -> State<'S, 'a> -> State<'S, 'b> =
        fun f state1 s ->
            let state2, a = state1 s
            let newStateComputation = f a
            newStateComputation state2

    let get : State<'s, 's> =
        fun x -> x, x

    let modify : ('S -> 'S) -> State<'S, unit> =
        fun f s -> (f s, ())

    let put : 'S -> State<'S, unit> =
        fun x _ -> x, ()

    type StateBuilder() =
        member _.Return(x) = ret(x)
        member _.Bind(x, f) = bind f x
        member _.For((sequence:'T seq), (transformationOnItem:'T -> State<'S, unit>)) = 
            let sequence' = sequence |> Seq.map transformationOnItem
            sequence' |> Seq.reduce (fun a b ->
                a |> bind (fun _ -> b)
            )
        member _.Combine((stateAction:State<'S, unit>), (stateValue:State<'S, 'T>)) =
            stateAction |> bind (fun _ -> stateValue)
            
            (*(fun a b s -> 
                let s', value = a s
                let s'', value2 = b s'
                s'', value2
            )*)
        member _.Delay(action:unit -> State<'S, 'T>) =
            action()
        member _.Zero() = fun x -> x,()

    let state = StateBuilder()
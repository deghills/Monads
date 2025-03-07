namespace Monads

module IO =
    type IO<'a> = IO of (unit -> 'a)

    let ret(x) = IO (fun () -> x)

    let bind(f, IO (action)) =
        let x = action()
        match f x with IO (action') ->
        IO action'

    let map f (IO (action)) =
        IO (action >> f)

    let putStrLine (message:string) = IO (fun () -> System.Console.WriteLine message)
    let getLine = IO System.Console.ReadLine

    type IOBuilder() =
        member _.Return(x) = ret(x)

        member _.Bind(x, f) = bind(f, x)

        member _.Zero() = IO (fun () -> ())

    let io = IOBuilder()
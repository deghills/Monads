namespace Monads

module Writer =
    type Writer<'a> = Log of ('a*string seq)

    let ret(x) = Log (x, Seq.empty<string>)

    let bind(f, Log (x, log1)) =
        match f x with Log (y, log2) ->

        Log (y, Seq.append log1 log2)

    let map(f, Log (x, log1)) =
        Log (f x, log1)

    let write (message:string) = Log ( 
        (), seq{message} 
    )

    type WriterBuilder() =
        member _.Return(x) = ret(x)

        member _.Bind(x, f) = bind(f, x)

        member _.Zero() = Log ((), Seq.empty)

    let writer = WriterBuilder()
namespace Monads

module Opt =
    type OptionBuilder() =
        member _.Return(x) = Some(x)
        member _.Bind(x, f) = Option.bind f x

    let option = OptionBuilder()
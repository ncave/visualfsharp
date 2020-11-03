module IdentityMonad

type M<'T> = 'T

module Identity =

    let inline run (expr: M<'T>): 'T =
        expr

    let inline bind (func: 'T -> M<'U>) (expr: M<'T>): M<'U> =
        func expr

    let inline ret (value: 'T): M<'T> =
        value

    let inline delay (func: unit -> M<'T>): M<'T> =
        func ()

    let inline combine (expr1: M<'T>) (expr2: M<'T>): M<'T>  =
        expr1 |> bind (fun _ -> expr2)

    let inline map (mapping: 'T -> 'U) (expr: M<'T>): M<'U>  =
        mapping expr

    let inline fold (folder: 'T -> 'U -> M<'T>) (acc: 'T) (seq: seq<'U>): M<'T>  =
        (ret acc, seq) ||> Seq.fold (fun acc x -> acc |> bind (fun acc -> folder acc x))

    let inline tryFinally (expr: M<'T>) (compensation: unit -> unit): M<'T> =
        try ret (run expr) finally compensation()

    let inline tryWith (expr: M<'T>) (handler: exn -> M<'T>): M<'T> =
        try ret (run expr) with e -> handler e

    let inline using (resource: 'T when 'T :> System.IDisposable) func =
        tryFinally (func resource) resource.Dispose

    let inline zero () =
        ret Unchecked.defaultof<'T>

    let inline whileLoop (pred: unit -> bool) body =
        while pred () do body ()

    let inline forLoop (collection: seq<'T>) func =
        let en = collection.GetEnumerator()
        using en (fun ie -> whileLoop ie.MoveNext (fun () -> func ie.Current))

    type IdentityBuilder() =
        member inline __.Bind(expr, func) = bind func expr
        member inline __.Return(value) = ret value
        member inline __.ReturnFrom(expr) = expr
        member inline __.Delay(func) = delay func
        member inline __.Combine(expr1, expr2) = combine expr1 expr2
        member inline __.For(collection, func) = forLoop collection func
        member inline __.TryWith(expr, handler) = tryWith expr handler
        member inline __.TryFinally(expr, compensation) = tryFinally expr compensation
        member inline __.Using(resource, expr) = using resource expr
        member inline __.While(pred, body) = whileLoop pred body
        member inline __.Zero() = zero ()

let identity = Identity.IdentityBuilder()

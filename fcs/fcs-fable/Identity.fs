module IdentityMonad

type M<'T> = 'T

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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

    let inline using (resource: 'T when 'T :> System.IDisposable) (func: 'T -> M<'U>): M<'U> =
        tryFinally (func resource) (fun () -> resource.Dispose())

    let inline zero (): M<'T> =
        ret Unchecked.defaultof<'T>

    let rec whileLoop (pred: unit -> bool) body =
        if pred() then body |> bind (fun _ -> whileLoop pred body)
        else zero ()

    let inline forLoop (collection: seq<'T>) func =
        let ie = collection.GetEnumerator()
        tryFinally
            (whileLoop
                (fun () -> ie.MoveNext())
                (delay (fun () -> func ie.Current)))
            (fun () -> ie.Dispose())

    type IdentityBuilder() =
        member __.Bind(expr, func) = bind func expr
        member __.Return(value) = ret value
        member __.ReturnFrom(expr) = expr
        member __.Delay(func) = delay func
        member __.Combine(expr1, expr2) = combine expr1 expr2
        member __.For(collection, func) = forLoop collection func
        member __.TryWith(expr, handler) = tryWith expr handler
        member __.TryFinally(expr, compensation) = tryFinally expr compensation
        member __.Using(resource, expr) = using resource expr
        member __.While(pred, body) = whileLoop pred body
        member __.Zero() = zero ()

let identity = Identity.IdentityBuilder()

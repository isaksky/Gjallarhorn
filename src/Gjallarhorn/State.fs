namespace Gjallarhorn

open System
open System.Threading
open Gjallarhorn.Internal

/// A thread-safe wrapper using interlock for a mutable value with change notification
type AtomicMutable<'a when 'a : not struct>(value : 'a) as self =
    let mutable v = value
    let deps = Dependencies.create [||] self
    let swap (f : 'a -> 'a) =
        lock deps <| fun _ ->
            let v2 = f v
            v <- v2
            deps.MarkDirty self
            v2

    let setValue value =
        swap (fun _ -> value) |> ignore

    /// Gets and sets the Value contained within this mutable
    member __.Value 
        with get() = v
        and set(value) = setValue value

    /// Updates the current value in a manner that guarantees proper execution, 
    /// given a function that takes the current value and generates a new value,
    /// and then returns the new value
    /// <remarks>The function may be executed multiple times, depending on the implementation.</remarks>
    member __.Update f = swap f

    interface IAtomicMutatable<'a> with
        member this.Update f = this.Update f
            
    interface System.IDisposable with
        member this.Dispose() =
            deps.RemoveAll this
            GC.SuppressFinalize this
    interface IObservable<'a> with
        member this.Subscribe obs = deps.Subscribe(obs, this)
    interface ITracksDependents with
        member this.Track dep = deps.Add (dep, this)
        member this.Untrack dep = deps.Remove (dep, this)
    interface IDependent with
        member __.UpdateDirtyFlag _ = ()
        member __.HasDependencies with get() = deps.HasDependencies
    interface ISignal<'a> with
        member __.Value with get() = v
    interface IMutatable<'a> with
        member __.Value with get() = v and set(v) = setValue v


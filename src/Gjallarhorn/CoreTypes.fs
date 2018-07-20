namespace Gjallarhorn.Helpers

open Gjallarhorn
open Gjallarhorn.Internal

open System
open System.Runtime.CompilerServices

[<Extension>]
type internal FSharpFuncExtensions = 
    [<Extension>] 
    static member ToFSharpFunc<'a,'b> (func:System.Func<'a,'b>) = fun a -> func.Invoke(a)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c> (func:System.Func<'a,'b,'c>) = fun a b -> func.Invoke(a,b)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d> (func:System.Func<'a,'b,'c,'d>) = fun a b c -> func.Invoke(a,b,c)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e> (func:System.Func<'a,'b,'c,'d,'e>) = fun a b c d -> func.Invoke(a,b,c,d)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f> (func:System.Func<'a,'b,'c,'d,'e,'f>) = fun a b c d e -> func.Invoke(a,b,c,d,e)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g> (func:System.Func<'a,'b,'c,'d,'e,'f,'g>) = fun a b c d e f -> func.Invoke(a,b,c,d,e,f)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h>) = fun a b c d e f g -> func.Invoke(a,b,c,d,e,f,g)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i>) = fun a b c d e f g h -> func.Invoke(a,b,c,d,e,f,g,h)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j>) = fun a b c d e f g h i -> func.Invoke(a,b,c,d,e,f,g,h,i)
    [<Extension>] 
    static member ToFSharpFunc<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k> (func:System.Func<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k>) = fun a b c d e f g h i j -> func.Invoke(a,b,c,d,e,f,g,h,i,j)

/// A disposable type that manages multiple other disposables, and disposes all of them when disposed
type ICompositeDisposable =
    inherit IDisposable

    /// Add an idisposable to this composite disposable
    abstract member Add : IDisposable -> unit
    /// Remove an idisposable to this composite disposable
    abstract member Remove : IDisposable -> unit

/// Type which allows tracking of multiple disposables at once
type CompositeDisposable() =
    let disposables = ResizeArray<_>()

    override this.Finalize() =
        this.Dispose()
        GC.SuppressFinalize this

    /// Add a new disposable to this tracker
    member __.Add (disposable : IDisposable) = disposables.Add(disposable)
    /// Remove a disposable from this tracker without disposing of it
    member __.Remove (disposable : IDisposable) = disposables.Remove(disposable) |> ignore

    /// Dispose all of our tracked disposables and remove them all 
    member __.Dispose() =
        disposables
        |> Seq.iter (fun d -> d.Dispose())
        disposables.Clear()

    interface ICompositeDisposable with
        member this.Add d = this.Add d
        member this.Remove d = this.Remove d 

    interface IDisposable with
        /// Dispose all of our tracked disposables and remove them all 
        member this.Dispose() = this.Dispose()

module internal DisposeHelpers =
    let getValue (provider : ISignal<_> option) typeNameFun =
        match provider with 
        | Some(v) -> v.Value
        | None -> raise <| ObjectDisposedException(typeNameFun())        

    let setValue (provider : IMutatable<_> option) mapping value typeNameFun =
        match provider with 
        | Some(v) -> v.Value <- mapping(value)
        | None -> raise <| ObjectDisposedException(typeNameFun())        

    let disposeIfDisposable (v : obj) =
        match v with
        | :? IDisposable as d -> 
            d.Dispose()
        | _ -> ()
        
    let cleanup (provider : #ISignal<'a> option byref) disposeProviderOnDispose (self : #IDependent) =
            match provider with
            | None -> ()
            | Some(v) ->
                v.Untrack self
                
                if disposeProviderOnDispose then
                    disposeIfDisposable v

                provider <- None

namespace Gjallarhorn.Internal

open Gjallarhorn
open Gjallarhorn.Helpers

open System

/// A lightweight wrapper for a mutable value which provides a mechanism for change notification as needed
type Mutable<'a when 'a : equality>(value : 'a) =
    let mutable v = value

    // Stores dependencies remotely to not use any space in the object (no memory overhead requirements)
    member private this.Dependencies with get() = Dependencies.createRemote this
    
    /// Gets and sets the Value contained within this mutable
    member this.Value 
        with get() = v
        and set(value) =
            if v <> value then            
                v <- value
                this.Dependencies.MarkDirty(this)

    override this.Finalize() =
        this.Dependencies.RemoveAll this        

    interface IObservable<'a> with
        member this.Subscribe obs = this.Dependencies.Subscribe(obs,this)
    interface ITracksDependents with
        member this.Track dep = this.Dependencies.Add (dep,this)
        member this.Untrack dep = this.Dependencies.Remove (dep,this)
    interface IDependent with
        member __.UpdateDirtyFlag _ = ()
        member this.HasDependencies with get() = this.Dependencies.HasDependencies
    interface ISignal<'a> with
        member __.Value with get() = v

    interface IMutatable<'a> with
        member this.Value with get() = v and set(v) = this.Value <- v

type MappingSignal<'a,'b when 'a : equality and 'b : equality>(valueProvider : ISignal<'a>, mapping : 'a -> 'b, disposeProviderOnDispose : bool) as self =
    let mutable valueProvider = Some(valueProvider)
    
    // Note that we default this here, then set it afterwards.  
    // This avoids an invalid operation exception in the finalizer
    // if the mapping throws at construction time.
    let mutable lastValue = Unchecked.defaultof<'b>
    let mutable lastInput = Unchecked.defaultof<'a>
    let mutable disposed = false

    do
        lastInput <- valueProvider.Value.Value
        lastValue <- mapping lastInput
        valueProvider.Value.Track self

    let dependencies = Dependencies.create [| valueProvider.Value :> ITracksDependents |] self
    let mutable dirty = false

    /// Gets the current value
    member this.Value 
        with get() : 'b = 
            lock dependencies <| fun _ ->
                if dirty then
                    let input = DisposeHelpers.getValue valueProvider (fun _ -> this.GetType().FullName)
                    if lastInput <> input then
                        lastInput <- input
                        let value = lastInput |> mapping
                        if lastValue <> value then
                            lastValue <- value
                        value
                    else
                        lastValue
                else lastValue

    member __.Dirty 
        with get() = lock dependencies (fun _ -> dirty) 
        and set(v) = lock dependencies (fun _ -> dirty <- v)
                
    /// Notifies us that we need to refresh our value
    member this.MarkDirty source =
        lock dependencies <| fun _ -> 
            dirty <- true
            dependencies.MarkDirty this |> ignore
        //this.MarkDirtyGuarded source

    /// Default implementations work off single set of dependenices
    abstract member HasDependencies : bool with get
    default __.HasDependencies with get() = dependencies.HasDependencies

    override this.Finalize() =
        (this :> IDisposable).Dispose()        

    interface ISignal<'b> with
        member this.Value with get() = lock dependencies <| fun _ -> this.Value

    interface IDependent with
        member this.UpdateDirtyFlag obj = this.MarkDirty obj
        member this.HasDependencies with get() = this.HasDependencies

    interface IObservable<'b> with
        member this.Subscribe obs = dependencies.Subscribe (obs,this)

    interface ITracksDependents with
        member this.Track dep = dependencies.Add (dep,this)
        member this.Untrack dep = dependencies.Remove (dep,this)

    interface IDisposable with
        member this.Dispose () =
            dependencies.RemoveAll this
            GC.SuppressFinalize this        

type internal Mapping2Signal<'a,'b,'c when 'a : equality and 'b : equality and 'c : equality>(valueProvider1 : ISignal<'a>, valueProvider2 : ISignal<'b>, mapping : 'a -> 'b -> 'c) as self =
    let mutable lastValue = Unchecked.defaultof<'c> 
    let mutable lastInput1 = Unchecked.defaultof<'a>
    let mutable lastInput2 = Unchecked.defaultof<'b>
    let mutable valueProvider1 = Some(valueProvider1)
    let mutable valueProvider2 = Some(valueProvider2)

    do 
        lastInput1 <- valueProvider1.Value.Value
        lastInput2 <- valueProvider2.Value.Value
        lastValue <- mapping lastInput1 lastInput2
        valueProvider1.Value.Track self
        valueProvider2.Value.Track self

    let dependencies = 
        Dependencies.create 
            [| valueProvider1.Value :> ITracksDependents
               valueProvider1.Value :> ITracksDependents |] 
            self
    let mutable dirty = false

    /// Gets the current value
    member this.Value 
        with get() : 'c  = 
            lock dependencies <| fun _ ->
                if dirty then
                    let input1 = DisposeHelpers.getValue valueProvider1 (fun _ -> this.GetType().FullName)
                    let input2 = DisposeHelpers.getValue valueProvider2 (fun _ -> this.GetType().FullName)
                    if lastInput1 <> input1 || lastInput2 <> input2 then
                        lastInput1 <- input1
                        lastInput2 <- input2
                        let value = mapping input1 input2
                        if lastValue <> value then
                            lastValue <- value
                            this.MarkDirty (this :> obj)
                        value
                    else
                        lastValue
                else
                    lastValue

    member __.Dirty 
        with get() = lock dependencies (fun _ -> dirty) 
        and set(v) = lock dependencies (fun _ -> dirty <- v)
                
    /// Notifies us that we need to refresh our value
    member this.MarkDirty source =
        lock dependencies <| fun _ -> 
            dirty <- true
            dependencies.MarkDirty this |> ignore

    /// Default implementations work off single set of dependenices
    member __.HasDependencies with get() = dependencies.HasDependencies

    override this.Finalize() =
        (this :> IDisposable).Dispose()        

    interface ISignal<'c> with
        member this.Value with get() = lock dependencies <| fun _ -> this.Value

    interface IDependent with
        member this.UpdateDirtyFlag obj = this.MarkDirty obj
        member this.HasDependencies with get() = this.HasDependencies

    interface IObservable<'c> with
        member this.Subscribe obs = dependencies.Subscribe (obs,this)

    interface ITracksDependents with
        member this.Track dep = dependencies.Add (dep,this)
        member this.Untrack dep = dependencies.Remove (dep,this)

    interface IDisposable with
        member this.Dispose () =
            this |> DisposeHelpers.cleanup &valueProvider1 false
            this |> DisposeHelpers.cleanup &valueProvider2 false 
            dependencies.RemoveAll this
            GC.SuppressFinalize this        

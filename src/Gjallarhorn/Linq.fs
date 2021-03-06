﻿namespace Gjallarhorn.Linq

open Gjallarhorn
open Gjallarhorn.Helpers
open Gjallarhorn.Internal
open Gjallarhorn.Validation

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Threading.Tasks

/// Functions for working with Mutables from C#
[<AbstractClass;Sealed>]
type Mutable() =
    /// Create a mutable given an initial value
    static member Create<'a when 'a : equality and 'a : not struct> (value : 'a) =
        new AtomicMutable<'a>(value) :> IMutatable<'a>
    
    /// Update a mutable given a stepping function
    static member Update<'a> (original : IMutatable<'a>, steppingFunction : Func<'a, 'a>) =
        Mutable.step steppingFunction.Invoke original

/// Functions for working with Signals from C#
[<AbstractClass;Sealed>]
type Signal() =
    /// Create a signal (which never notifies) given a constant value
    static member FromConstant<'a> (value : 'a) =
        Signal.constant value

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, mapping:Func<_,_,_>) =
        Signal.map2 (mapping.ToFSharpFunc()) signal1 signal2

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, mapping:Func<_,_,_,_>) =
        Signal.map3 (mapping.ToFSharpFunc()) signal1 signal2 signal3

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, mapping:Func<_,_,_,_,_>) =
        Signal.map4 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, mapping:Func<_,_,_,_,_,_>) =
        Signal.map5 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, signal6, mapping:Func<_,_,_,_,_,_,_>) =
        Signal.map6 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5 signal6

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, signal6, signal7, mapping:Func<_,_,_,_,_,_,_,_>) =
        Signal.map7 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5 signal6 signal7

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, signal6, signal7, signal8, mapping:Func<_,_,_,_,_,_,_,_,_>) =
        Signal.map8 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, signal6, signal7, signal8, signal9, mapping:Func<_,_,_,_,_,_,_,_,_,_>) =
        Signal.map9 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8 signal9

    /// Combines signals using a specified mapping function
    static member Combine (signal1, signal2, signal3, signal4, signal5, signal6, signal7, signal8, signal9, signal10, mapping:Func<_,_,_,_,_,_,_,_,_,_,_>) =
        Signal.map10 (mapping.ToFSharpFunc()) signal1 signal2 signal3 signal4 signal5 signal6 signal7 signal8 signal9 signal10

/// Extension methods for working with Signals from C# using a LINQ inspired API    
[<AbstractClass;Sealed;Extension>]
type SignalExtensions() =
    /// Create a subscription to the changes of a signal which calls the provided function upon each change
    [<Extension>]
    static member Subscribe<'a when 'a : equality>(this : ISignal<'a>, func : Action<'a>) =
        Signal.Subscription.create func.Invoke this

    /// Create a subscription to the changes of a signal which copies its value upon change into a mutable
    [<Extension>]
    static member CopyTo<'a when 'a : equality>(this : ISignal<'a>, target : IMutatable<'a>) =
        Signal.Subscription.copyTo target this

    [<Extension>]
    /// Create a subscription to the changes of a signal which copies its value upon change into a mutable via a stepping function
    static member SubscribeAndUpdate<'a,'b when 'a : equality>(this : ISignal<'a>, target : IMutatable<'b>, stepFunction : Func<'b,'a,'b>) =
        let f = stepFunction.ToFSharpFunc()
        Signal.Subscription.copyStep target f this

    // TODO: port fromObservable

    [<Extension>]
    /// Perform a mapping from one signal to another
    static member Select<'a,'b when 'a : equality and 'b : equality> (this:ISignal<'a>, mapper:Func<'a,'b>) =
        this
        |> Signal.map (mapper.ToFSharpFunc())

    [<Extension;EditorBrowsable(EditorBrowsableState.Never)>]
    /// Perform a projection from a signal, typically only used for query syntax
    static member SelectMany<'a,'b>(this:ISignal<'a>, mapper:Func<'a,ISignal<'b>>) =
        mapper.Invoke(this.Value)

    [<Extension;EditorBrowsable(EditorBrowsableState.Never)>]
    /// Perform a projection from a signal, typically only used for query syntax
    static member SelectMany<'a,'b,'c when 'a : equality and 'b : equality and 'c : equality>(this:ISignal<'a>, mapper:Func<'a,ISignal<'b>>, selector:Func<'a,'b,'c>) : ISignal<'c> =
        let b' = mapper.Invoke(this.Value)
        Signal.map2 (fun a b -> selector.Invoke(a,b)) this b'

    [<Extension>]
    /// Creates a signal on two values that is true if both inputs are equal
    static member Equal<'a when 'a : equality> (this:ISignal<'a>, other:ISignal<'a>) =
        this
        |> Signal.equal other

    [<Extension>]
    /// Creates a signal on two values that is true if both inputs are not equal
    static member NotEqual<'a when 'a : equality> (this:ISignal<'a>, other:ISignal<'a>) =
        this
        |> Signal.notEqual other

    [<Extension>]
    /// Creates a signal over a bool value that negates the input
    static member Not (this:ISignal<bool>) =
        this
        |> Signal.not

    [<Extension>]
    /// Creates a signal on two bools that is true if both inputs are true
    static member And (this:ISignal<bool>, other:ISignal<bool>) =
        this
        |> Signal.both other

    [<Extension>]
    /// Creates a signal on two bools that is true if either input is true
    static member Or (this:ISignal<bool>, other:ISignal<bool>) =
        this
        |> Signal.either other
        
//NOTE: This allows non-F# extensions to have proper visibility/interop with all CLR languages
//NOTE: This attribute is only required once per assembly.
//NOTE: This is being placed _outside_ of AssemblyInfo.fs since that file gets automatically
//      regenerated (and inserting it into the generated file every time is kind of a pain).
[<assembly: ExtensionAttribute()>]
do()

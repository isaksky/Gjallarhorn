module Gjallarhorn.Tests.Observable

open Gjallarhorn

open System
open NUnit.Framework

[<TestCase(1, 2)>]
[<TestCase(42, 32)>]
[<TestCase(Int32.MinValue, Int32.MaxValue)>]
let ``Mutation triggers IObservable`` (start : int) finish =
    let result = Mutable.create start
    
    let changedValue = ref result.Value
    use subscription = 
        result |> Observable.subscribe((fun i -> changedValue := i))
    
    result.Value <- finish
    Assert.AreEqual(finish, !changedValue)

[<TestCase(1, 2, "2")>]
[<TestCase(42, 32, "32")>]
[<TestCase(Int32.MinValue, Int32.MaxValue, "2147483647")>]
let ``Signal triggers IObservable`` (start : int) (finish:int) (viewFinish: string) =
    let result = Mutable.create start
    let view = Signal.map (fun i -> i.ToString()) result    
    
    let changedValue = ref view.Value
    use subscription = 
        view |> Observable.subscribe((fun s -> changedValue := s))
        
    result.Value <- finish
    Assert.AreEqual(viewFinish, !changedValue)

[<TestCase(1, 2)>]
[<TestCase(42, 32)>]
[<TestCase(Int32.MinValue, Int32.MaxValue)>]
let ``Observable Dispose stops tracking`` (start:int) finish =
    let result = Mutable.create start    
    
    let changedValue = ref result.Value
    let subscription = 
        result |> Observable.subscribe((fun i -> changedValue := i))
        
    // Should track/change
    result.Value <- finish
    Assert.AreEqual(finish, !changedValue)
    subscription.Dispose()
    
    // Should not track/change anymore
    result.Value <- start
    Assert.AreEqual(finish, !changedValue)

[<TestCase(1, 2, "2")>]
[<TestCase(42, 32, "32")>]
[<TestCase(Int32.MinValue, Int32.MaxValue, "2147483647")>]
let ``Signal triggers IObservable with add, even after GC`` (start : int) (finish:int) (viewFinish: string) =
    let result = Mutable.create start
    let view = Signal.map (fun i -> i.ToString()) result    
    
    let changedValue = ref view.Value    
    view |> Observable.add((fun s -> changedValue := s))
        
    GC.Collect()
    GC.WaitForPendingFinalizers()

    // Note that we keep view alive - when it GCs, the observer finishes
    GC.KeepAlive(view)

    result.Value <- finish
    
    Assert.AreEqual(viewFinish, !changedValue)

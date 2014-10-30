module Gjallarhorn.Tests.View

open Gjallarhorn.Control

open System
open NUnit.Framework

[<Test;TestCaseSource(typeof<Utilities>,"CasesStart")>]
let ``View.constant constructs with proper value`` start =
    let value = View.constant start

    Assert.AreEqual(box start, value.Value)

[<Test;TestCaseSource(typeof<Utilities>,"CasesStartToString")>]
let ``View.map constructs from mutable`` start finish =
    let value = Mutable.create start
    let view = 
        value 
        |> View.map (fun i -> i.ToString()) 

    Assert.AreEqual(box view.Value, finish)

[<Test;TestCaseSource(typeof<Utilities>,"CasesPairToString")>]
let ``View.map2 constructs from mutables`` start1 start2 finish =
    let v1 = Mutable.create start1
    let v2 = Mutable.create start2
    let map i j = i.ToString() + "," + j.ToString()
    let view = 
        View.map2 map v1 v2

    Assert.AreEqual(box view.Value, finish)


[<Test;TestCaseSource(typeof<Utilities>,"CasesStartEndToStringPairs")>]
let ``View updates with mutable`` start initialView finish finalView =
  let result = Mutable.create start
  let view = View.map (fun i -> i.ToString()) result

  Assert.AreEqual(view.Value, initialView)

  result.Value <- finish
  Assert.AreEqual(view.Value, finalView)

[<Test;TestCaseSource(typeof<Utilities>,"CasesPairStartEndToStringPairs")>]
let ``View2 updates from mutables`` start1 start2 startResult finish1 finish2 finishResult =
    let v1 = Mutable.create start1
    let v2 = Mutable.create start2
    let view = View.map2 (fun i j -> i.ToString() + "," + j.ToString()) v1 v2

    Assert.AreEqual(box view.Value, startResult)

    v1.Value <- finish1
    v2.Value <- finish2

    Assert.AreEqual(box view.Value, finishResult)


[<Test;TestCaseSource(typeof<Utilities>,"CasesStartEndToStringPairs")>]
let ``View updates with view`` start initialView finish finalView =
    // Create a mutable value
    let result = Mutable.create start
    
    // Create a view to turn the value from int -> string
    let view = View.map (fun i -> i.ToString()) result
    
    // Create a view to turn the first view back from string -> int
    let backView = View.map (fun s -> Convert.ChangeType(s, start.GetType())) view
    
    Assert.AreEqual(view.Value, initialView)
    Assert.AreEqual(backView.Value, start)
    
    result.Value <- finish
    Assert.AreEqual(view.Value, finalView)
    Assert.AreEqual(backView.Value, finish)

[<Test;TestCaseSource(typeof<Utilities>,"CasesStartEndToStringPairs")>]
let ``Cached View updates with View`` start initialView finish finalView =
    // Create a mutable value
    let result = Mutable.create start
    
    // Create a view to turn the value from int -> string
    let view = View.map (fun i -> i.ToString()) result
    
    // Create a view to turn the first view back from string -> int
    let bv = View.map (fun s -> Convert.ChangeType(s, start.GetType())) view

    // Cache the view
    let backView = View.cache bv
    
    Assert.AreEqual(view.Value, initialView)
    Assert.AreEqual(backView.Value, start)
    
    result.Value <- finish
    Assert.AreEqual(view.Value, finalView)
    Assert.AreEqual(backView.Value, finish)

[<Test>]
let ``Operator <*> allows arbitrary arity`` () =
    let f = (fun a b c d -> sprintf "%d,%d,%d,%d" a b c d)
    let v1 = Mutable.create 1
    let v2 = Mutable.create 2
    let v3 = Mutable.create 3
    let v4 = Mutable.create 4
    
    let view = View.constant f <*> v1 <*> v2 <*> v3 <*> v4

    Assert.AreEqual(view.Value, "1,2,3,4")

[<Test>]
let ``Operator <*> preserves tracking`` () =
    let f = (fun a b c d -> sprintf "%d,%d,%d,%d" a b c d)
    let v1 = Mutable.create 1
    let v2 = Mutable.create 2
    let v3 = Mutable.create 3
    let v4 = Mutable.create 4
    
    let view = View.constant f <*> v1 <*> v2 <*> v3 <*> v4
    // let view = View.apply( View.apply( View.apply( View.apply (View.constant f) v1) v2) v3) v4
    
    // Mutate
    v1.Value <- 5
    v3.Value <- 7
    Assert.AreEqual(view.Value, "5,2,7,4")

[<Test>]
let ``Compose a view using a computation expression``() =
    let m1 = Mutable.create 1
    let m2 = Mutable.create 2

    let v1 = View.map (fun i -> i+10) m1
    let v2 = View.map (fun i -> i*100) m2

    let view = View.compose {
        let! start = v1
        let! finish = v2
        let! mut = m1
        if finish > 500 then
            return start
        else
            return start + finish + mut
    }
    
    Assert.AreEqual(view.Value, 212)

    // Mutate
    m1.Value <- 5
    Assert.AreEqual(view.Value, 220)

    m2.Value <- 7
    Assert.AreEqual(view.Value, 15)
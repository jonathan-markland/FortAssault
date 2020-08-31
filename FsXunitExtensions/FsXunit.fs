module FsXunit

open Xunit

// ---------------------------------------------------------------------------------------------
//  General pass/fail outcomes
// ---------------------------------------------------------------------------------------------

let TestPasses () =
    Assert.True(true)

let TestFailBecause message =
    Assert.True(false, message)

let TestFailWhenDoing (activityDescription:string) =
    let message = sprintf "Expected %s to succeed" activityDescription
    TestFailBecause message

// ---------------------------------------------------------------------------------------------
//  Assertions on booleans
// ---------------------------------------------------------------------------------------------

let private ActivityAssertTrue activityDescription (condition:bool) =
    let message = sprintf "Expected %s to succeed" activityDescription
    Assert.True(condition, message)

let private ActivityAssertFalse activityDescription (condition:bool) =
    let message = sprintf "Expected %s to fail" activityDescription
    Assert.False(condition, message)

let ShouldBeTrueWhenDoing activityDescription condition =
    condition |> ActivityAssertTrue activityDescription

let ShouldBeFalseWhenDoing activityDescription condition =
    condition |> ActivityAssertFalse activityDescription

// ---------------------------------------------------------------------------------------------
//  Assertions about equality
// ---------------------------------------------------------------------------------------------

let ShouldEqual expected actual =
    let result = (expected = actual)
    Assert.True(result, sprintf "Items differ, expected vs actual:\r\n%A\r\n%A" expected actual)

let ShouldEqualInt (expected:int) (actual:int) =
    Assert.Equal(expected, actual)

let ShouldEqualString (expected:string) (actual:string) =
    Assert.Equal(expected, actual)

// ---------------------------------------------------------------------------------------------
//  Outcome type to unify handling of Option and Result types
// ---------------------------------------------------------------------------------------------

[<Struct>]
type private FsXUnitOutcomeType<'a,'b> =
    | Outcome1 of one:'a
    | Outcome2 of two:'b

let private OptionToOutcome optionType =
    match optionType with
        | Some value -> Outcome1 value
        | None       -> Outcome2 ()

let private ResultToOutcome resultType =
    match resultType with
        | Ok value    -> Outcome1 value
        | Error value -> Outcome2 value

let private ShouldBeOutcome1WhenDoing realType activityDescription pred outcome =
    match outcome with
        | Outcome1 value ->
            if pred value then
                TestPasses ()
            else 
                let message = sprintf "The %s returned from %s didn't have the expected value." realType activityDescription
                TestFailBecause message
        | Outcome2 _ -> 
            let message = sprintf "Expected %s to be %s" activityDescription realType
            TestFailBecause message

let private ShouldBeOutcome2WhenDoing realType activityDescription pred outcome =
    match outcome with
        | Outcome1 _ ->
            let message = sprintf "Expected %s to be %s" activityDescription realType
            TestFailBecause message
        | Outcome2 value -> 
            if pred value then
                TestPasses ()
            else 
                let message = sprintf "The %s returned from %s didn't have the expected value." realType activityDescription
                TestFailBecause message

let private Tautology _ =
    true

// ---------------------------------------------------------------------------------------------
//  Assertions on Option types
// ---------------------------------------------------------------------------------------------
    
let ShouldBeOptionSomeWhenDoing activityDescription pred option =
    option
        |> OptionToOutcome
        |> ShouldBeOutcome1WhenDoing "Option.Some" activityDescription pred

let ShouldBeOptionNoneWhenDoing activityDescription option =
    option
        |> OptionToOutcome
        |> ShouldBeOutcome2WhenDoing "Option.None" activityDescription Tautology

// ---------------------------------------------------------------------------------------------
//  Assertions on Result types
// ---------------------------------------------------------------------------------------------

let ShouldBeResultOkWhenDoing activityDescription pred result =
    result
        |> ResultToOutcome
        |> ShouldBeOutcome1WhenDoing "Result.Ok" activityDescription pred

let ShouldBeResultErrorWhenDoing activityDescription pred result =
    result
        |> ResultToOutcome
        |> ShouldBeOutcome2WhenDoing "Result.Error" activityDescription pred

let ShouldBeAnyResultOkWhenDoing activityDescription result =
    ShouldBeResultOkWhenDoing activityDescription Tautology result

let ShouldBeAnyResultErrorWhenDoing activityDescription result =
    ShouldBeResultErrorWhenDoing activityDescription Tautology result

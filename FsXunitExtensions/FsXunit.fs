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
//  Wider Test capture support
// ---------------------------------------------------------------------------------------------

/// If the code under test raises a stray exception, then 
/// include additional contextual information for the test 
/// failure report.
let WithContextIfStrayException activityDescription testFunction =  // TODO: remove
    try
        testFunction ()
    with 
        | :? Xunit.Sdk.XunitException as e ->
            failwith (sprintf "An Xunit assertion failed while %s: %s" activityDescription (e.Message))
        | other ->
            failwith (sprintf "Code under test raised exception while %s: %s" activityDescription (other.Message))


type TestExecutionSnapshot =
    | TestTerminatedNormally of context:string option
    | XunitAssertFailed      of context:string option * Xunit.Sdk.XunitException
    | StrayExceptionHappened of context:string option * System.Exception


/// Invokes the testFunction with the testData, and captures the result in a shapshot.
/// The testFunction should be the function to test, or indirectly, something that
/// invokes the function to test with additional light preparation.  The testFunction
/// is at liberty to perform Xunit assertions.  Any (non-Xunit) exceptions that escape 
/// the testFunction are bound into the returned snapshot.
let WhenInputTo testFunction testInputData =
    try
        testInputData |> testFunction
        TestTerminatedNormally(None)
    with
        | :? Xunit.Sdk.XunitException as xue ->
            XunitAssertFailed(None, xue)
        | other ->
            StrayExceptionHappened(None, other)



let private TidyAppend (afterStr:string) (beforeStr:string) =
    let str1 = beforeStr.Trim()
    let str2 = afterStr.Trim()
    if str1.Length > 0 && str2.Length > 0 then
        str1 + System.Environment.NewLine + str2
    else if str1.Length > 0 then
        str1
    else    
        str2




/// Apply an additional user-defined message to the snapshot, which 
/// is useful if you want test fail messages to have more detailed 
/// contextual information.
let InContext newContextMessage testExecutionSnapshot =

    let newContextMessage = "When " + newContextMessage + ":"

    let before existingContext str =
        Some (str |> TidyAppend (existingContext |> Option.defaultValue ""))

    match testExecutionSnapshot with
        
        | TestTerminatedNormally existingContext -> 
            TestTerminatedNormally(newContextMessage |> before existingContext)
        
        | XunitAssertFailed(existingContext, xue) ->
            XunitAssertFailed(newContextMessage |> before existingContext, xue)
        
        | StrayExceptionHappened(existingContext, ex) ->
            StrayExceptionHappened(newContextMessage |> before existingContext, ex)



let private Whoops ctx msg =
    failwith (ctx |> Option.defaultValue "" |> TidyAppend msg)



let private WhoopsEx ctx explanation (ex:System.Exception) =
    failwith (ctx |> Option.defaultValue "" |> TidyAppend explanation |> TidyAppend (ex.Message))



/// Asserts that the test function should have terminated without stray exceptions
/// and that no Xunit assertions that may have been performed did fire.
let ShouldPass testExecutionSnapshot =

    match testExecutionSnapshot with
        
        | TestTerminatedNormally _ -> 
            TestPasses ()
        
        | XunitAssertFailed(ctx, xue) ->
            WhoopsEx ctx "This test should have passed, but an Xunit assertion failed instead:" xue
        
        | StrayExceptionHappened(ctx, ex) ->
            WhoopsEx ctx "This test should have passed, but an exception was thrown:" ex



/// Asserts that the test function threw an exception, and that the function did
/// not just terminate normally, and nor did any Xunit assertions that may have
/// been performed raise failures.
let ShouldFailWithAnException testExecutionSnapshot =

    match testExecutionSnapshot with
        
        | TestTerminatedNormally ctx -> 
            Whoops ctx "This test should have failed with an exception but it terminated normally."
        
        | XunitAssertFailed(ctx, xue) ->
            WhoopsEx ctx "This test should have failed with an exception but an Xunit assertion failed instead:" xue
        
        | StrayExceptionHappened _ ->
            // Here the we are not interested in the exception type or content.
            TestPasses ()



/// Asserts that the test function threw an exception, and that the function did
/// not just terminate normally, and nor did any Xunit assertions that may have
/// been performed raise failures.
let ShouldFailWithExceptionMessageWhere predicate testExecutionSnapshot =

    match testExecutionSnapshot with
        
        | TestTerminatedNormally ctx -> 
            Whoops ctx "This test should have failed with an exception but it terminated normally."
        
        | XunitAssertFailed(ctx, xue) ->
            WhoopsEx ctx "This test should have failed with an exception but an Xunit assertion failed instead:" xue
        
        | StrayExceptionHappened(ctx, ex) ->
            let message = ex.Message
            if predicate message then
                TestPasses ()
            else
                WhoopsEx ctx "This test indeed raised an exception, but the exception message didn't meet the test expectation.  The unexpected message was:" ex




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

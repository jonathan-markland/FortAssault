module EnterYourNameTests

open Xunit
open FsXunit
open EnterYourName


let JonathanModel =
    {
        Name          = "JONATHAN"
        SelectorIndex = 0
        MaxNameLength = 12
        Complete      = false
    }



[<Fact>]
let ``Test model screen display`` () =

    JonathanModel
        |> EnterYourNameModelScreenText
        |> ShouldEqual
            [
                "ENTER YOUR NAME"
                ""
                ""
                "JONATHAN...."
                ""
                ""
                " ."
                "PQRSTUVWXYZ .X ABCDEFGHIJKLMNO"
            ]



[<Fact>]
let ``Test rotate right`` () =

    JonathanModel
        |> EnterYourNameModelWithInputApplied RotateRight
        |> EnterYourNameModelScreenText
        |> ShouldEqual
            [
                "ENTER YOUR NAME"
                ""
                ""
                "JONATHAN...."
                ""
                ""
                " ."
                "QRSTUVWXYZ .X ABCDEFGHIJKLMNOP"
            ]


[<Fact>]
let ``Test rotate left`` () =

    JonathanModel
        |> EnterYourNameModelWithInputApplied RotateLeft
        |> EnterYourNameModelScreenText
        |> ShouldEqual
            [
                "ENTER YOUR NAME"
                ""
                ""
                "JONATHAN...."
                ""
                ""
                " ."
                "OPQRSTUVWXYZ .X ABCDEFGHIJKLMN"
            ]


[<Fact>]
let ``Test select character`` () =

    JonathanModel
        |> EnterYourNameModelWithInputApplied SelectLetter
        |> EnterYourNameModelScreenText
        |> ShouldEqual
            [
                "ENTER YOUR NAME"
                ""
                ""
                "JONATHANA..."
                ""
                ""
                " ."
                "PQRSTUVWXYZ .X ABCDEFGHIJKLMNO"
            ]



[<Fact>]
let ``Test delete character`` () =

    JonathanModel
        |> EnterYourNameModelWithInputApplied RotateLeft
        |> EnterYourNameModelWithInputApplied RotateLeft
        |> EnterYourNameModelWithInputApplied SelectLetter
        |> EnterYourNameModelScreenText
        |> ShouldEqual
            [
                "ENTER YOUR NAME"
                ""
                ""
                "JONATHA....."
                ""
                ""
                " ."
                "NOPQRSTUVWXYZ .X ABCDEFGHIJKLM"
            ]



[<Fact>]
let ``Complete flag is FALSE if insufficient characters entered`` () =

    JonathanModel
        |> NameEntryComplete
        |> ShouldBeFalseWhenDoing "checking if name entry is completed"


[<Fact>]
let ``Complete flag is still FALSE when all characters entered`` () =

    { JonathanModel with Name="JONATHAN...." }                // The name is full.
        |> NameEntryComplete
        |> ShouldBeFalseWhenDoing "checking if name entry is completed"


[<Fact>]
let ``Complete flag is TRUE if sufficient characters entered and another letter selected`` () =

    { JonathanModel with Name="JONATHAN...." }                // The name is full.
        |> EnterYourNameModelWithInputApplied (SelectLetter)  // Now try entering one more character.
        |> NameEntryComplete
        |> ShouldBeTrueWhenDoing "checking if name entry is complete when selecting addition character once input box is full"



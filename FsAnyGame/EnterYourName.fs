module EnterYourName

type EnterYourNameModel =
    {
        Name          : string
        SelectorIndex : int
        MaxNameLength : int
        Complete      : bool
    }


type EnterYourNameInput =
    | RotateLeft
    | RotateRight
    | SelectLetter


let Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ .X "  // the second X is the delete index.


let NewEnterYourNameModel maxNameLength =
    {
        Name          = ""
        SelectorIndex = 0
        MaxNameLength = maxNameLength
        Complete      = false
    }


let EnterYourNameModelWithInputApplied input model =

    let name  = model.Name
    let index = model.SelectorIndex

    let endIndex    = Alphabet.Length - 1
    let deleteIndex = endIndex - 1

    match input with

        | RotateLeft ->

            let index = if index = 0 then endIndex else index - 1
            { model with SelectorIndex = index }

        | RotateRight ->

            let index = if index = endIndex then 0 else index + 1
            { model with SelectorIndex = index }

        | SelectLetter ->

            let alreadyFull = (name.Length = model.MaxNameLength)

            let name = 
                if alreadyFull then
                    name
                else if index = deleteIndex then
                    if name.Length = 0 then name else name.Substring(0, name.Length - 1)
                else
                    name + Alphabet.[index].ToString()

            { model with Name = name ; Complete = alreadyFull }


let RotatedAlphabet (alphabet:string) index =

    let a = alphabet
    let n = a.Length
    let h = n / 2

    let split = (index - h)
    let split = if split < 0 then split + n else split

    a.Substring(split) + a.Substring(0,split)


let EnterYourNameModelScreenText model =
    [
        "ENTER YOUR NAME"
        ""
        ""
        model.Name.PadRight(model.MaxNameLength, '.')
        ""
        ""
        " ."
        RotatedAlphabet Alphabet model.SelectorIndex
    ]


let NameEntryComplete model =

    model.Complete


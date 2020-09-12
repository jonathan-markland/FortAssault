
open System.IO

let SanitizedForIdentifiers (s:string) =
    s.Replace("-", "")

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let FileSequenceMap f fileSet =
    fileSet 
        |> Seq.map (fun filePath ->
            let leafName                 = Path.GetFileName(filePath:string)
            let leafNameWithoutExtension = Path.GetFileNameWithoutExtension(filePath) |> SanitizedForIdentifiers
            f filePath leafName leafNameWithoutExtension)


let DumpSequenceToConsole sq =
    sq |> Seq.iter (fun s -> System.Console.WriteLine(s:string))


let WithSomeSpacePadding (s:string) =
    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ImagesHTML fileSet =

    let recordFields fileSet =
        fileSet 
            |> FileSequenceMap (fun filePath leafName leafNameWithoutExtension ->
                sprintf "<img id=\"%s\" src=\"%s\"/>" leafNameWithoutExtension leafName)

    seq
        {
            yield! fileSet |> recordFields
        }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let GameSpritesModule fileSet =

    let recordFields fileSet =
        fileSet 
            |> FileSequenceMap (fun _filePath _leafName leafNameWithoutExtension ->
                sprintf "        %s : ImageFileMetaData" (leafNameWithoutExtension |> WithSomeSpacePadding))

    let fieldInitializer fileSet =
        fileSet 
            |> FileSequenceMap (fun _filePath leafName leafNameWithoutExtension ->
                sprintf "        %s = fromFile magenta \"%s\"" (leafNameWithoutExtension |> WithSomeSpacePadding) leafName)

    seq
        {
            yield "module GameSprites"
            yield ""
            yield "type GameSprites ="
            yield "{"
            yield! fileSet |> recordFields
            yield "}"
            yield ""
            yield ""
            yield ""

            yield "let LoadGameImages (renderer:RendererNativeInt) rootPath ="
            yield ""
            yield "    let fromFile transparencyColour name = "
            yield ""
            yield "        let fullPath = Path.Combine(rootPath, name)"
            yield ""
            yield "        match LoadFromFileAndPrepareForRenderer renderer fullPath transparencyColour with"
            yield "            | Some(imageRecord) -> imageRecord"
            yield "            | None -> failwith (sprintf \"Game could not start because file '%s' is missing or has invalid content.\" fullPath)"
            yield ""
            yield "    let magenta = Some({ Red=255uy ; Green=0uy ; Blue=255uy })"
            yield ""
            yield "    {"
            yield! fileSet |> fieldInitializer
            yield "    }"

        }
        
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<EntryPoint>]
let main argv =

    let SearchPath = @"C:\Users\Jonathan\Documents\Work\FsBeachHead\FsBeachHeadSDL2CS\Images\"

    let fileSet =
        Directory.EnumerateFiles(SearchPath, "*.png")

    fileSet |> GameSpritesModule |> DumpSequenceToConsole
    
    0


/// Framework for SDL2-Desktop games.
module DesktopGameFramework

open System.IO
open SDLCover
open ResourceFileMetadata



type GameResourcesRecord =
    {
        GameBMPs    : ImageWithHostObject[]
        Fonts       : NumCapsFontDefinition[]
    }



let LoadGameImagesAndFonts gameResourceImages gameFontResourceImages (renderer:RendererNativeInt) rootPath =

    let fromFile transparencyColour name = 

        let fullPath =
            Path.Combine(rootPath, name)
        
        if not (File.Exists(fullPath)) then
            failwith (sprintf "Game could not start because file '%s' is missing." fullPath)

        match LoadFromFileAndPrepareForRenderer renderer fullPath transparencyColour with
            | Some(imageRecord) -> imageRecord
            | None -> failwith (sprintf "Game could not start because file '%s' has invalid content." fullPath)

    let unwrapFont fileName opt =
        match opt with
            | Some(font) -> font
            | None -> failwith (sprintf "Game could not start because font '%s' file has incorrect content." fileName)

    let magenta =
        Some({ Red=255uy ; Green=0uy ; Blue=255uy })
    
    let imagesArray =
        gameResourceImages
            |> List.map (fun metadata -> 
                
                let key = 
                    match metadata.ImageColourKey with 
                        | NoColourKey -> None 
                        | MagentaColourKey -> magenta
                
                let fileName = metadata.ImageFileName
                
                let hostImageObject = fromFile key fileName

                {
                    EngineImageMetadata = metadata
                    HostImageObject     = HostImageObject(hostImageObject)
                })

            |> List.toArray

    let fontsArray =
        gameFontResourceImages 
            |> List.map (fun metadata -> 
                fromFile magenta metadata.ImageFileName
                    |> MakeNumCapsFontFromBMP 
                    |> unwrapFont metadata.ImageFileName) 
                        |> List.toArray

    {
        GameBMPs = imagesArray
        Fonts    = fontsArray
    }




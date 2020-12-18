/// Only to be used from the Host program's initialisation.
module StaticResourceSetup

open ImagesAndFonts

let mutable (StaticImagesArray : Image[]) = [||]
let mutable (StaticFontsArray  : Font[])  = [||]

/// To be called by the game on startup to establish a collection
/// of images and fonts that can then be referred to by ImageID or FontID
/// as given in the ResourceFileMetadata module.
let SetStaticImageAndFontResourceArrays imageArray fontArray =
    StaticImagesArray <- imageArray
    StaticFontsArray  <- fontArray

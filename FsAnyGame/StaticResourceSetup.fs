/// Only to be used from the Host program's initialisation.
module StaticResourceSetup

open ResourceFileMetadata

let mutable (StaticImagesArray : ImageWithHostObject[]) = [||]

/// To be called by the Host to establish the game's statically available images.
let SetStaticImageResourceArray imageArray =
    StaticImagesArray <- imageArray




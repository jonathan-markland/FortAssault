module StaticResourceAccess

open ResourceFileMetadata
open StaticResourceSetup

let ImageFromID (ImageID(imageIndex)) =  // TODO:  Have another version of this that digs the engine metadata object out?
    StaticImagesArray.[imageIndex]


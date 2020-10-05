module StaticResourceAccess

open ImagesAndFonts
open StaticResourceSetup

/// Obtain an image given an ID number.
/// The ImageID number indexes a collection that must be 
/// set by calling the StaticResourceSetup module on game startup.
let inline ImageFromID (ImageID(imageIndex)) =  // TODO:  Have a convenience version of this that digs the engine metadata object out?
    StaticImagesArray.[imageIndex]

/// Obtain a font given an ID number.
/// The FontID number indexes a collection that must be 
/// set by calling the StaticResourceSetup module on game startup.
let inline FontFromID (FontID(fontIndex)) =
    StaticFontsArray.[fontIndex]

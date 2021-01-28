module Sounds

/// Sound information supplied by the game engine to the host.
type SoundMetadata =
    {
        /// The leaf-name of the file from which a sound resource originates.
        SoundFileName       : string
    }

/// Opaque type for referring to a *static* sound resource, which can be
/// obtained through the StaticResourceAccess module.
[<Struct>]
type SoundID = SoundID of int

/// A reference to the host's sound object (opaque type).
type HostSoundRef = HostSoundRef of obj

/// Sound record, used with the play function.
/// Includes metadata about the sound.
type Sound =
    {
        SoundMetadata   : SoundMetadata
        HostSoundRef    : HostSoundRef
    }

/// Description of an operation requested upon the sound subsystem.
type SoundOperation =

    /// Play a sound effect into the mix.
    | PlaySoundEffect of Sound

    /// Change the background music, or start it.
    | ChangeTheMusic of Sound

    /// Stop the background music.
    | StopTheMusic


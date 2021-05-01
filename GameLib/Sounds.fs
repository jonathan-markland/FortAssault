module Sounds

/// A game's engine will use this type to request a sound.
type RequestedSound =
    {
        RequestedSoundFileName : string
    }

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
    // TODO: | ChangeTheMusic of Sound  <--- reconsider design

    /// Stop the background music.
    // TODO: | StopTheMusic  <--- reconsider design

    // TODO: Consider for sound that "music" may be a looped effect.  We need looping.

    // TODO: Consider what could conveniently be the identity for a sound resource that
    //       would enable us to request switching an instance of it on, and then off again.


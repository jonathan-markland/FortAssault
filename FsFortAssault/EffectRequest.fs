module EffectRequest

open SoundEffects

/// Specifies effects that the framework should apply this frame.
type EffectRequest =
    {
        NewSoundsToBePlayed : SoundEffect list
        // TODO:  RumblePadActivation : bool
    }



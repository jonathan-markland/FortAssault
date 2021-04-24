/// All things related to the retro-screen size and window title.
module Screen

open Geometry

/// Settings used to initialise the framework
type RetroScreenSettings =
    {
        /// Shown in the browser or desktop window title bar
        RetroScreenTitle  : string

        /// Width required for the retro screen, in pixels.
        RetroScreenWidth  : int<epx>

        /// Height required for the retro screen, in pixels.
        RetroScreenHeight : int<epx>
    }

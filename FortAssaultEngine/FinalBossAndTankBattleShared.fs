module FinalBossAndTankBattleShared

open Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type Target =
    {
        TargetLocation : Point<float32<epx>>
    }

/// Targets remaining on the fort.  Passed between tank battle and fort screens.
type FinalBossTargets =
    {
        TargetsOnFinalBoss : Target list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#if SHORT_PLAYTHROUGH

let InitialTargetLocations =
    [
        // X coordinate is used as the IDENTITY of the record!

        (169.0F<epx> ,  50.0F<epx>)
        (231.0F<epx> ,  96.0F<epx>)
    ]

#else

let InitialTargetLocations =
    [
        // X coordinate is used as the IDENTITY of the record!

        (169.0F<epx> ,  50.0F<epx>)
        (134.0F<epx> ,  71.0F<epx>)
        (194.0F<epx> ,  90.0F<epx>)
        (123.0F<epx> , 111.0F<epx>)
        (221.0F<epx> ,  68.0F<epx>)
        ( 74.0F<epx> , 108.0F<epx>)
        (249.0F<epx> , 116.0F<epx>)
        (163.0F<epx> ,  83.0F<epx>)
        (104.0F<epx> ,  70.0F<epx>)
        (231.0F<epx> ,  96.0F<epx>)
    ]

#endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewFinalBossAndTankBattleData () =

    {
        TargetsOnFinalBoss = 
            InitialTargetLocations |> List.map (fun (x,y) -> 
                {
                    TargetLocation = { ptx=x ; pty=y }
                })
    }


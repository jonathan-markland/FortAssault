module FinalBossAndTankBattleShared

open Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type Target =
    {
        TargetLocation : PointF32
    }

/// State that is passed between the Tank battle and Final boss screens.
type FinalBossAndTankBattleData =
    {
        /// The targets remaining on the fort.
        TargetsOnFinalBoss : Target list

        /// Increments for each successful time through the tank course.
        /// Used to determine the map, must be range-checked against the maps array!
        TankBattleMapNumber : int
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewFinalBossAndTankBattleData () =

    {
        TargetsOnFinalBoss = 
            InitialTargetLocations |> List.map (fun (x,y) -> 
                {
                    TargetLocation = { ptx=x ; pty=y }
                })

        TankBattleMapNumber = 0
    }


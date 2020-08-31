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

        (169.0F<wu> ,  50.0F<wu>)
        (134.0F<wu> ,  71.0F<wu>)
        (194.0F<wu> ,  90.0F<wu>)
        (123.0F<wu> , 111.0F<wu>)
        (221.0F<wu> ,  68.0F<wu>)
        ( 74.0F<wu> , 108.0F<wu>)
        (249.0F<wu> , 116.0F<wu>)
        (163.0F<wu> ,  83.0F<wu>)
        (104.0F<wu> ,  70.0F<wu>)
        (231.0F<wu> ,  96.0F<wu>)
    ]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewFinalBossAndTankBattleData () =

    {
        TargetsOnFinalBoss = 
            InitialTargetLocations |> List.map (fun (x,y) -> 
                {
                    TargetLocation = { xwf=x ; ywf=y }
                })

        TankBattleMapNumber = 0
    }


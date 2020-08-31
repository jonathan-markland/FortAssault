module Collisions

open Algorithm
open ScoreHiScore



[<Struct>]
type private Notepad<'explosion> =
    {
        ExplosionsList: 'explosion list
        TotalScore:     uint32
    }



let ResultOfProjectileCollisions
        (projectiles:'projectile list)
        (targets:'target list)
        collides
        (getProjectileId:'projectile -> 'projectileId)
        (getTargetId:'target -> 'targetId)
        (explosions:'explosion list)
        (score:ScoreAndHiScore)
        createExplosionAndScoreFor =

    let collidingPairs =
        List.allPairs projectiles targets |> List.filter (fun (projectile,target) -> collides projectile target)

    match collidingPairs with

        | [] ->

            // performance optimisation when no intersections.

            projectiles, targets, explosions, score

        | _ ->

            let (collidedProjectiles, collidedTargets) = 
                collidingPairs |> List.unzip
    
            let collidedProjectiles = 
                collidedProjectiles |> List.distinctBy getProjectileId
    
            let collidedTargets = 
                collidedTargets |> List.distinctBy getTargetId

            let survivingProjectiles = 
                projectiles |> List.filter (NotInListById collidedProjectiles getProjectileId)
    
            let survivingTargets = 
                targets |> List.filter (NotInListById collidedTargets getTargetId)
    
            let newExplosions, totalScore = 
                
                let initialNotes = { ExplosionsList = explosions ; TotalScore = 0u }
                
                let folder notes projectile =
                    let explosion,score = createExplosionAndScoreFor projectile  // TODO: Separate into two functions the caller must pass
                    { ExplosionsList = explosion::notes.ExplosionsList ; TotalScore = notes.TotalScore + score }
                
                let finalNotes = collidedProjectiles |> List.fold folder initialNotes
                
                finalNotes.ExplosionsList, finalNotes.TotalScore

            survivingProjectiles, survivingTargets, newExplosions, score |> ScoreIncrementedBy totalScore




type PlayerSurvival =
    | PlayerDestroyed
    | PlayerSurvives


let ResultOfProjectileCollisionsWithSingleTarget
    (projectiles:'projectile list)
    (target:'target)
    collides
    (getProjectileId:'projectile -> 'projectileId)
    (explosions:'explosion list)
    (score:ScoreAndHiScore)
    createExplosionAndScoreFor =

    let survivingProjectiles, survivingTargets, newExplosions, score =
        ResultOfProjectileCollisions
            projectiles
            [target]  // TODO:  This is slightly bad -- making a list of one thing!
            collides
            getProjectileId
            (fun _ -> 1)
            explosions
            score
            createExplosionAndScoreFor 

    let status =
        if (survivingTargets |> List.isEmpty) then PlayerDestroyed else PlayerSurvives

    survivingProjectiles, status, newExplosions, score

module Collisions

open Algorithm



[<Struct>]
type private Notepad<'explosion> =
    {
        AdditionalExplosionsList : 'explosion list
        AdditionalScore          : uint32
    }



let ResultOfProjectileCollisions
        (projectiles     : 'projectile list)
        (targets         : 'target list)
        collides
        (getProjectileId : 'projectile -> 'projectileId)
        (getTargetId     : 'target -> 'targetId)
        createExplosionAndScoreFor =

    let collidingPairs =
        List.allPairs projectiles targets |> List.filter (fun (projectile,target) -> collides projectile target)

    match collidingPairs with

        | [] ->

            projectiles, targets, [], 0u   // performance optimisation when no intersections.

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
    
            let additionalExplosions, additionalScore = 
                
                let initialNotes = { AdditionalExplosionsList = [] ; AdditionalScore = 0u }
                
                let folder notes projectile =
                    let explosion,score = createExplosionAndScoreFor projectile  // TODO: Separate into two functions the caller must pass
                    { 
                        AdditionalExplosionsList = explosion::notes.AdditionalExplosionsList
                        AdditionalScore = notes.AdditionalScore + score 
                    }
                
                let finalNotes = collidedProjectiles |> List.fold folder initialNotes
                
                finalNotes.AdditionalExplosionsList, finalNotes.AdditionalScore

            survivingProjectiles, survivingTargets, additionalExplosions, additionalScore




type PlayerSurvival =
    | PlayerDestroyed
    | PlayerSurvives


let ResultOfProjectileCollisionsWithSingleTarget  // TODO:  Hmmm.. Ideology:  It isn't really a generic "target" if we return a "Player"-related status type!
    (projectiles     : 'projectile list)
    (target          : 'target)
    collides
    (getProjectileId : 'projectile -> 'projectileId)
    createExplosionAndScoreFor =

    let survivingProjectiles, survivingTargets, additionalExplosions, additionalScore =
        ResultOfProjectileCollisions
            projectiles
            [target]  // TODO:  This is slightly bad -- making a list of one thing!
            collides
            getProjectileId
            (fun _ -> 1)
            createExplosionAndScoreFor 

    let status =
        if (survivingTargets |> List.isEmpty) then PlayerDestroyed else PlayerSurvives

    survivingProjectiles, status, additionalExplosions, additionalScore

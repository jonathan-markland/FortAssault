﻿module Collisions

open Algorithm



[<Struct>]
type private Notepad<'explosion> =
    {
        AdditionalExplosionsList : 'explosion list
        AdditionalScore          : uint32
    }


// TODO: This would be simplified if constructing explosions and scores 
//       were separated out.  Let's return surviving and collided lists.
//       The caller can map the collided lists to explosion objects, scores and sounds itself
//       or with a helper function.

let ResultOfProjectileCollisions  
    (projectiles     : 'projectile list)
    (targets         : 'target list)
    collides
    (getProjectileId : 'projectile -> 'projectileId)
    (getTargetId     : 'target -> 'targetId)
    (explosionAndScoreForProjectile : ('projectile -> 'explosion * uint32) option)
    (explosionAndScoreForTarget : ('target -> 'explosion * uint32) option) =

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
                
                let notepad = { AdditionalExplosionsList = [] ; AdditionalScore = 0u }
                
                let folder createExplosionAndScoreFor notes item =
                    let (explosion,score) = createExplosionAndScoreFor item
                    { 
                        AdditionalExplosionsList = explosion::notes.AdditionalExplosionsList
                        AdditionalScore = notes.AdditionalScore + score 
                    }
                
                let notepad = 
                    match explosionAndScoreForProjectile with
                        | Some provider -> collidedProjectiles |> List.fold (folder provider) notepad
                        | None -> notepad

                let notepad = 
                    match explosionAndScoreForTarget with
                        | Some provider -> collidedTargets |> List.fold (folder provider) notepad
                        | None -> notepad
                
                notepad.AdditionalExplosionsList, notepad.AdditionalScore

            survivingProjectiles, survivingTargets, additionalExplosions, additionalScore



type PlayerSurvival =
    | PlayerDestroyed
    | PlayerSurvives


let ResultOfProjectileCollisionsWithSingleTarget  // TODO:  Hmmm.. Ideology:  It isn't really a generic "target" if we return a "Player"-related status type!
    (projectiles     : 'projectile list)
    (target          : 'target)
    collides
    (getProjectileId : 'projectile -> 'projectileId)
    explosionAndScoreForProjectile =

    let explosionAndScoreForTarget = fun _ -> ()

    let survivingProjectiles, survivingTargets, additionalExplosions, additionalScore =
        ResultOfProjectileCollisions
            projectiles
            [target]  // TODO:  This is slightly bad -- making a list of one thing!
            collides
            getProjectileId
            (fun _ -> 1)
            (Some explosionAndScoreForProjectile)
            None

    let status =
        if (survivingTargets |> List.isEmpty) then PlayerDestroyed else PlayerSurvives

    survivingProjectiles, status, additionalExplosions, additionalScore

module SoundEffects

type SoundEffect =  // TODO: Sound was not implemented, but in any case needs to move into ResourceFiles.fs and be done in the same way (ie: file names)
    | EchoSound
    | EngagementSirenSound
    | EnemyTorpedoLaunchSound
    | ShipExplosionSound
    | SubStageCompleteSound
    | StageCompleteSound
    | PlaneExplosionSound
    | AircraftSound // TODO: This will need cancelling abruptly when plane destroyed.
    | EnemyAircraftLaunchSound
    | EnemyMissileLaunchSound
    | SplashSound
    | BoomSound
    | AlliedShipMissileSound
    | TankFiringSound
    | TankExplosionSound
    | BossFiringSound
    | VictoryTuneSound
    | GameOverSound

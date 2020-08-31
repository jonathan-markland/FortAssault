module SoundEffects

type SoundEffect =
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

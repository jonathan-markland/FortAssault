module Rules

#if SHORT_PLAYTHROUGH
let NumShipsAtInitialEngagement = 5u  // So it's not too high to be able to test dying!
#else
let NumShipsAtInitialEngagement = 10u
#endif

let MaxPlayerNameLength = 12

let ToTankCountFromShipCount  numShips = numShips * 2u

type EnemyStrength = StrongEnemy | WeakerEnemy


module ProbabilityTable

open GhostDirectionChoosing  // for DirectionChoiceProbabilities
open MazeFilter              // for direction bitmasks



let MovementProbabilitiesFor railsBitmask facingDirection =

    if railsBitmask = 0uy then
        failwith "Cannot determine movement probabilties without a rail bitmask"

    // Dead ends - the result does not depend on facingDirection

    else if railsBitmask = MazeByteLeft  then  { ProbLeft=1uy ; ProbUp=0uy ; ProbRight=0uy ; ProbDown=0uy }
    else if railsBitmask = MazeByteUp    then  { ProbLeft=0uy ; ProbUp=1uy ; ProbRight=0uy ; ProbDown=0uy }
    else if railsBitmask = MazeByteRight then  { ProbLeft=0uy ; ProbUp=0uy ; ProbRight=1uy ; ProbDown=0uy }
    else if railsBitmask = MazeByteDown  then  { ProbLeft=0uy ; ProbUp=0uy ; ProbRight=0uy ; ProbDown=1uy }

    // 

    else failwith "incomplete"
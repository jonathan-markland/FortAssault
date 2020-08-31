module ScoreboardModel

type NewScoreboardEntry =
    {
        NewPlayerName  : string
        NewPlayerScore : uint32 
    }

type ScoreAndName =
    {
        PlayerName        : string
        PlayerScore       : uint32
        PlayerScoreString : string
    }

/// Obtain the PlayerScore field from a ScoreAndName record.
let private ScoreField {PlayerScore=s} = s

/// Generate an initial scoreboard from a list of imaginary player names,
/// and a linear score sequence.
let InitialScoreboard imaginaryNames lowestScore scoreIncrement =

    imaginaryNames |> List.mapi (fun i name -> 
        let s = lowestScore + (i |> uint32) * scoreIncrement
        { 
            PlayerName        = name
            PlayerScore       = s
            PlayerScoreString = s.ToString() 
        }
    )
        |> List.sortByDescending ScoreField

/// Returns true if the given score can enter the scoreboard
/// based on whether it equals or beats the lowest score on
/// the board.
let ScoreCanEnterBoard scoreboard score =
    
    if scoreboard = [] then
        false
    else
        let lowest = scoreboard |> List.minBy ScoreField |> ScoreField
        score >= lowest

/// Returns a new scoreboard with a score included if it beats
/// or equals the lowest score so far.  The current lowest score
/// drops off the board.
let WithNewScoreIncluded newScoreboardEntry scoreboard =

    let { NewPlayerName = name ; NewPlayerScore = score } = newScoreboardEntry

    if score |> ScoreCanEnterBoard scoreboard then

        let newScoreboardEntry = 
            {
                PlayerName        = name.Trim()
                PlayerScore       = score
                PlayerScoreString = score.ToString()
            }

        newScoreboardEntry::scoreboard
            |> List.sortBy ScoreField // so head item is the lowest score
            |> List.tail              // discard the lowest score
            |> List.sortByDescending ScoreField

    else

        scoreboard

/// Return a string with padding dots between the two
let DotsBetween (s1:string) (s2:string) lineLength =

    let l1 = s1.Length
    let l2 = s2.Length
    let total = l1 + l2 + 2  // 2 for the concatenated spaces below.

    if total > lineLength then 
        ""
    else
        let diff = lineLength - total
        s1 + " " + (String.init diff (fun _ -> ".")) + " " + s2

/// Transform scoreboard to text
let ScoreboardText lineLength scoreBoard =
    scoreBoard |> List.map (
        fun { PlayerName=name ; PlayerScoreString=scoreStr } -> 
            DotsBetween name scoreStr lineLength
        )

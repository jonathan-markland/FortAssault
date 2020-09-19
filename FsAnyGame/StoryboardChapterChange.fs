module StoryboardChapterChange

[<Struct>]
type SimpleChapterTransition<'model> =
    | StayOnThisChapter1 of newModel_1a:'model
    | GoToNextChapter1   of newModel_1b:'model

[<Struct>]
type ChapterTransitionWithGameOver<'model> =
    | StayOnThisChapter2 of newModel_2a:'model
    | GoToNextChapter2   of newModel_2b:'model
    | GameOver2          of newModel_2c:'model


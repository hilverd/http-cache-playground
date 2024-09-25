module Data.SequenceDiagramVisibility exposing (SequenceDiagramVisibility(..))


type SequenceDiagramVisibility
    = CompletelyRevealed
    | FinalInteractionsConcealedByPresenter
    | FinalInteractionsConcealedForExercise
    | FinalInteractionsRevealedForExercise

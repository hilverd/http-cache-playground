module Data.SequenceDiagramVisibility exposing (SequenceDiagramVisibility(..))


type SequenceDiagramVisibility
    = Revealed
    | FinalInteractionsConcealedButRevealable
    | FinalInteractionsPermanentlyConcealed

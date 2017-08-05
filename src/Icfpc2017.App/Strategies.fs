module Strategies

open MinimaxStrategy
open Strategy

let all =
    [("randomEdge", randomEdge);
     ("growFromMines", growFromMines);
     ("bruteForceOneStep", bruteForceOneStep);
     ("minimax", minimax)]
    |> Map.ofList

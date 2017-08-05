module Strategies

open MinimaxStrategy
open Strategy
open GreadyStrategy

let all =
    [("randomEdge", randomEdge);
     ("growFromMines", growFromMines);
     ("bruteForceOneStep", bruteForceOneStep);
     ("minimax", minimax);
     ("gready", greadyStrategy)]
    |> Map.ofList

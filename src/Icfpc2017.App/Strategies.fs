module Strategies

open MinimaxStrategy
open Strategy
open GreadyStrategy

let all =
    [("randomEdge", randomEdge);
     ("growFromMines", growFromMines);
     ("bruteForceOneStep", bruteForceOneStep);
     ("bruteForceTwoStep", bruteForceTwoStep);
     ("minimax", minimax);
     ("gready", greadyStrategy)]
    |> Map.ofList

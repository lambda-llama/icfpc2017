module Strategies

open MinimaxStrategy
open Strategy
open GreadyStrategy
open MixedStrategy

let all =
    [("randomEdge", randomEdge);
     ("growFromMines", growFromMines);
     ("bruteForceOneStep", bruteForceOneStep);
     ("bruteForceTwoStep", bruteForceTwoStep);
     ("minimax", minimax);
     ("gready", greadyStrategy);
     ("mixed", mixedStrategy)]
    |> Map.ofList

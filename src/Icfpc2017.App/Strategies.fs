module Strategies

open MinimaxStrategy
open Strategy
// open GreadyStrategy
open MixedStrategy

let all =
    [(randomEdge.name, randomEdge);
     (growFromMines.name, growFromMines);
     (bruteForce1.name, bruteForce1);
     (bruteForce3.name, bruteForce3);
     (minimax.name, minimax);
     (minimax2.name, minimax2);
     // (greadyStrategy.name, greadyStrategy);
     (mixedStrategy.name, mixedStrategy)
     (combinedForce.name, combinedForce)]
    |> Map.ofList

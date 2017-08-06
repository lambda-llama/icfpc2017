module Strategies

open BruteForceStrategy
open GrowFromMinesStrategy
open MinimaxStrategy
// open GreadyStrategy
open MixedStrategy

let all =
    [(growFromMines.name, growFromMines);
     (bruteForce1.name, bruteForce1);
     (bruteForce3.name, bruteForce3);
     (minimax.name, minimax);
     (minimax2.name, minimax2);
     // (greadyStrategy.name, greadyStrategy);
     (mixedStrategy.name, mixedStrategy)
     (combinedForce.name, combinedForce)]
    |> Map.ofList

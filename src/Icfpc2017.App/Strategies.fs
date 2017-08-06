module Strategies

open BruteForceStrategy
open GrowFromMinesStrategy
open MinimaxStrategy
open GreadyStrategy
open MixedStrategy

let greadyBrute =
    Strategy.mixSlowFastTimeout "greadyBrute" 300 bruteForce3 greadyStrategy

let all =
    [(growFromMines.name, growFromMines);
     (bruteForce1.name, bruteForce1);
     (bruteForce3.name, bruteForce3);
     (minimax.name, minimax);
     (minimax2.name, minimax2);
     (greadyStrategy.name, greadyStrategy);
     (greadyStrategyWithHeur.name, greadyStrategyWithHeur);
     (mixedStrategy.name, mixedStrategy)
     (combinedForce.name, combinedForce)
     (greadyBrute.name, greadyBrute)]
    |> Map.ofList

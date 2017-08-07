module Strategies

open BruteForceStrategy
open GrowFromMinesStrategy
open MinimaxStrategy
open GreadyStrategy
open MixedStrategy

let greedyBrute =
    Strategy.mixSlowFastTimeout "greedyBrute" 300 bruteForce3 greedyStrategy

let all =
    [(growFromMines.name, growFromMines);
     (bruteForce1.name, bruteForce1);
     (bruteForce3.name, bruteForce3);
     (minimax.name, minimax);
     (minimax2.name, minimax2);
     (greedyStrategy.name, greedyStrategy);
     (greedyStrategyWithHeur.name, greedyStrategyWithHeur);
     (greedyStrategyWithHeur2.name, greedyStrategyWithHeur2);
     (greedyRandomStrategy.name, greedyRandomStrategy);
     (mixedStrategy.name, mixedStrategy)
     (combinedForce.name, combinedForce)
     (greedyBrute.name, greedyBrute)]
    |> Map.ofList

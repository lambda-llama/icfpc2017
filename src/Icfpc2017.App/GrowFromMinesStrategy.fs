module GrowFromMinesStrategy

open Graphs

let growFromMines =
    Strategy.withSetup "growFromMines" Traversal.shortestPaths (fun distances {Game.Graph=graph; Game.Me=me} ->
        let sources = Graph.sources graph in
        let isOurEdge = Graph.isClaimedBy me graph

        let attachedToMine edge = Seq.exists (Edge.contains edge) sources in
        let attachedToOurEdge v = Graph.adjacentEdges graph v |> Seq.exists isOurEdge in

        let weight (edge) =
            let (u, v) = Edge.ends edge
            let uIsOurs = attachedToOurEdge u
            let vIsOurs = attachedToOurEdge v
            if uIsOurs || vIsOurs
            then
                let next = if vIsOurs then u else v in
                distances
                |> Map.toSeq
                |> Seq.map (fun (_, ds) -> ds.[int next])
                |> Seq.min
            else if attachedToMine edge then 1
            else 0
        in

        let edge =
            if not (Seq.isEmpty <| Graph.claimedBy graph me)
            then Graph.unclaimed graph |> Seq.maxBy weight
            else
                let mine =
                    distances |> Map.toSeq
                    |> Seq.map (fun (mine, ds) ->
                        (mine, Array.fold (fun acc x -> acc + x*x) 0 ds))
                    |> Seq.maxBy (fun (_, distance) -> distance)
                    |> fst
                in

                Graph.adjacentEdges graph mine
                (* TODO: pick the most remote one. *)
                |> Seq.find (fun _ -> true)
        (edge, Map.empty)
    )

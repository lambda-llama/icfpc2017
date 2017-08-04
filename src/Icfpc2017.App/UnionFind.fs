namespace UnionFind

// using array from 1..N 
// don't use .[0]

type Node = { Parent:int ; Rank:int}

type Partition (N:int) = 
    let nodearray = [| for i in 0..N do yield {Parent = i; Rank = 0} |]

    let length = (Array.length nodearray) - 1

    member p.Item 
        with get(idx) = nodearray.[idx]

    member p.find (i) = 
        let mutable tmp0 = i
        let mutable tmp1 = p.[i].Parent
        while (tmp0 <> tmp1) do tmp0 <- tmp1
                                tmp1 <- p.[tmp1].Parent
        tmp0
    member p.find_compress (i) :int =
        let mutable tmp0 = i
        let mutable tmp1 = p.[i].Parent
        let rec one_step ok list =
            match ok with 
              | false -> list
              | true ->   tmp0 <- tmp1
                          tmp1 <- p.[tmp1].Parent
                          one_step (tmp0<>tmp1) (tmp0::list) 

        let list = one_step (tmp0<>tmp1) [i]  
        // ranks stay the same with find_compress
        list |> List.head |> (fun i ->  let r = nodearray.[i].Rank
                                        (nodearray.[i] <- {Parent = tmp0 ; Rank=r} ))
        list |> List.tail |> List.iter ( fun i -> let r = nodearray.[i].Rank
                                                  (nodearray.[i] <- {Parent = tmp0 ; Rank=r} ))

        tmp0

    member p.union_by_rank (i,j) : bool = // returns false if i and j hav the same parent, true otherwise
       if i=j then false else
          let x = p.find_compress(i)
          let y = p.find_compress(j)
          let rx = p.[x].Rank 
          let ry = p.[y].Rank

          if x=y then false else
             if (rx<ry) then nodearray.[x] <- {Parent = y; Rank = rx}                         
             if (ry<rx) then nodearray.[y] <- {Parent = x; Rank = ry}                
             if (rx=ry) then nodearray.[x] <- {Parent = y; Rank = ry}
                             nodearray.[y] <- {Parent = y; Rank = ry+1}
             true                             

    member p.print() = 
       printfn "%A" nodearray

    member p.output() = [|for i in 0..length do yield (i,p.find_compress(i))|]
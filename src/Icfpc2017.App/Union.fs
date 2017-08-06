module Union

type T = {
    Sources: int array
    Sites: int array
}

let getOneSideScore (u1: T) (u2: T) (dist: Map<int, int[]>) = 
    let mutable sum = 0 in
    for m in u1.Sources do
        for s in u2.Sources do
            let d = dist.[m].[int s] in
            sum <- sum + d * d
    sum

let getScore union1 union2 dist =
    getOneSideScore union1 union2 dist + getOneSideScore union2 union1 dist
 
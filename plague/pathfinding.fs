module Pathfinding

open Config
open Node

let isWall(intendedMove: int*int, world: Node[,]): bool =
  let node = Array2D.get world (fst intendedMove) (snd intendedMove)
  node.isPassable

let distance(a: int*int, b: int*int): int*int =
  let (ax, ay) = a
  let (bx, by) = b
  let distX = abs (ax - bx)
  let distY = abs (ay - by)
  (distX, distY)

let pythogora(a: int*int, b: int*int): float =
  let (ax, ay) = a
  let (bx, by) = b
  let xExp = float(pown (ax - bx) 2)
  let yExp = float(pown (ay - by) 2)
  sqrt(xExp + yExp)

let shortestPath(world: Node[,], startPos: int*int, endPos: int*int): List<int*int> =
  let rec recur(startPoint: int*int, endPoint: int*int, acc: List<int*int>): List<int*int> =
    let (startX, startY) = startPoint
    let (endX, endY) = endPoint
    if startX = endX && startY = endY then acc |> List.rev
    else
      let possibleRoute = [(startX+1, startY); (startX-1, startY); (startX, startY+1); (startX, startY-1)]
                            |> List.filter (fun m -> isWall(m, world))
      if (possibleRoute |> List.isEmpty) then acc
      else
        let best = possibleRoute |> List.minBy (fun x -> distance(x, endPoint))
        recur(best, endPoint, best::acc)
  recur(startPos, endPos, [])

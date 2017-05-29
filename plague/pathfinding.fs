module Pathfinding

open Logger
open Config
open Node

let isSamePos(x: int, y: int, playerPos: int*int): bool = x = (fst playerPos) && y = (snd playerPos)

let isWall(intendedMove: int*int, world: Node[,]): bool =
  let node = Array2D.get world (fst intendedMove) (snd intendedMove)
  node.isPassable

let distance(a: int*int, b: int*int): int*int =
  let (ax, ay) = a
  let (bx, by) = b
  let distX = abs (ax - bx)
  let distY = abs (ay - by)
  (distX, distY)

let pythagora(a: int*int, b: int*int): float =
  let (ax, ay) = a
  let (bx, by) = b
  let xExp = float(pown (ax - bx) 2)
  let yExp = float(pown (ay - by) 2)
  sqrt(xExp + yExp)

let isNotAlreadyVisited(node: int*int, list: List<int*int>): bool =
  list.IsEmpty || not (list |> List.exists ((=) node))

let shortestPath(startPos: int*int, endPos: int*int, world: Node[,]): List<int*int> =
  let rec recur(startPoint: int*int, endPoint: int*int, weight: float, visited: List<int*int>, acc: List<int*int>): List<int*int> =
    let (startX, startY) = startPoint
    let (endX, endY) = endPoint
    if isSamePos(startX, startY, endPoint) then acc |> List.rev
    else
      let possibleRoute = [(startX+1, startY); (startX-1, startY); (startX, startY+1); (startX, startY-1)]
                            |> List.filter (fun m -> isWall(m, world))
                            |> List.filter (fun n -> isNotAlreadyVisited(n, visited))
      if (possibleRoute |> List.isEmpty) then failwith "No routes"  //Failed to resolve path
      else
        let best = possibleRoute |> List.minBy (fun start -> pythagora(start, endPoint))
        let newWeight = pythagora(distance(best, endPoint), endPoint)
        let accHead = if acc.IsEmpty then (startX, startY) else acc.Head
        logger.info (sprintf "route: %A" acc)
        recur(best, endPoint, weight+newWeight, best::visited, best::acc)

  recur(startPos, endPos, 0.0, [], [])

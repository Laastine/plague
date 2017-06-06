module Pathfinding

open Logger
open Config
open Node

let isSamePos(x: int, y: int, playerPos: int*int): bool = x = (fst playerPos) && y = (snd playerPos)

let isNotWall(intendedMove: int*int, world: Node[,]): bool =
  let (x, y) = intendedMove
  x >= 0 && y >= 0 && x < worldY && y < worldX && (Array2D.get world x y).isPassable

let distance(a: int*int, b: int*int): float =
  let (ax, ay) = a
  let (bx, by) = b
  let distX = abs (ax - bx)
  let distY = abs (ay - by)
  float(distX + distY)

let distanceXY(a: int*int, b: int*int): int*int =
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

let isNotAlreadyVisited(node: int*int, list: List<int*int>): bool = not (list |> List.exists ((=) node))

type GridNode = struct
  val pos: int*int
  val score: float
  val heuristic: float
  val parent: int*int
  val value: float
  new (pos, parent, score, heuristic, value) =
    {pos = pos; parent = parent; score = score; heuristic = heuristic; value = value;}
end

let gridNodeToPosList(inputList: List<GridNode>): List<int*int> =
  let rec recur(inputList: List<GridNode>, acc: List<int*int>) =
    match inputList with
    | [] -> acc |> List.rev
    | head::tail -> recur(tail, (head.pos)::acc)
  recur(inputList, [])

let posToGridNode(node: int*int, parent: int*int, head: GridNode, endPoint: int*int): GridNode =
  let heuristic = distance(head.pos, endPoint)
  let score = head.score + 1.0
  let value = score + heuristic
  GridNode(node, parent, score, heuristic, value)

let reconstructRoute(route: List<GridNode>, startPoint: int*int, endPoint: int*int): List<int*int> =
  let rec recur(routePath: List<GridNode>, acc: List<int*int>): List<int*int> =
    match routePath with
    | [] -> acc.Tail
    | head::tail ->
                    if head.pos = startPoint then acc
                    else recur(tail, (head.parent)::acc)
  recur(route, [])

let shortestPath(startPos: int*int, endPos: int*int, world: Node[,]): List<int*int> =
  let value = distance(startPos, endPos)+1.0 + distance(startPos, endPos)
  let openList = [GridNode(startPos, startPos, distance(startPos, endPos)+1.0, distance(startPos, endPos), value)]
  let closedList = []

  let rec recur(startPoint: int*int, endPoint: int*int, visited: List<GridNode>, openList: List<GridNode>, acc: List<GridNode>): List<int*int> =
    match openList with
    | [] -> gridNodeToPosList(openList)
    | openList ->
            let head = openList |> List.minBy (fun x -> x.value)
            let newVisitedList = head::visited
            let (startX, startY) = head.pos
            if isSamePos(startX, startY, endPoint) then reconstructRoute(head::acc, startPoint, endPoint)
            else
              let possibleRoutes = [(startX+1, startY); (startX-1, startY); (startX, startY+1); (startX, startY-1)]
                                    |> List.filter (fun n -> isNotAlreadyVisited(n, gridNodeToPosList(visited)) && isNotWall(n, world))
                                    |> List.map (fun n -> posToGridNode(n, head.pos, head, endPos))
              let newScore = if possibleRoutes.IsEmpty then [] else [possibleRoutes |> List.minBy (fun r -> r.value)]
              let tail = openList |> List.filter ((<>) head)
              if not newScore.IsEmpty &&
                (head.value + 1.0) >= newScore.Head.value &&
                isNotAlreadyVisited(head.pos, gridNodeToPosList(visited)) then
                recur(startPos, endPos, newVisitedList, List.concat(seq [possibleRoutes; tail]), newScore.Head::acc)
              else
                recur(startPos, endPos, newVisitedList, List.concat(seq [possibleRoutes; tail]), acc)
  recur(startPos, endPos, closedList, openList, [])
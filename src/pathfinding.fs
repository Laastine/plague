module Pathfinding

open Logger
open Config
open Node
open System.Collections.Generic

type Cost =
  | Blocked
  | Cost of cost:int

type GridNode = {
  x: int
  y: int
  cost: Cost
}

type Graph = {
  nodes: Dictionary<(int * int), GridNode>
  parent: Dictionary<GridNode, GridNode>
  pathCost: Dictionary<GridNode, int>
  path: list<GridNode>
}

let isSamePos(pos: int*int, inputList: list<(int*int)>): bool =
  inputList |> List.exists (fun e -> (fst e) = (fst pos) && (snd e) = (snd pos))

let isNotWall(intendedMove: int*int, world: Node[,]): bool =
  let (x, y) = intendedMove
  x >= 0 && y >= 0 && x < worldY && y < worldX && (Array2D.get world x y).isPassable

let distanceXY(a: int*int, b: int*int): int =
  let distX = abs ((fst a) - (fst b))
  let distY = abs ((snd a) - (snd b))
  distX + distY

let positionVector(a: int*int, b: int*int): int*int =
  let distX = abs ((fst a) - (fst b))
  let distY = abs ((snd a) - (snd b))
  (distX, distY)

let pythagora(a: int*int, b: int*int): float =
  let xExp = float(pown ((fst a) - (fst b)) 2)
  let yExp = float(pown ((snd a) - (snd b)) 2)
  sqrt(xExp + yExp)

let directions = [{ x = 1; y = 0; cost = Blocked }; { x = 0; y = -1; cost = Blocked }; {x = -1; y = 0; cost = Blocked }; {x = 0; y = 1; cost = Blocked }]

let cost(nodeB: GridNode): int =
  match nodeB.cost with
  | Cost(cost) -> cost
  | Blocked -> failwith (sprintf "No cost defined on %A" nodeB)

let childNodes(graph: Graph, node: GridNode): list<GridNode> =
  directions
    |> List.map (fun direction ->
                        let neighborX = node.x + direction.x
                        let neighborY = node.y + direction.y
                        let (neighborExists, child) = graph.nodes.TryGetValue((neighborX, neighborY))
                        if neighborExists && child.cost <> Blocked then Some child
                        else None)
    |> List.choose id

let mutable(frontier: list<(GridNode * int)>) = []

let enqueue(head: (GridNode * int)) = frontier <- head::frontier

let dequeue() =
  let sortedQueue = frontier |> List.sortBy (fun (_, prio) -> prio)
  frontier <- sortedQueue.Tail
  sortedQueue.Head

let shortestPath(graph: Graph, startPoint: GridNode, endPoint: GridNode, heuristic) =
  enqueue(startPoint, cost(startPoint))
  graph.parent.[startPoint] <- startPoint
  graph.pathCost.[startPoint] <- cost(startPoint)

  let mutable isFound = false
  let arbitraryBigNumber = 1000
  while frontier.Length > 0 && (not isFound) do
    let (head, _) = dequeue()
    if head = endPoint then isFound <- true
    if (not isFound) then
      childNodes(graph, head)
        |> List.iter (fun child ->
          logger.flush()
          let (isFound, headVal) = graph.pathCost.TryGetValue(head)
          let newCost =
            if isFound then
              headVal + cost(child)
            else arbitraryBigNumber + cost(child)
          if (not (graph.pathCost.ContainsKey(child))) || newCost < graph.pathCost.[child] then
            graph.pathCost.[child] <- newCost
            let prio = newCost + heuristic((child.x, child.y), (endPoint.x, endPoint.y))
            enqueue(child, prio)
            graph.parent.[child] <- head)
  graph

let reconstructPath(parent : Dictionary<GridNode, GridNode>, startPoint: GridNode, endPoint: GridNode): list<GridNode> =
  let mutable head = endPoint
  let mutable path = [head]
  while head <> startPoint do
    head <- parent.[head]
    path <- head::path
  path

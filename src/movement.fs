module Movement

open Config
open Logger
open Node
open Pathfinding
open System.Collections.Generic

let isWorldsEdge(input: int, isVerticalAxel: bool): bool =
  if isVerticalAxel then input > -1 && input < worldX
  else input > -1 && input <= (worldY-1)

let movementInput(keyChar: char, playerPos: int * int, world: Node[,]): int*int =
  let (posX, posY) = playerPos
  match keyChar with
    | 'a' ->
              let intendedMove = (posX, (posY-1))
              if isWorldsEdge((posY-1), true) && isNotWall(intendedMove, world) then intendedMove
              else playerPos
    | 'd' ->
              let intendedMove = ((posX), (posY+1))
              if isWorldsEdge((posY+1), true) && isNotWall(intendedMove, world) then intendedMove
              else playerPos
    | 'w' ->
              let intendedMove = ((posX-1), posY)
              if isWorldsEdge(posX-1, false) && isNotWall(intendedMove, world) then intendedMove
              else playerPos
    | 's' ->
              let intendedMove = ((posX+1), (posY))
              if isWorldsEdge(posX+1, false) && isNotWall(intendedMove, world) then intendedMove
              else playerPos
    | 'q' ->
            logger.info "Exit Plague"
            logger.close()
            exit(0)
    | _ ->
            logger.info "Unknown key pressed"
            ((fst playerPos), (snd playerPos))

let convertNodesToGrap(world: Node[,]) =
  let graphNodes = Dictionary<(int*int), GridNode>()
  world |> Array2D.iteri (fun x y idx ->
    if isNotWall((x,y), world) then graphNodes.Add((x,y), {x=x; y=y; cost=Cost(1)}) |> ignore
    else graphNodes.Add((x,y), {x=x; y=y; cost=Blocked}) |> ignore) |> ignore
  {
    nodes = graphNodes
    parent = Dictionary<GridNode, GridNode>()
    pathCost = Dictionary<GridNode, int>()
    path = []
  }

let moveMonster(playerPos: int*int, monsterPos: int*int, world: Node[,]): int*int =
  let graphWorld = convertNodesToGrap(world)
  let startPoint = {x=(fst monsterPos); y=(snd monsterPos); cost=Cost(1)}
  let endPoint = {x=(fst playerPos); y=(snd playerPos); cost=Cost(1)}
  let graph = shortestPath(graphWorld, startPoint, endPoint, distanceXY)
  let path = reconstructPath(graph.parent, startPoint, endPoint) |> List.map(fun e -> (e.x, e.y)) |> List.tail
  if path |> List.isEmpty then monsterPos
  else path.Head

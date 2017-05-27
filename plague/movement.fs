module Movement

open Config
open Logger
open Node
open Pathfinding

let isWorldsEdge(input: int, isVerticalAxel: bool): bool =
  if isVerticalAxel then input > -1 && input < worldX
  else input > -1 && input <= (worldY-1)

let movementInput(keyChar: char, playerPos: int * int, world: Node[,]): int*int =
  let (posX, posY) = playerPos
  match keyChar with
    | 'a' ->
              let intendedMove = (posX, (posY-1))
              if isWorldsEdge((posY-1), true) && isWall(intendedMove, world) then intendedMove
              else playerPos
    | 'd' ->
              let intendedMove = ((posX), (posY+1))
              if isWorldsEdge((posY+1), true) && isWall(intendedMove, world) then intendedMove
              else playerPos
    | 'w' ->
              let intendedMove = ((posX-1), posY)
              if isWorldsEdge(posX-1, false) && isWall(intendedMove, world) then intendedMove
              else playerPos
    | 's' ->
              let intendedMove = ((posX+1), (posY))
              if isWorldsEdge(posX+1, false) && isWall(intendedMove, world) then intendedMove
              else playerPos
    | 'q' ->
            logger.info "Exit Plague"
            logger.close()
            exit(0)
    | _ ->
            logger.info "Unknown key pressed"
            ((fst playerPos), (snd playerPos))

let moveMonster(playerPos: int * int, monsterPos: int*int, world: Node[,]): int*int =
  let path = shortestPath(monsterPos, playerPos, world)
  if path |> List.isEmpty then monsterPos
  else path |> List.head

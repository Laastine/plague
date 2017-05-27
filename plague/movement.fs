module Movement

open Config
open LogAgent
open Node

let isWorldsEdge(input: int, isVerticalAxel: bool): bool =
  if isVerticalAxel then input > -1 && input < worldX
  else input > -1 && input <= (worldY-1)

let isWall(intendedMove: int*int, world: Node[,]): bool =
  let node = Array2D.get world (fst intendedMove) (snd intendedMove)
  node.isPassable

let movementInput(keyChar: char, playerPos: int * int, world: Node[,]) =
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
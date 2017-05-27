module Plague

open System
open LogAgent
open Node

let worldX = 60
let worldY = 30

let initialPlayerPos = (28, 58)

let createHouse(posX:int , posY:int, size: int, world: Node[,]) =
  
  let lowerX = posX - 1;
  let upperX = posX + size;
  
  let lowerY = posY - 1;
  let upperY = posY + size;

  Array2D.mapi( fun x y idx ->
    if x = lowerX && (y >= lowerY && y <= upperY) then new Node((x,y), "#", false)
    elif x = upperX && (y >= lowerY && y <> (lowerY + 2) && y <= upperY) then new Node((x,y), "#", false)
    elif (x >= lowerX && x <= upperX) && (y = lowerY || y = upperY) then new Node((x,y), "#", false)
    else new Node((x,y), ".", true)
  ) world

let initWorldArray = 
  createHouse(3, 3, 5, Array2D.init worldY worldX (fun x y -> new Node((x,y), ".", true)))

let logger = new LogAgent(@"./log.txt")

let renderWorld(world: Node[,], playerPos: int*int) =
  world
    |> Array2D.mapi (fun x y idx ->
      if x = (fst playerPos) && y = (snd playerPos)
      then printf "@" elif (y+1) % worldX = 0
      then printfn "%s" idx.value
      else printf "%s" idx.value)

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

let render(initWorldArray, playerPos: int*int) =
  System.Console.Clear()
  renderWorld(initWorldArray, playerPos) |> ignore

let rec inputHandler(playerPos: int*int) =
  render(initWorldArray, playerPos)
  let key = Console.ReadKey().KeyChar
  logger.info (sprintf "Key: %c" key)
  let newPlayerPos = movementInput(key, playerPos, initWorldArray)
  render(initWorldArray, newPlayerPos)
  logger.flush()
  inputHandler newPlayerPos

[<EntryPoint>]
let main argv =
  logger.info "Plague started"
  inputHandler initialPlayerPos
  0
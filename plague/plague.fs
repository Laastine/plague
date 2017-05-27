module Plague

open System
open LogAgent
open Node

let worldX = 60
let worldY = 30

let initialPlayerPos = (28, 58)

let initWorldArray = Array2D.init worldY worldX (fun x y -> new Node((x,y), ".", true))

let logger = new LogAgent(@"./log.txt")

let renderWorld(world: Node[,], playerPos: int*int) =
  world
    |> Array2D.mapi (fun x y idx ->
      if x = (fst playerPos) && y = (snd playerPos)
      then printf "@" elif (y+1) % worldX = 0
      then printfn "%s" idx.value
      else printf "%s" idx.value)

let isLegalMove(input: int, isVerticalAxel: bool): bool =
  if isVerticalAxel then input > -1 && input < worldX
  else input > -1 && input <= (worldY-1)

let movementInput(keyChar: char, playerPos: int * int) =
  let (posX, posY) = playerPos
  match keyChar with
    | 'a' ->
              if isLegalMove((posY-1), true) then (posX, (posY-1))
              else playerPos
    | 'd' ->
              if isLegalMove((posY+1), true) then ((posX), (posY+1))
              else playerPos
    | 'w' ->
              if isLegalMove(posX-1, false) then ((posX-1), posY)
              else playerPos
    | 's' ->
              if isLegalMove(posX+1, false) then ((posX+1), (posY))
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
  let newPlayerPos = movementInput(key, playerPos)
  render(initWorldArray, newPlayerPos)
  logger.flush()
  inputHandler newPlayerPos

[<EntryPoint>]
let main argv =
  logger.info "Plague started"
  inputHandler initialPlayerPos
  0

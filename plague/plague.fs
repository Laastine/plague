module Plague

open System

open Config
open LogAgent
open Node
open Movement

let createHouse(posX:int , posY:int, size: int, world: Node[,]) =

  let lowerX = posX - 1;
  let upperX = posX + size;

  let lowerY = posY - 1;
  let upperY = posY + size;

  Array2D.mapi( fun x y idx ->
    if x = lowerX && (y >= lowerY && y <= upperY) then Node((x,y), "#", false)
    elif x = upperX && (y >= lowerY && y <> (lowerY + 2) && y <= upperY) then Node((x,y), "#", false)
    elif (x >= lowerX && x <= upperX) && (y = lowerY || y = upperY) then Node((x,y), "#", false)
    else Node((x,y), ".", true)
  ) world

let initWorldArray =
  createHouse(3, 3, 5, Array2D.init worldY worldX (fun x y -> Node((x,y), ".", true)))

let renderWorld(world: Node[,], playerPos: int*int) =
  world
    |> Array2D.mapi (fun x y idx ->
      if x = (fst playerPos) && y = (snd playerPos)
      then printf "@" elif (y+1) % worldX = 0
      then printfn "%s" idx.value
      else printf "%s" idx.value)

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
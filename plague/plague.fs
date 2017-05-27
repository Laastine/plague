module Plague

open System

open Config
open LogAgent
open Node
open Movement

let createHouse(posX: int , posY: int, size: int) (world: Node[,]: Node[,]) =
  let lowerX = posX - 1
  let upperX = posX + size

  let lowerY = posY - 1
  let upperY = posY + size

  Array2D.mapi( fun x y idx ->
    if x = lowerX && (y >= lowerY && y <= upperY) then Node((x,y), "#", false)
    elif x = upperX && (y >= lowerY && y <> (lowerY + 2) && y <= upperY) then Node((x,y), "#", false)
    elif (x >= lowerX && x <= upperX) && (y = lowerY || y = upperY) then Node((x,y), "#", false)
    else Array2D.get world x y
  ) world

let initWorldArray: Node[,] =
  let world = Array2D.init worldY worldX (fun x y -> Node((x,y), ".", true))
  world
    |> (fun h -> (createHouse(15, 15, 2)(h)))
    |> (fun h -> (createHouse(5, 5, 4)(h)))
    |> (fun h -> (createHouse(25, 25, 4)(h)))
    |> (fun h -> (createHouse(7, 35, 6)(h)))

let renderWorld(world: Node[,], playerPos: int*int) =
  System.Console.Clear()
  let isEdge(y: int): bool = (y+1) % worldX = 0
  let isSamePos(x: int, y: int, playerPos: int*int): bool = x = (fst playerPos) && y = (snd playerPos)
  world
    |> Array2D.mapi (fun x y idx ->
      if isSamePos(x, y, playerPos) && isEdge(y) then printfn "@"
      elif isSamePos(x, y, playerPos) && not (isEdge(y)) then printf "@"
      elif isEdge(y) then printfn "%s" idx.value
      else printf "%s" idx.value)
     |> ignore

let rec inputHandler(playerPos: int*int) =
  renderWorld(initWorldArray, playerPos)
  let key = Console.ReadKey().KeyChar
  logger.info (sprintf "Key: %c %A" key playerPos)
  let newPlayerPos = movementInput(key, playerPos, initWorldArray)
  renderWorld(initWorldArray, newPlayerPos)
  logger.flush()
  inputHandler newPlayerPos

[<EntryPoint>]
let main argv =
  logger.info "Plague started"
  inputHandler initialPlayerPos
  0
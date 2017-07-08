module Plague

open System

open Config
open Logger
open Node
open Movement
open Pathfinding

let createHouse(posX: int , posY: int, size: int)(world: Node[,]: Node[,]) =
  let lowerX = posX - 1
  let upperX = posX + size

  let lowerY = posY - 1
  let upperY = posY + size

  Array2D.mapi( fun x y idx ->
    if x = lowerX && (y >= lowerY && y <= upperY) then Node((x,y), setColorizedText("#", Color.ColorBrown), false)
    elif x = upperX && (y >= lowerY && y <> (lowerY + 2) && y <= upperY) then Node((x,y), setColorizedText("#", Color.ColorBrown), false)
    elif (x >= lowerX && x <= upperX) && (y = lowerY || y = upperY) then Node((x,y), setColorizedText("#", Color.ColorBrown), false)
    else Array2D.get world x y
  ) world

let createPond(posX: int, posY: int, size: int)(world: Node[,]: Node[,]) =
  let isWithinCircle(x,y) = pythagora(positionVector((posX, posY), (x,y)), (x,y)) <= float (size)
  Array2D.mapi(fun x y i ->
                if isWithinCircle(x,y) then Node((x,y), setColorizedText("~", Color.ColorBlue), false)
                else Array2D.get world x y) world

let initWorldArray =
  let world = Array2D.init worldY worldX (fun x y -> Node((x,y), setColorizedText(".", Color.ColorGreen), true))
  world
    |> (fun h -> (createHouse(15, 15, 2)(h)))
    |> (fun h -> (createHouse(5, 5, 4)(h)))
    |> (fun h -> (createHouse(23, 25, 4)(h)))
    |> (fun h -> (createHouse(7, 35, 6)(h)))
    |> (fun w -> (createPond(42, 15, 8)(w)))

let initMonsters: List<int*int> =
  let rnd = System.Random()
  List.init 1 (fun _ -> (rnd.Next(1, 4), rnd.Next(1, 4)))

let renderWorld(world: Node[,], playerPos: int*int, monsterPos: (int*int) list) =
  System.Console.Clear()
  let isEdge(y: int): bool = (y+1) % worldX = 0
  world
    |> Array2D.mapi (fun x y idx ->
      if isSamePos((x, y), monsterPos) && isEdge(y) then printf "X"
      elif isSamePos((x, y), monsterPos) && not (isEdge(y)) then printf "X"
      elif isSamePos((x, y), [playerPos]) && isEdge(y) then printfn "@"
      elif isSamePos((x, y), [playerPos]) && not (isEdge(y)) then printf "@"
      elif isEdge(y) then printfn "%s" idx.value
      else printf "%s" idx.value)
     |> ignore

let rec inputHandler(playerPos: int*int, monsterPos: (int*int) list) =
  renderWorld(initWorldArray, playerPos, monsterPos)
  let key = Console.ReadKey().KeyChar
  logger.info (sprintf "Key: %c %A" key playerPos)
  let newPlayerPos = movementInput(key, playerPos, initWorldArray)
  let newMonsterPos = monsterPos |> List.map (fun e -> moveMonster(newPlayerPos, e, initWorldArray))
  logger.info (sprintf "foo: %A" newMonsterPos)
  renderWorld(initWorldArray, newPlayerPos, newMonsterPos)
  if newPlayerPos = newMonsterPos.Head then printfn "Game over"
  else
    logger.info (sprintf "player: %A->%A, monster: %A->%A" playerPos newPlayerPos monsterPos newMonsterPos)
    logger.flush()
    inputHandler(newPlayerPos, newMonsterPos)

[<EntryPoint>]
let main argv =
  logger.info "Plague started"
  inputHandler(initialPlayerPos, initMonsters)
  0

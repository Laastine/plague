module plague

open System
open LogAgent

let worldX = 30
let worldY = 60

let initialPlayerPos = (5,5)

let initWorldArray = Array2D.init worldX worldY (fun x y -> ".")

let logger = new LogAgent(@"./log.txt")

let renderWorld(world, playerPos) =
  Array2D.mapi (fun x y idx ->
    if x = (fst playerPos) && y = (snd playerPos)
    then printf "@" elif (y+1) % worldY = 0
    then printfn "%s" idx
    else printf "%s" idx) world


let movementInput(keyChar: char, playerPos: int * int) =
  match keyChar with
    | 'w' -> ((fst playerPos)-1, (snd playerPos))
    | 's' -> ((fst playerPos)+1, (snd playerPos))
    | 'a' -> ((fst playerPos), (snd playerPos)-1)
    | 'd' -> ((fst playerPos), (snd playerPos)+1)
    | 'q' -> exit(0)
    | _ -> ((fst playerPos), (snd playerPos))

let render(initWorldArray, playerPos: int*int) =
  System.Console.Clear()
  renderWorld(initWorldArray, playerPos) |> ignore

let rec inputHandler playerPos =
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

module Config

let worldX = 60
let worldY = 30

let initialPlayerPos = (10, 20)

let monsterPos = (1,1)

[<AbstractClass; Sealed>]
type Color =
  static member ColorRed = "\x1b[31m"
  static member ColorGreen = "\x1b[32m"
  static member ColorYellow = "\x1b[33m"
  static member ColorBlue = "\x1b[34m"
  static member ColorMagneta = "\x1b[35m"
  static member ColorBrown = "\x1b[38;2;139;69;19m"
  static member ColorReset = "\x1b[0m"
let  setColorizedText(text: string, color: string) =
    color + text + Color.ColorReset
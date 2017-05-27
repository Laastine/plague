module Config

let worldX = 60
let worldY = 30

let initialPlayerPos = (10, 20)

let ColorRED = "\x1b[31m"
let ColorGREEN = "\x1b[32m"
let ColorYELLOW = "\x1b[33m"
let ColorBLUE = "\x1b[34m"
let ColorMAGENTA = "\x1b[35m"
let ColorBROWN = "\x1b[38;2;139;69;19m"
let ColorRESET = "\x1b[0m"


let setTextColor(text: string, color: string) =
  color + text + "\x1b[0m"
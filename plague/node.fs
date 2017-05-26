module Node

type Node = struct
  val value: string
  val pos: int*int
  val isPassable: bool
  new (pos, value, isPassable) =
    {pos = pos; value = value; isPassable = isPassable;}
end

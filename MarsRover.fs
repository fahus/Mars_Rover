module MarsRover
open System  


//Domain Modeling 

type Direction = 
| North 
| East 
| South
| West 

type Location = {
    X: int
    Y: int 
}

type RoverPosition = {
    Location: Location
    Direction: Direction 
}

type Command = 
| TurnLeft 
| TurnRight 
| MoveForward


let step direction =
    match direction with
    | North -> {X=0;Y = -1}
    | East -> {X=1;Y=0}
    | South -> {X=0;Y=1}
    | West -> {X = -1;Y=0}

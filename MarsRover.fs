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
| M
| L
| R


let step direction (location:Location)  =
    match direction with
    | North -> {location with Y = location.Y + 1 }
    | East -> {location with X = location.X + 1 }
    | South -> {location with Y = location.Y - 1 }
    | West -> {location with X = location.X - 1 }



let rightOf direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let leftOf direction =
    match direction with
    | North -> West
    | West -> South
    | South -> East
    | East -> North


let turnLeft rover = {rover with Direction=leftOf rover.Direction}
let turnRight rover = {rover with Direction=rightOf rover.Direction} 

let moveForward rover = {rover with Location= step rover.Direction rover.Location}

let interpretCommand command = 
    match command with 
    | L -> turnLeft
    | R -> turnRight
    | M -> moveForward

let listOfCommands:Command list = [L;M;L;M;L;M;L;M;M ]

let myRoverPosition  =  { Location = {X = 1; Y = 2} ; Direction = North}



let listOfRovers = [
    (listOfCommands, myRoverPosition)
    ([M;M;R;M;M;R;M;R;R;M],{ Location = {X = 3; Y = 3} ; Direction = East} )
    ]


let folder (roverPosition:RoverPosition) (command: Command):RoverPosition= 
    interpretCommand command roverPosition 


let foldRover roverPosition commands = 
    List.fold folder roverPosition commands

let result = 
    listOfRovers
    |> List.map( fun (lcmds, myRP) -> foldRover myRP lcmds )


module MarsRover 

open System


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


let step direction (location:Location) (upperRight:Location)  =
    match direction with
    | North -> {location with Y = if location.Y = upperRight.Y then location.Y else location.Y + 1 }
    | East -> { location with X = if location.X = upperRight.X then location.X else location.X + 1 }
    | South -> { location with Y = if location.Y = upperRight.Y then location.Y else location.Y - 1 }
    | West -> {location with X = if location.X = upperRight.X then location.X else location.Y - 1 }



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

let moveForward (upperRight:Location) rover = {rover with Location= step rover.Direction rover.Location upperRight }

let interpretCommand (upperRight:Location) command  = 
    match command with 
    | L -> turnLeft
    | R -> turnRight
    | M -> moveForward upperRight

let listOfCommands:Command list = [L;M;L;M;L;M;L;M;M ]

let myRoverPosition  =  { Location = {X = 1; Y = 2} ; Direction = North}



let listOfRovers = [
    (listOfCommands, myRoverPosition)
    ([M;M;R;M;M;R;M;R;R;M],{ Location = {X = 3; Y = 3} ; Direction = East} )
    ([M;M;M;M;M;M],{ Location = {X = 2; Y = 4} ; Direction = North} )
    ([M;M;M;M;M;M],{ Location = {X = 2; Y = 4} ; Direction = South} )
    ] 


let folder (upperRight:Location)(roverPosition:RoverPosition) (command: Command):RoverPosition = 
    interpretCommand upperRight command roverPosition 


let foldRover (upperRight:Location)  roverPosition commands = 
    let partialAppFolder = folder upperRight
    List.fold partialAppFolder roverPosition commands 

let firstInput = {X = 5; Y = 5}
let result  = 
    listOfRovers
    |> List.map( fun (lcmds, myRP) -> foldRover firstInput myRP lcmds )


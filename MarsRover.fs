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

// 3 3 E

//MMRMMRMRRM

let listOfRovers = [(listOfCommands, myRoverPosition)]

// type Rover = 
//  { Commands: Command List
//    RoverPosition: RoverPosition}

let folder (roverPosition:RoverPosition) (command: Command):RoverPosition= 
    interpretCommand command roverPosition 

let state :RoverPosition = myRoverPosition

let list :Command list = listOfCommands

let result = List.fold folder state list



(*
    intermediate rover positions:
  init - 1 2 N
     L - 1 2 W
     M - 0 2 W
     L - 0 2 S
     M - 0 1 S 
     L - 0 1 W
     M - 1 1 W
     L - 1 1 N
     M - 1 2 N
     M - 1 3 N 
*)

(*
List.fold: 
   folder: 'State -> 'T -> 'State ->
   state : 'State   ->
   list  : list<'T> 
        -> 'State
        
*)
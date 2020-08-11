module MarsRover

open System

type Direction =
    | North
    | East
    | South
    | West

type Location = { X: int; Y: int }

type RoverPosition =
    { Location: Location
      Direction: Direction }

type Command =
    | M
    | L
    | R

type DeployRoversInput =
    { ListOfRovers: (Command List * RoverPosition) List
      UpperRight: Location }

let lowerLeft = { X = 0; Y = 0 }

let step direction (location: Location) (upperRight: Location) =
    match direction with
    | North ->
        { location with
              Y = if location.Y = upperRight.Y then upperRight.Y else location.Y + 1 }
    | East ->
        { location with
              X = if location.X = upperRight.X then upperRight.X else location.X + 1 }
    | South ->
        { location with
              Y = if location.Y = lowerLeft.Y then lowerLeft.Y else location.Y - 1 }
    | West ->
        { location with
              X = if location.X = lowerLeft.X then lowerLeft.X else location.X - 1 }



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


let turnLeft rover =
    { rover with
          Direction = leftOf rover.Direction }

let turnRight rover =
    { rover with
          Direction = rightOf rover.Direction }

let moveForward (upperRight: Location) rover =
    { rover with
          Location = step rover.Direction rover.Location upperRight }

let interpretCommand (upperRight: Location) command =
    match command with
    | L -> turnLeft
    | R -> turnRight
    | M -> moveForward upperRight

let folder (upperRight: Location) (roverPosition: RoverPosition) (command: Command): RoverPosition =
    interpretCommand upperRight command roverPosition

let foldRover (upperRight: Location) roverPosition commands =
    let partialAppFolder = folder upperRight
    List.fold partialAppFolder roverPosition commands

let deployRovers (input: DeployRoversInput) =
    input.ListOfRovers
    |> List.map (fun (lcmds, myRP) -> foldRover input.UpperRight myRP lcmds)

// Learn more about F# at http://fsharp.org
module MarsRover.Console

open MarsRover.Domain
open System



let convertToInt (s: string) =
    match Int32.TryParse s with
    | true, i -> Some i
    | false, _ -> None

let createLocation (input: string): Location =
    match input.Trim().Split(' ') with
    | [| x; y |] ->
        match convertToInt x, convertToInt y with
        | Some x1, Some y1 -> { X = x1; Y = y1 }
        | _, _ -> failwith "could not convert string to int"
    | _ -> failwith "must provide two coordinates"

let convertInputToDirection (d: string) =
    match d with
    | "N" -> Some North
    | "E" -> Some East
    | "S" -> Some South
    | "W" -> Some West
    | _ -> None


let constructRoverPosition (input: string) =
    match input.Trim().Split(' ') with
    | [| x; y; d |] ->
        match convertToInt x, convertToInt y, convertInputToDirection d with
        | Some x1, Some y1, Some d ->
            { Location = { X = x1; Y = y1 }
              Direction = d }
        | _, _, None -> failwith "Enter valid direction"
        | None, _, _ -> failwith "Enter valid x coordinate"
        | _, None, _ -> failwith "Enter valid y coordinate"
    | _ -> failwith "must provide three items"



let matchStringToCommand (command: char) =
    match command with
    | 'M' -> Some M
    | 'L' -> Some L
    | 'R' -> Some R
    | _ -> None


let inputToCommandList (command: string): Command list =
    Seq.toList command
    |> List.map (fun s ->
        match matchStringToCommand s with
        | Some command -> command
        | None -> failwith "enter valid command")


type RoverInput =
    | Input of Command List * RoverPosition
    | Stop

let getRoverInput (): RoverInput =
    printfn "Please enter a Rover, if not Stop"
    let input = Console.ReadLine()
    if input = "Stop" then
        Stop
    else
        let roverPosition = constructRoverPosition input
        let input = Console.ReadLine()
        let commandList = inputToCommandList input
        Input(commandList, roverPosition)



let rec createListOfRovers existingRovers =
    let roverInput = getRoverInput ()
    match roverInput with
    | Stop -> existingRovers
    | Input (commandList, roverPosition) -> createListOfRovers ((commandList, roverPosition) :: existingRovers)
    |> List.rev



let getRoverPositionString (input: RoverPosition) =
    let directionToString (direction: Direction) =
        match direction with
        | North -> "N"
        | South -> "S"
        | East -> "E"
        | West -> "W"

    let locationToString (location: Location) = sprintf "%i %i" location.X location.Y
    let directionString = directionToString input.Direction
    let locationString = locationToString input.Location
    sprintf "%s %s" locationString directionString

let printRovers (input: RoverPosition list): unit =
    input
    |> List.map (fun c -> getRoverPositionString c)
    |> List.iter (fun c -> printfn "%s" c)


[<EntryPoint>]
let main argv =
    printfn "Welcome to Mars Rover!"
    try
        let input = Console.ReadLine()
        let upperRight = createLocation input
        let listOfRovers = createListOfRovers []
        // printfn "%A" listOfRovers
        { ListOfRovers = listOfRovers
          UpperRight = upperRight }
        |> deployRovers
        |> printRovers


        0 // return an integer exit code
    with e ->
        eprintfn "%s" e.Message
        -1

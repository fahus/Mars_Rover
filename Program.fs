// Learn more about F# at http://fsharp.org
open MarsRover
open System

let convertToInt (s:string) =
    match Int32.TryParse s with 
    | true, i -> Some i 
    | false, _ -> None

let createLocation (input:string) :Location =
    match input.Trim().Split (' ') with 
    |[|x;y|] ->
        match convertToInt x, convertToInt y with 
        | Some x1, Some y1 ->   { X = x1; Y = y1 }
        | _,_ -> failwith "could not convert string to int"
    |_-> failwith "must provide two coordinates"

// convert string to direction 1 2 N
let convertInputToDirection (d:string)  =
    match d with
    | "N" -> Some North
    | "E" -> Some East
    | "S" -> Some South
    | "W" -> Some West 
    |_-> None


let constructRoverPosition (input:string)   = 
    match input.Trim().Split (' ') with 
    |[|x;y;d|] -> //[1;2;N]
       match convertToInt x, convertToInt y, convertInputToDirection d with 
        | Some x1, Some y1, Some d -> { Location = {X = x1; Y = y1} ; Direction = d}
        | _,_,None -> failwith "Enter valid direction"
        | None,_,_ -> failwith "Enter valid x coordinate"
        | _,None,_ -> failwith "Enter valid y coordinate"
    |_-> failwith "must provide three items"





[<EntryPoint>]
let main argv =
    printfn "Welcome to Mars Rover!"
    try 
        let input = Console.ReadLine()
        let upperRight = createLocation input 
        let input = Console.ReadLine()
        let roverPosition = constructRoverPosition input    
        printfn "%A" upperRight
        printfn "%A" roverPosition
        0 // return an integer exit code
    with e ->  
        eprintfn "%s" e.Message 
        -1



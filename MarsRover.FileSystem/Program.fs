// Learn more about F# at http://fsharp.org
open MarsRover.Domain
open System


let stringtoInt (input:string) =
    match Int32.TryParse input with
    | true, i -> Some i
    | false, _ -> None

let stringToLocation (input:string) = 
    match input.Split(' ') with
    | [| x; y |] ->
        match stringtoInt x, stringtoInt y with
        | Some x1, Some y1 -> { X = x1; Y = y1 }
        | _, None -> failwith "Enter valid y coordinate"
        | None, _ -> failwith "Enter valid x coordinate"
    | _ -> failwith "must provide two valid coordinates"

let stringtoDirection (d:string)  =
    match d with
    |"N" -> Some North
    |"E" -> Some East
    | "S" -> Some South
    |"W" -> Some West 
    | _ -> None

let stringToRoverPosition (input:string)  = 
    match input.Split (' ') with 
    |[|x;y;d|] ->
        match stringtoInt x, stringtoInt y, stringtoDirection d with 
        | Some x1, Some y1, Some d -> {Location = { X = x1 ; Y = y1 }
                                       Direction = d } 
        |_, _,None -> failwith "Enter valid direction"
        | None, _, _ -> failwith "Enter valid x coordinate"
        |_, None, _ -> failwith "Enter valid y coordinate"
    | _ -> failwith "Provide three items"
      

let matchStringToCommand (command:char)  =
    match command with 
    | 'M' -> Some M
    | 'L' -> Some L
    | 'R' -> Some R
    | _ -> None 

let stringToCommandList (command:string) :Command list =
    Seq.toList command 
    |> List.map (fun s ->
        match matchStringToCommand s with 
        | Some command -> command
        | None -> failwith "Enter valid Command" )
 
(*
5 5 upper right 
1 2 N rover position rover 1
LMLMLMLMM command list  rover 1 
3 3 E rover position rover 2 
MMRMMRMRRM command list rover 2
3 1 W
MMMRMMMLMMMRMM
9 2 S
MMRRMLM
*)


   
let removeFirstItem input = 
        match input |> List.ofArray with 
        | h::t ->  t
        | _ -> failwith "Cannot remove first item in list"

let extractRoverFromList (input:string list) = 
    List.head input
    |> stringToRoverPosition
   

let extractCommandFromList (input:string list) =
    List.head input
    |> stringToCommandList

let matchPairs (input:string list ) =
    let outputLength = input.Length/2 
    let newlist = [1..outputLength] 
    newlist |> List.map (fun x -> ( (x + x - 2), (x + x) - 1))
            |> List.map ( fun (x,y) -> stringToRoverPosition input.[x] , stringToCommandList input.[y] )
            |> List.rev
       


    



 
    

    
    
     


// [1..11]
// |>  List.chunkBySize 2
// // [
//     // [1; 2];
//     // [3; 4];
//     // [5; 6];
//     // [7; 8];
//     // [9; 10] 
//       //[11]
// // ]
// |> List.map (fun x -> x.[0], List.tryItem 1 x)
// //[(1,2); (3, Some 4); (5,6); (7,8); (9,10); (11, None)]
            





   


[<EntryPoint>]
let main argv =
    let input = (System.IO.File.ReadAllLines("/Users/fhussein/Projects/MarsRover/input.txt"))
    // printfn "%A" input
    let upperRight = stringToLocation input.[0]
    let result1 = input |> removeFirstItem |> matchPairs
    let result = {ListOfRovers =  result1; UpperRight = upperRight} |> deployRovers
    input |> removeFirstItem |> matchPairs |> deployRovers

// [ a; b; c; d; e; f ]
// [ 1; 2; 3 ]
// [ (1, 1); (2, 2); (3, 3) ]
// [ (0, 1); (1, 2); (2, 3) ]
// [ (0, 1); (2, 3); (4, 5) ]


// [ (a, b); (c, d); (e, f) ]



    0 // return an integer exit code




    


 
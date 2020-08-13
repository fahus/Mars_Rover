// Learn more about F# at http://fsharp.org
open MarsRover.Domain
open System


let convertStringtoInt (input:string) =
    match Int32.TryParse input with
    | true, i -> Some i
    | false, _ -> None

let concertStringToLocation (input:string) = 
    match input.Split(' ') with
    | [| x; y |] ->
        match convertStringtoInt x, convertStringtoInt y with
        | Some x1, Some y1 -> { X = x1; Y = y1 }
            | _, _ -> failwith "failed to comvert string to int"
    | _ -> failwith "must provide two valid coordinates"







[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let input = (System.IO.File.ReadAllLines("/Users/fhussein/Projects/MarsRover/input.txt"))
    printfn "%A" input
    0 // return an integer exit code




    


 
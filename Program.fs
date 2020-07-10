// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let input = Console.ReadLine()
    printfn "%s" input
    0 // return an integer exit code

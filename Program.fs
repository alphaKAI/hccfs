// Learn more about F# at http://fsharp.org

open System
open System.IO

open HccFS.Ksexp
open HccFS.Compiler

[<EntryPoint>]
let main argv =
  Array.iter (fun fileName ->
    let fileData = File.ReadAllText fileName
    printfn "fileName: %s" fileName
    printfn "%s" fileData
    let parsed = sexpParse fileData
    printfn "parsed: %A" parsed
    let compiled = compile parsed
    printfn "compiled: %A" compiled
    ) argv
  0 // return an integer exit code

// Copyright (c) 2010 Russell Wallace
// Released under the MIT license

open System
open System.Diagnostics
open System.IO
open Types

let mutable timelimit = 0L
let rec parse args =
    match args with
    | ""::t -> parse t
    | "-f"::s::t ->
        let lines = File.ReadAllLines s |> List.ofArray
        parse lines @ parse t
    | "-t"::s::t ->
        timelimit <- int64 s * 1000L
        parse t
    | "."::_ -> []
    | s::t when s.StartsWith "#" -> parse t
    | s::_ when s.StartsWith "-" ->
        printfn "Ayane version 1"
        printfn "Copyright (c) 2010 Russell Wallace"
        printfn "Released under the MIT license"
        printfn ""
        printfn "Usage: ayane [options] files"
        printfn "-f file  get args from file"
        printfn "-t secs  time limit per problem"
        Environment.Exit 1
        []
    | s::t -> s :: parse t
    | _ -> []
let files = Environment.GetCommandLineArgs() |> List.ofArray |> List.tail |> parse

let ts64(x:int64) = String.Format("{0:#,0}", x)
let ts x = ts64(int64 x)

let total = files.Length
let mutable hardest = "[none]"
let mutable hardesttime = TimeSpan.Zero
let mutable solved = 0

for file in files do 
    let watch = Stopwatch.StartNew()
    let mutable input = 0
    try
        Infer.clear()
        Tptp.read file
        input <- Infer.unprocessed.Count
        Infer.solve timelimit
    with
    | ResultException(r, c) ->
        let file = Path.GetFileName file
        printf "%% SZS status %A for %s" r file
        let e = Tptp.expected
        let bad =
            r = Satisfiable && e = Unsatisfiable ||
            r = Unsatisfiable && e = Satisfiable
        if bad then printf " : WARNING - problem thought to be %A" e
        printfn ""
        let success =
            match r with
            | Satisfiable -> true
            | Unsatisfiable ->
                printfn "%% SZS output start CNFRefutation for %s" file
                Tptp.proof c
                printfn "%% SZS output end CNFRefutation for %s" file
                true
            | _ -> false
        let p = Infer.processed.Count
        let u = Infer.unprocessed.Count
        printfn "%% Clauses %s -> %s + %s = %s" (ts input) (ts p) (ts u) (ts(p + u))
        let time = watch.Elapsed
        printfn "%% %A" time
        if success then
            if hardesttime < time then
                hardest <- file
                hardesttime <- time
            solved <- solved + 1
        printfn ""
        if bad then Environment.Exit 1
    
let p = Process.GetCurrentProcess()
printfn "%%================================="
printfn "%% Solved         %s / %s = %0.3f%%" (ts solved) (ts total) (float solved / float total * 100.0)
printfn "%% Hardest solved %s" hardest
printfn "%% Hardest time   %A" hardesttime
printfn "%% Working set    %s" (ts64 p.PeakWorkingSet64)
printfn "%% Virtual memory %s" (ts64 p.PeakVirtualMemorySize64)
printfn "%% CPU time       %A" p.TotalProcessorTime
printfn "%%================================="
printfn ""

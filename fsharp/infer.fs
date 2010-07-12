// Copyright (c) 2010 Russell Wallace
// Released under the MIT license

module Infer
open C5
open System
open System.Collections.Generic
open System.Diagnostics
open Types

// Data

let processed = new List<Clause>()
let seen = new HashSet<Term list>()
let mutable unprocessed = new IntervalHeap<Clause>(cmpclauses)

let clear() =
    processed.Clear()
    seen.Clear()
    unprocessed <- new IntervalHeap<Clause>(cmpclauses)

// Add

let rec normvars' m xs =
    match xs with
    | x::t ->
        let m, x = normvars m x
        let m, t = normvars' m t
        m, x :: t
    | _ -> m, xs
and normvars m x =
    match x with
    | Call(s, args) ->
        let m, args = normvars' m args
        m, Call(s, args)
    | Eq(x, y) ->
        let m, x = normvars m x
        let m, y = normvars m y
        m, Eq(x, y)
    | Ne(x, y) ->
        let m, x = normvars m x
        let m, y = normvars m y
        m, Ne(x, y)
    | Var _ when Map.containsKey x m -> m, m.[x]
    | Var _ ->
        let v = Var m.Count
        Map.add x v m, v
    | _ -> m, x

let rec rmdup xs =
    match xs with
    | x::y::t when x = y -> rmdup(y :: t)
    | x::y::t -> x :: rmdup(y :: t)
    | _ -> xs
let rec size x =
    match x with
    | Call(s, args) -> 1 + List.sumBy size args
    | Eq(x, y) | Ne(x, y) -> 1 + size x + size y
    | _ -> 1
let tautology neg pos = List.exists (fun x -> List.exists (fun y -> x = y) neg) pos

let add from name es =
    let es = List.filter (fun x -> x <> True) es |> List.sort |> rmdup
    if not(tautology neg pos) then
        let _, es = normvars' Map.empty es
        if seen.Add(es) then
            let c = {
                from = from
                name = name
                es = es
                priority = List.sumBy size es
            }
            if es = [] then raise(ResultException(Unsatisfiable, c))
            unprocessed.Add c |> ignore

// Input

let rec flat x es =
    match x with
    | Or(x, y) -> flat x es |> flat y 
    | Not(Eq(x, y)) -> Ne(x, y) :: es 
    | Not(Ne(x, y)) -> Eq(x, y) :: es 
    | Not(Not x) -> flat x es 
    | Not x -> Ne(x, True) :: es
    | _ -> Eq(x, True) :: es

let input name x =
    add [] name (flat x [])

// Unify

let rec occurs m v x =
    match x with
    | Call(_, args) -> List.exists (occurs m v) args
    | Eq(x, y) | Ne(x, y) -> occurs m v x || occurs m v y
    | Var _ when v = x -> true
    | Var _ when Map.containsKey x m -> occurs m v m.[x]
    | _ -> false

let rec unifyvar m v x =
    if Map.containsKey v m then unify (Some m) m.[v] x 
    elif Map.containsKey x m then unify (Some m) v m.[x] 
    elif occurs m v x then None
    else Map.add v x m |> Some
and unify m x y =
    match m with
    | Some m' ->
        match x, y with
        | _ when x = y -> m
        | Var _, _ -> unifyvar m' x y
        | _, Var _ -> unifyvar m' y x
        | Call(xf, xargs), Call(yf, yargs) when xf = yf -> List.fold2 unify m xargs yargs
        | Eq(x, x'), Eq(y, y') | Ne(x, x'), Ne(y, y') -> unify (unify m x y) x' y'
        | _ -> None
    | _ -> None

// Resolve

let rec subst m x =
    match x with
    | Call(s, args) -> Call(s, List.map (subst m) args)
    | Eq(x, y) -> Eq(subst m x, subst m y)
    | Ne(x, y) -> Ne(subst m x, subst m y)
    | Var _ when Map.containsKey x m -> subst m m.[x]
    | _ -> x

let resolve g es (c:Clause) =
    for e in es do
        for ce in c.es do
            match unify (Some Map.empty) e ce with
            | Some m ->
                let neg = List.filter (fun z -> z <> x) neg @ neg' |> List.map (subst m)
                let pos = pos @ List.filter (fun z -> z <> y) pos' |> List.map (subst m)
                add from False neg pos
            | _ -> ()

// Solve

let rec altvars x =
    match x with
    | Call(s, args) -> Call(s, List.map altvars args)
    | Eq(x, y) -> Eq(altvars x, altvars y)
    | Ne(x, y) -> Ne(altvars x, altvars y)
    | Var i -> Var ~~~i
    | _ -> x

let solve timelimit =
    let watch = Stopwatch.StartNew()
    while not unprocessed.IsEmpty do
        if timelimit <> 0L && timelimit <= watch.ElapsedMilliseconds then raise(ResultException(Timeout, noclause))
        let g = unprocessed.DeleteMin()
        let es = List.map altvars g.es
        for c in processed do
            let neg' = c.neg
            let pos' = c.pos
            let from = [c; g]
            resolve neg pos neg' pos' from
            resolve neg' pos' neg pos from
        processed.Add g
    raise(ResultException(Satisfiable, noclause))

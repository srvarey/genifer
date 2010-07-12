// Copyright (c) 2010 Russell Wallace
// Released under the MIT license

module Types
open System.Collections.Generic
open System.Numerics

type Term =
// Core
| Call of string * Term list
| Eq of Term * Term
| False
| Int of BigInteger
| Ne of Term * Term
| Rat of BigRational
| Real of BigRational
| String of string
| True
| Var of int
// External
| All of Term list * Term
| And of Term * Term
| Eqv of Term * Term
| Exists of Term list * Term
| Imp of Term * Term
| Not of Term
| Or of Term * Term
| Xor of Term * Term

type Clause = {
    from: Clause list
    mutable name: Term
    es: Term list
    priority: int
}
let cmpclauses = {new IComparer<Clause> with member this.Compare(a, b) = a.priority - b.priority}
let noclause:Clause = (Array.zeroCreate 1).[0]

type Result =
| Satisfiable
| Timeout
| Unsatisfiable 
let results =
    dict[
        "Satisfiable", Satisfiable;
        "Unsatisfiable", Unsatisfiable;
    ]
exception ResultException of Result * Clause

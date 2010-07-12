// Copyright (c) 2010 Russell Wallace
// Released under the MIT license

module Tptp
open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open Types

// Input

type Token =
| ALL
| AND
| ATOM
| CLOSEPAREN
| CLOSESQUARE
| COLON
| COMMA
| DOT
| EOF
| EQ
| EQV
| EXISTS
| IMP
| IMPR
| NAME
| NAND
| NE
| NOR
| NOT
| OPENPAREN
| OPENSQUARE
| OR
| VAR
| XOR

let alnum c = Char.IsLetterOrDigit c || c = '_'
let ten = bignum.FromInt 10
let pow10 e = if 0 <= e then bignum.PowN(ten, e) else bignum.One / bignum.PowN(ten, -e)
let statusrx = new Regex(@"Status\s*:\s*(\w+)", RegexOptions.Compiled)

let tptp = Environment.GetEnvironmentVariable "TPTP"
let incdir = if tptp = null then "." else tptp

let sb = new StringBuilder()
let vari = ref 0
let vars = new Dictionary<string, int>()

let mutable c = ' '
let mutable term = False
let mutable token = EOF

let mutable expected = Timeout

let rec read' file =
    use stream = File.OpenRead file

    let readc() = c <- stream.ReadByte() |> char
    let appendc() =
        sb.Append c |> ignore
        readc()

    let integer() =
        match c with
        | '-' -> appendc()
        | '+' -> readc()
        | _ -> ()
        while Char.IsDigit c do appendc()
    let num() =
        sb.Length <- 0
        let minus = c = '-'
        integer()
        match c with
        | '.' ->
            let w = sb.ToString() |> bignum.Parse
            sb.Length <- 0
            readc()
            integer()
            let f = (sb.ToString() |> bignum.Parse) / pow10 sb.Length
            let f = if minus then -f else f
            let m = w + f
            match c with
            | 'e' | 'E' ->
                sb.Length <- 0
                readc()
                integer()
                let e = sb.ToString() |> int |> pow10
                term <- Real(m * e)
            | _ -> term <- Real m
        | '/' ->
            appendc()
            integer()
            term <- Rat(sb.ToString() |> bignum.Parse)
        | 'e' | 'E' ->
            let m = sb.ToString() |> bignum.Parse
            sb.Length <- 0
            readc()
            integer()
            let e = sb.ToString() |> int |> pow10
            term <- Real(m * e)
        | _ -> term <- Int(sb.ToString() |> bigint.Parse)

    let got t =
        readc()
        token <- t
    let rec lex() =
        token <- ATOM
        match c with
        | '!' ->
            readc()
            match c with
            | '=' -> got NE
            | _ -> token <- ALL
        | '"' ->
            sb.Length <- 0
            readc()
            while c <> '"' do
                if c = '\\' then readc()
                appendc()
            readc()
            term <- String(sb.ToString())
        | '$' ->
            sb.Length <- 0
            readc()
            while alnum c do appendc()
            term <- if sb.ToString() = "true" then True else False
        | '%' ->
            sb.Length <- 0
            while c <> '\n' do appendc()
            let m = statusrx.Match(sb.ToString())
            if m.Success then
                let r = ref Timeout
                if results.TryGetValue(m.Groups.[1].ToString(), r) then expected <- !r
            lex()
        | '&' -> got AND
        | '\'' ->
            token <- NAME
            sb.Length <- 0
            readc()
            while c <> '\'' do
                if c = '\\' then readc()
                appendc()
            readc()
        | '(' -> got OPENPAREN
        | ')' -> got CLOSEPAREN
        | '+' | '-' -> num()
        | ',' -> got COMMA
        | '.' -> got DOT
        | '/' ->
            readc()
            c <- ' '
            while c <> '/' do
                while c <> '*' do readc()
                readc()
            readc()
            lex()
        | ':' -> got COLON
        | '<' ->
            readc()
            match c with
            | '=' ->
                readc()
                match c with
                | '>' -> got EQV
                | _ -> token <- IMPR
            | _ ->
                readc()
                got XOR
        | '=' ->
            readc()
            match c with
            | '>' -> got IMP
            | _ -> token <- EQ
        | '?' -> got EXISTS
        | '[' -> got OPENSQUARE
        | ']' -> got CLOSESQUARE
        | '|' -> got OR
        | '~' ->
            readc()
            match c with
            | '&' -> got NAND
            | '|' -> got NOR
            | _ -> token <- NOT
        | _ when Char.IsWhiteSpace c ->
            readc()
            lex()
        | _ when Char.IsUpper c ->
            token <- VAR
            sb.Length <- 0
            while alnum c do appendc()
            let s = sb.ToString()
            term <-
                if vars.TryGetValue(s, vari) then Var !vari
                else
                    let n = vars.Count
                    vars.Add(s, n)
                    Var n
        | _ when Char.IsDigit c -> num()
        | _ when alnum c ->
            token <- NAME
            sb.Length <- 0
            while alnum c do appendc()
        | _ -> token <- EOF

    let eat t =
        if token = t then
            lex()
            true
        else false
    let rec args() =
        match token with
        | CLOSEPAREN | CLOSESQUARE ->
            lex()
            []
        | _ ->
            let x = expr()
            eat COMMA |> ignore
            x :: args()
    and element() =
        match token with
        | NAME ->
            let s = sb.ToString()
            lex()
            Call(s, if eat OPENPAREN then args() else [])
        | OPENPAREN ->
            lex()
            let x = expr()
            eat CLOSEPAREN |> ignore
            x
        | OPENSQUARE ->
            lex()
            let x = Call("[]", args())
            x
        | _ ->
            let x = term
            lex()
            x
    and infix() =
        let x = element()
        match token with
        | COLON ->
            lex()
            Call(":", [x; infix()])
        | EQ ->
            lex()
            Eq(x, infix())
        | NE ->
            lex()
            Ne(x, infix())
        | _ -> x
    and quant f =
        lex()
        eat OPENSQUARE |> ignore
        let vs = args()
        eat COLON |> ignore
        f(vs, prefix())
    and prefix() =
        match token with
        | ALL -> quant All
        | EXISTS -> quant Exists
        | NOT ->
            lex()
            Not(prefix())
        | _ -> infix()
    and expr() =
        let x = prefix()
        match token with
        | AND ->
            lex()
            And(x, expr())
        | EQV ->
            lex()
            Eqv(x, expr())
        | IMP ->
            lex()
            Imp(x, expr())
        | IMPR ->
            lex()
            Imp(expr(), x)
        | NAND ->
            lex()
            Not(And(x, expr()))
        | NOR ->
            lex()
            Not(Or(x, expr()))
        | OR ->
            lex()
            Or(x, expr())
        | XOR ->
            lex()
            Xor(x, expr())
        | _ -> x
    let input() =
        match sb.ToString() with
        | "include" ->
            lex()
            eat OPENPAREN |> ignore
            let c' = c
            let term' = term
            let token' = token
            Path.Combine(incdir, sb.ToString()) |> read'
            c <- c'
            term <- term'
            token <- token'
        | _ ->
            lex()
            eat OPENPAREN |> ignore
            let name = expr()
            eat COMMA |> ignore
            let role = expr()
            eat COMMA |> ignore
            vars.Clear()
            let formula = expr()
            Infer.input name formula
        eat COMMA |> ignore
        args() |> ignore
        eat DOT |> ignore

    readc()
    lex()
    while token <> EOF do input()

let read file =
    expected <- Timeout
    read' file

// Output

let quote q s =
    printf "%c" q
    for c in s do
        if c = q || c = '\\' then printf "\\"
        printf "%c" c
    printf "%c" q
let quoted(s:string) = s.Length = 0 || Char.IsDigit(s, 0) || Char.IsUpper(s, 0) || not(Seq.forall alnum s)
let rec commas xs =
    match xs with
    | x::y::t ->
        print x
        printf ", "
        commas(y :: t)
    | x::_ -> print x
    | _ -> ()
and infix x op y =
    print x
    printf " %s " op
    print y
and print x =
    match x with
    | Call(s, args) ->
        if quoted s then quote '\'' s else printf "%s" s
        if args <> [] then
            printf "("
            commas args
            printf ")"
    | Eq(x, y) -> infix x "=" y
    | False -> printf "$false"
    | Int x -> printf "%O" x
    | Ne(x, y) -> infix x "!=" y
    | Rat x -> printf "%O" x
    | Real x -> printf "%A" (float x)
    | String s -> quote '"' s
    | True -> printf "$true"
    | Var i when i < 26 -> printf "%c" ('A' + char i) 
    | Var i -> printf "Z%d" (i - 25)
    | Or(x, y) -> infix x "|" y
    | _ -> ()

let rec toterm xs =
    match xs with
    | x::y::t -> toterm(Or(x, y) :: t)
    | x::_ -> x
    | _ -> False
let printc c =
    printf "cnf("
    print c.name
    printf ", plain, "
    print(toterm c.es)
    if c.from <> [] then
        printf ", inference(sp, [status(thm)], ["
        commas[for a in c.from -> a.name]
        printf "])"
    printfn ")."

let proof c =
    let cs = new List<Clause>()
    let visited = new HashSet<Clause>()
    let rec f c =
        if visited.Add c then
            for a in c.from do f a
            cs.Add c
    f c
    for i = 0 to cs.Count - 1 do
        let c = cs.[i]
        if c.from <> [] then c.name <- Int(bigint i)
        printc c

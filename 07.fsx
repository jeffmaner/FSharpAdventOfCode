module Day07

#load "Ancillary.fsx"
open Ancillary

type Wires = Map<string,int>

let isNumeric = System.Int32.TryParse >> fst

let assign w v m = Map.add w v m

let doBinaryGate g i j w (m:Wires) =
    let i',j' =
        match isNumeric i, isNumeric j with
        | true, true -> (int i , int j )
        | true, _    -> (int i ,  m.[j])
        | _   , true -> ( m.[i], int j )
        | _   , _    -> ( m.[i],  m.[j])
    let v = g i' j'
     in assign w v m

let toSignal i m = if isNumeric i then int i else Map.find i m

let doAnd    = doBinaryGate (&&&)
let doOr     = doBinaryGate (|||)
let doLShift = doBinaryGate (<<<)
let doRShift = doBinaryGate (>>>)
let doNot i w m =
    let v = toSignal i m |> (uint16 >> (~~~) >> int)
     in assign w v m

let arrow = "->"

let doOperation m = function
    | [| i; "AND"   ; j; arrow; o |] -> doAnd    i j o m
    | [| i; "OR"    ; j; arrow; o |] -> doOr     i j o m
    | [| i; "LSHIFT"; j; arrow; o |] -> doLShift i j o m
    | [| i; "RSHIFT"; j; arrow; o |] -> doRShift i j o m
    | [|    "NOT"   ; i; arrow; o |] -> doNot    i   o m
    | [| i;              arrow; o |] -> let i' = toSignal i m in assign o i' m
    |    _                           -> failwith "Undefined"

let isReady (m:Wires) = function
    | [| "NOT"; i; arrow; o |]
    | [| i;       arrow; o |]                                 -> isNumeric i || m.ContainsKey i
    | [| i; _; j; arrow; o |] when isNumeric i                -> m.ContainsKey j
    | [| i; _; j; arrow; o |] when isNumeric j                -> m.ContainsKey i
    | [| i; _; j; arrow; o |] when isNumeric i && isNumeric j -> true
    | [| i; _; j; arrow; o |]                                 -> m.ContainsKey i && m.ContainsKey j
    |    _                                                    -> false

let rec execute instructions wires =
    match instructions with
    | [] -> Map.find "a" wires
    | _  -> let (ready,notReady) = instructions |> List.partition (isReady wires)
             in execute notReady (List.fold doOperation wires ready)

let instructions =
    inputFile "07"
    |> (readLines >> Array.map words >> List.ofArray)

// Part One.
let a = execute instructions Map.empty // 46065

// Part Two.
let changeBToA = function
    | [| _; arrow; "b" |] -> [| string a; arrow; "b" |]
    |  _ as x             -> x

let instructions' = instructions |> List.map changeBToA

let a' = execute instructions' Map.empty // 14134

// vim:ft=fs

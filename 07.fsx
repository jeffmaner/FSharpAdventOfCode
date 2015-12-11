module Day07

open System.IO

let (</>) p q = Path.Combine (p,q)

let words (s:string) = s.Split ' '

let map f xs = Seq.map f xs

let isNumeric = System.Int32.TryParse >> fst

type Signal = int
type Identifier = string

type Wire = { identifier: Identifier; signal: Signal }

type Throughput =
     | Wire of Wire
     | Signal of Signal

type Gate =
     | AND    of (Throughput -> Throughput -> Throughput -> Throughput)
     | LSHIFT of (Throughput -> Throughput -> Throughput -> Throughput)
     | NOT    of (Wire -> Wire -> Wire)
     | OR     of (Throughput -> Throughput -> Throughput -> Throughput)
     | RSHIFT of (Throughput -> Throughput -> Throughput -> Throughput)

type Operation =
     | AssignWireToWire of (Wire -> Wire -> Wire)
     | AssignSignalToWire of (Signal -> Wire -> Wire)
     | Gate of Gate

let makeThroughput x =
    if isNumeric x
    then Signal <| int x
    else Wire { identifier = x; signal = 0 }

type Instruction = { input: Throughput list; operation: Operation; output: Throughput }

let arrow = "->"

let makeAssignment (s:Throughput) (d:Throughput) =
    match (s,d) with
    | Signal s', d' -> AssignSignalToWire (fun s' d' -> { d' with signal=s' })
    | Wire   s', d' -> AssignWireToWire   (fun s' d' -> { d' with signal=s'.signal })

let makeNot (s:Wire) (d:Wire) =
    Gate.NOT (fun s' d' -> { d' with signal= ~~~ s'.signal }) |> Operation.Gate

let makeInstruction = function
    | [| i;              arrow; o |] -> { input = [ makeThroughput i                   ]; operation = makeAssignment (makeThroughput i) (makeThroughput o) ; output = makeThroughput o }
    | [|    "NOT"   ; i; arrow; o |] -> { input = [ makeThroughput i                   ]; operation = makeNot (Wire (makeThroughput i)) (Wire (makeThroughput o))   ; output = makeThroughput o }
    | [| i; "AND"   ; j; arrow; o |] -> { input = [ makeThroughput i; makeThroughput j ]; operation = Gate AND   ; output = makeThroughput o }
    | [| i; "OR"    ; j; arrow; o |] -> { input = [ makeThroughput i; makeThroughput j ]; operation = Gate OR    ; output = makeThroughput o }
    | [| i; "LSHIFT"; j; arrow; o |] -> { input = [ makeThroughput i; makeThroughput j ]; operation = Gate LSHIFT; output = makeThroughput o }
    | [| i; "RSHIFT"; j; arrow; o |] -> { input = [ makeThroughput i; makeThroughput j ]; operation = Gate RSHIFT; output = makeThroughput o }
    |    _                           -> failwith "Undefined"

let instructions =
    __SOURCE_DIRECTORY__ </> "07.input"
    |> File.ReadAllLines
    |> map words
    |> map makeInstruction

// let (|Wire|Signal|) x = x

let f = function
    | Wire w -> Some w
    | Signal _ -> None

let collectWires = function
    | { input=i; operation=_; output=o } -> let wsi =  List.map f i |> List.filter (fun o -> o.IsSome) |> List.map (fun o -> o.Value) |> List.map (fun w -> w.identifier)
                                            let wo = let x = f o in if x.IsSome then x.Value.identifier else ""
                                             in wo :: wsi |> Seq.ofList

let collectWires' = function
    | { input=[Wire i]; operation=_; output=Wire o } -> ()
    | { input=[Signal i]; operation=_; output=Wire o } -> ()
    | { input=[Wire i; Wire j]; operation=_; output=Wire o } -> ()
    | { input=[Wire i; Signal j]; operation=_; output=Wire o } -> ()
    | { input=[Signal i; Wire j]; operation=_; output=Wire o } -> ()
    | { input=[Signal i; Signal j]; operation=_; output=Wire o } -> ()
    | { input=[Wire i]; operation=_; output=Signal o } -> ()
    | { input=[Signal i]; operation=_; output=Signal o } -> ()
    | { input=[Wire i; Wire j]; operation=_; output=Signal o } -> ()
    | { input=[Wire i; Signal j]; operation=_; output=Signal o } -> ()
    | { input=[Signal i; Wire j]; operation=_; output=Signal o } -> ()
    | { input=[Signal i; Signal j]; operation=_; output=Signal o } -> ()

let wires = instructions |> Seq.collect collectWires

let execute ws = function
    | { input=i; operation=Assignment ; output=Signal o } -> ws |> Seq.find (function
                                                              | { identifier=i'; signal=_ } -> List.map (function | Wire w -> w.identifier | _ -> failwith "Undefined") i |> List.contains i')
                                                              |> (fun w -> {w with signal=o})
    | { input=i; operation=Gate NOT   ; output=o } -> ()
    | { input=i; operation=Gate AND   ; output=o } -> ()
    | { input=i; operation=Gate OR    ; output=o } -> ()
    | { input=i; operation=Gate LSHIFT; output=o } -> ()
    | { input=i; operation=Gate RSHIFT; output=o } -> ()

// vim:ft=fs
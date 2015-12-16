module Day06

#load "Ancillary.fsx"
open Ancillary

type Action = TurnOn | TurnOff | Toggle

type Point = { x : int ; y : int }

let newPoint (s:string) =
    s.Split ','
    |> (map int >> fun a -> { x = Seq.item 0 a ; y = Seq.item 1 a })

type Instruction = {
  action : Action
  start  : Point
  stop   : Point }

let newInstruction = function
    | [| "toggle"     ; a; _; b |] -> { action = Toggle ; start = newPoint a; stop = newPoint b }
    | [| "turn"; "on" ; a; _; b |] -> { action = TurnOn ; start = newPoint a; stop = newPoint b }
    | [| "turn"; "off"; a; _; b |] -> { action = TurnOff; start = newPoint a; stop = newPoint b }
    |  _                           -> failwith "Undefined"

let instructions =
    inputFile "06"
    |> (readLines >> map words >> map newInstruction)

let flatten (array:'a[,]) = array |> Seq.cast<'a>

// Part One.
let lights = Array2D.create 1000 1000 false

let doInstruction' ls a b f =
    for x in [a.x .. b.x] do
        for y in [a.y .. b.y] do
            Array2D.set ls x y (f <| Array2D.get ls x y)
    ls

let doInstruction ls = function
    | { action=Toggle ; start=a; stop=b } -> doInstruction' ls a b (fun b -> not b)
    | { action=TurnOn ; start=a; stop=b } -> doInstruction' ls a b (fun _ -> true)
    | { action=TurnOff; start=a; stop=b } -> doInstruction' ls a b (fun _ -> false)

let litLights = fold doInstruction lights instructions

let on = (=) true

let lightsOn = litLights |> flatten |> filter on |> length // 543903

// Part Two.
let doInstruction2 ls = function
    | { action=Toggle ; start=a; stop=b } -> doInstruction' ls a b ((+) 2)
    | { action=TurnOn ; start=a; stop=b } -> doInstruction' ls a b ((+) 1)
    | { action=TurnOff; start=a; stop=b } -> doInstruction' ls a b (fun n -> max 0 (n - 1))

let lights' = Array2D.zeroCreate<int> 1000 1000

let litLights' = fold doInstruction2 lights' instructions

let totalBrightness = litLights' |> flatten |> map ((max) 0) |> sum // 14687245

// vim:ft=fs
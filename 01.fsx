module Day01

open System.IO

let upOrDown = function
    | '(' ->  1
    | ')' -> -1
    |  _  ->  0

let (</>) p q = Path.Combine (p, q)

let instructions =
  __SOURCE_DIRECTORY__ </> "01.input"
  |> File.ReadAllText
  |> List.ofSeq

// First Part.
let whatFloor = List.map upOrDown >> List.sum

whatFloor instructions // 280

// Second Part.
let firstStepIntoBasement =
    let groundFloor = 0
    let inBasement = (>) groundFloor
    let move f c = f + upOrDown c

     in List.scan move groundFloor >> List.findIndex inBasement

firstStepIntoBasement instructions // 1797

// vim:ft=fs
module Day11

#load "ancillary.fsx"
open Ancillary

let inc = function
    | 'z' -> (Some 'a','a')
    |  c  -> (None, int c |> (+) 1 |> char)

let increment chars =
    let rec f cs =
        match cs with
        | []    -> []
        | x::[] -> match inc x with
                   | (Some c, d) -> [c;d]
                   | (None  , c) -> [c]
        | x::xs -> match inc x with
                   | (Some _, d) -> d::f xs
                   | (None  , c) -> c::xs
     in chars |> (List.rev >> f >> List.rev)

let incrementPassword = increment

let includesOneIncreasingStraightOfThree (chars: char list) =
    let charsToInts = map int
    let differences = Seq.pairwise >> map (fun t -> snd t - fst t)
    let trues = filter (id)
    let isOne = (=) 1
    let inline every f xs = Seq.forall f xs
    let isIncreasing = charsToInts >> differences >> every isOne
    let moreThanOne = trues >> length >> ((<) 1)
     in chars |> (List.windowed 3 >> map isIncreasing >> moreThanOne)

let doesNotIncludeConfusingLetters chars =
    let confusingLetters = charList "ilo"
    let isConfusingLetter c = c |> elem <| confusingLetters
     in chars |> (map isConfusingLetter >> filter (id) >> length >> ((=) 0))

// Move windowed to Ancillary and refactor fsxs.
let inline windowed n xs = Seq.windowed n xs

let containsAtLeastTwoDifferentNonOverlappingPairs chars =
    let pairs = chars |> windowed 2 |> filter (fun xs -> xs.[0] = xs.[1]) |> filter (fun xs -> length xs |> ((=) 2))
    let distinctPairs = pairs |> Seq.distinct
     in distinctPairs |> (length >> ((<=) 2))

let isValid s =
    let requirements = [ doesNotIncludeConfusingLetters; includesOneIncreasingStraightOfThree; containsAtLeastTwoDifferentNonOverlappingPairs ]
     in all (fun f -> f s) requirements

let rec until test f x =
    let x' = f x
     in if test x'
        then x'
        else until test f x'

// Move join to Ancillary and refactor fsxs.
let join (cs:char list) = System.String.Join("", cs)

let currentPassword =  "hxbxwxba" // "ghijklmn" // "abcdefgh"

let partOne = until isValid incrementPassword (charList currentPassword) |> join // hxccdeff is incorrect.

// vim:ft=fs
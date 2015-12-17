module Day05

#load "Ancillary.fsx"
open Ancillary

let inline all      f xs = List.forall  f xs
let inline first      xs = Array.head     xs
let inline groupBy  f xs = Seq.groupBy  f xs
let inline indexed    xs = Seq.indexed    xs
let inline last       xs = Array.last     xs
let inline pairwise   xs = Seq.pairwise   xs
let inline reduce   f xs = Seq.reduce   f xs
let inline toSeq      xs = List.toSeq     xs
let inline windowed n xs = Seq.windowed n xs

// Part One.
let isNice s =
    let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ] |> toSeq
    let vowelCount = filter (fun c -> c |> elem <| vowels) >> length
    let atLeastThreeVowels = vowelCount >> (<=) 3

    let repeatedCharCount = pairwise >> filter (fun (x,y) -> x=y) >> length
    let atLeastOneRepeatedChar = repeatedCharCount >> (<) 0

    let offendingCombinations = [ "ab"; "cd"; "pq"; "xy" ] |> toSeq |> map charList
    let offenses s =
        let s' = charList s
         in filter (flip isInfixOf s') offendingCombinations
    let isOffensive = offenses >> length >> (<) 0
    let isNotOffensive = not << isOffensive

    let requirements = [ atLeastThreeVowels; atLeastOneRepeatedChar; isNotOffensive ]
     in all (fun f -> f s) requirements

let niceStrings =
  inputFile "05"
  |> (readLines >> filter isNice >> length) // 258

// Part Two.
let isNice' s =
    let containsRepeatedSequences s' =
        let groups = s' |> (windowed 2 >> indexed >> groupBy snd)

        let repeated = snd >> length >> (<) 1
        let filterRepeats = filter repeated
        let stripChars = map fst
        let simplify = map (fun g -> fst g, snd g |> stripChars)
        let sortIndices = map (fun g -> fst g, snd g |> Seq.sortDescending)
        let measureDistances = map (fun (k,vs) -> (k, vs |> reduce (-)))

        let distances = groups |> (filter repeated >> simplify >> sortIndices >> measureDistances)
        let repeatedSequences = distances |> filter (snd >> (<) 1) |> map fst
         in repeatedSequences |> length |> (<) 0

    let containsPattern s' =
        let matchesPattern a = first a = last a
        let sequences = s' |> (windowed 3 >> filter matchesPattern)
         in sequences |> (length >> (<) 0)

    let requirements = [containsRepeatedSequences; containsPattern]
     in all (fun f -> f s) requirements

let niceStrings' =
  __SOURCE_DIRECTORY__ </> "05.input"
  |> (readLines >> filter isNice' >> length) // 53

// vim:ft=fs

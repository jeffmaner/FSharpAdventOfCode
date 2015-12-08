module Day05

open System.IO

let inline all      xs = List.forall  xs
let inline contains xs = Seq.contains xs
let inline exists   xs = List.exists  xs
let inline filter   xs = Seq.filter   xs
let inline first    xs = Array.head   xs
let inline groupBy  xs = Seq.groupBy  xs
let inline indexed  xs = Seq.indexed  xs
let inline last     xs = Array.last   xs
let inline length   xs = Seq.length   xs
let inline map      xs = Seq.map      xs
let inline pairwise xs = Seq.pairwise xs
let inline reduce   xs = Seq.reduce   xs
let inline toList   xs = List.ofSeq   xs
let inline toSeq    xs = List.toSeq   xs
let inline windowed xs = Seq.windowed xs

// A la Haskell.
let (</>) p q = Path.Combine (p,q)

// A la Haskell.
let rec tails xs =
    match xs with
    | []    -> [[]]
    | _::ys -> xs :: tails ys

// A la Haskell.
let rec isPrefixOf needle haystack =
    match (needle,haystack) with
    | ([],_) -> true
    | (_,[]) -> false
    | (x::xs,y::ys) -> x=y && isPrefixOf xs ys

// A la Haskell.
let rec isInfixOf needle haystack = exists (isPrefixOf needle) (tails haystack)

// A la Haskell.
let flip f x y = f y x

// Part One.
let isNice s =
    let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ] |> toSeq
    let vowelCount = filter (fun c -> contains c vowels) >> length
    let atLeastThreeVowels = vowelCount >> (<=) 3

    let repeatedCharCount = pairwise >> filter (fun (x,y) -> x=y) >> length
    let atLeastOneRepeatedChar = repeatedCharCount >> (<) 0

    let offendingCombinations = [ "ab"; "cd"; "pq"; "xy" ] |> toSeq |> map toList
    let offenses s =
        let s' = toList s
         in filter (flip isInfixOf s') offendingCombinations
    let isOffensive = offenses >> length >> (<) 0
    let isNotOffensive = not << isOffensive

    let requirements = [ atLeastThreeVowels; atLeastOneRepeatedChar; isNotOffensive ]
     in all (fun f -> f s) requirements

let niceStrings =
  __SOURCE_DIRECTORY__ </> "05.input"
  |> File.ReadAllLines
  |> filter isNice
  |> length // 258

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
  |> File.ReadAllLines
  |> filter isNice'
  |> length // 53

// vim:ft=fs

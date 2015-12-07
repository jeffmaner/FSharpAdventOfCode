module Day05

open System.IO

let inline all    xs = List.forall xs
let inline filter xs = Seq.filter  xs
let inline length xs = Seq.length  xs
let inline map    xs = Seq.map     xs
let inline toList xs = List.ofSeq  xs
let inline toSeq  xs = List.toSeq  xs

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
let rec isInfixOf needle haystack = List.exists (isPrefixOf needle) (tails haystack)

// A la Haskell.
let flip f x y = f y x

// Part One.
let isNice s =
    let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ] |> toSeq
    let vowelCount : string -> int = filter (fun c -> Seq.contains c vowels) >> length
    let atLeastThreeVowels : string -> bool = vowelCount >> (<=) 3

    let repeatedCharCount : string -> int = Seq.pairwise >> filter (fun (x,y) -> x=y) >> length
    let atLeastOneRepeatedChar : string -> bool = repeatedCharCount >> (<) 0

    let offendingCombinations = [ "ab"; "cd"; "pq"; "xy" ] |> toSeq |> map toList
    let offenses s =
        let s' = toList s
         in filter (flip isInfixOf s') offendingCombinations
    let isOffensive : string -> bool = offenses >> length >> (<) 0
    let isNotOffensive : string -> bool = not << isOffensive

    let requirements = [ atLeastThreeVowels; atLeastOneRepeatedChar; isNotOffensive ]
     in all (fun f -> f s) requirements

let niceStrings =
  __SOURCE_DIRECTORY__ </> "05.input"
  |> File.ReadAllLines
  |> filter isNice
  |> length // 258

// This FS0030 Value Restriction error is seriously pissing me off. What's the
// point in point-free programming if we can't actually do point-free
// programming?
// First I just added explicit parameters. That grated on me. So I replaced
// explicit parameters with type function signatures.

// vim:ft=fs

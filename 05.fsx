module Day05

open System.IO

let (</>) p q = Path.Combine (p,q)

let isNice s =
    let vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
    let vowelCount s = (Seq.filter (fun c -> List.contains c vowels) >> Seq.length) s
    let atLeastThreeVowels s = let vs = vowelCount s in vs >= 3
    let repeatedCharCount s = (Seq.pairwise >> Seq.map (fun (x,y) -> x=y) >> Seq.filter (fun b -> b) >> Seq.length) s
    let atLeastOneRepeatedChar s = let cs = repeatedCharCount s in cs > 0
    let offendingCombinations = [ "ab"; "cd"; "pq"; "xy" ]
    let isNotOffensive (s:string) = offendingCombinations |> List.filter (fun c -> s.Contains c) |> List.length |> (=) 0
    let requirements = [ atLeastThreeVowels; atLeastOneRepeatedChar; isNotOffensive ]
     in List.forall (fun f -> f s) requirements

let niceStrings =
  __SOURCE_DIRECTORY__ </> "05.input"
  |> File.ReadAllLines
  |> Seq.filter isNice
  |> Seq.length // 258

// vim:ft=fs

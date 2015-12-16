module Ancillary

open System.IO

// A la Haskell.

let (</>) p q = Path.Combine (p,q)

let read = File.ReadAllText
let readLines = File.ReadAllLines

let words (s:string) = s.Split ' '

let inline exists f xs = List.exists f xs
let inline filter f xs = Seq.filter  f xs
let inline fold   f xs = Seq.fold    f xs
let inline length   xs = Seq.length    xs
let inline map    f xs = Seq.map     f xs
let inline sum      xs = Seq.sum       xs

let inline flip f x y = f y x

let rec tails xs =
    match xs with
    | []    -> [[]]
    | _::ys -> xs :: tails ys

let rec isPrefixOf needle haystack =
    match (needle,haystack) with
    | ([],_) -> true
    | (_,[]) -> false
    | (x::xs,y::ys) -> x=y && isPrefixOf xs ys

let rec isInfixOf needle haystack = exists (isPrefixOf needle) (tails haystack)

let inputFile day = __SOURCE_DIRECTORY__ </> sprintf "%s.input" day

// vim:ft=fs
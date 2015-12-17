module Day08

#load "Ancillary.fsx"
open Ancillary

let inline every f xs = List.forall f xs

let hexChars = [['0'..'9'] ; ['a'..'f'] ; ['A'..'F']] |> concat

let isHex c = c |> elem <| hexChars

let rec parsedLength = function
    | '\\'::'x'::y::z::xs when every isHex [y;z] -> 1 + parsedLength xs
    | '\\'::'\\'::xs                             -> 1 + parsedLength xs
    | '\\'::'"'::xs                              -> 1 + parsedLength xs
    | x::xs                                      -> 1 + parsedLength xs
    | []                                         -> 0

let lengths s = (length s, (parsedLength s) - 2) // Plus two for " at start and " at end.

let stringLiterals = inputFile "08" |> readLines

let sumTuple t a = (fst t) + (fst a), (snd t) + (snd a)

let code,value = stringLiterals |> (map charList >> map lengths >> fold sumTuple (0,0))

let partOne = code - value // 1350

let rec encodedLength = function
    | '\\'::'x'::y::z::xs when every isHex [y;z] -> 5 + encodedLength xs
    | '\\'::'\\'::xs                             -> 4 + encodedLength xs
    | '\\'::'"'::xs                              -> 4 + encodedLength xs
    | x::xs                                      -> 1 + encodedLength xs
    | []                                         -> 0

let lengths' s = (length s, (encodedLength s) + 4) // Plus four for \" at start and \" at end.

let code',value' = stringLiterals |> (map charList >> map lengths' >> fold sumTuple (0,0))

let partTwo = value' - code' // 2085

// vim:ft=fs
module Day12

#load "ancillary.fsx"
open Ancillary
open System.Text.RegularExpressions

let input = inputFile "12" |> read

let numbers = Regex.Matches(input, @"-?\d+")

let ns = [ for n in numbers do
               yield int n.Value ]

let partOne = sum ns // 156366

// vim:ft=fs
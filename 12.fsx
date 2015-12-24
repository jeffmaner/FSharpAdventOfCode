module Day12

#load "ancillary.fsx"
open Ancillary
open System.Text.RegularExpressions

let input = inputFile "12" |> read

let numbers = Regex.Matches(input, @"-?\d+")

let ns = [ for n in numbers do
               yield int n.Value ]

let partOne = sum ns // 156366

#load "json-parser.fsx"
open JsonParser

let (=~) s t = Regex.IsMatch(s,t)

let isRedObject = function
    | JSString s -> s =~ ".*red.*" // Apparently s won't equal red, "red", ""red"", or anything.
    | _          -> false

let rec removeRedObjects = function
    | JSObject o -> if Map.exists (fun k v -> isRedObject v) o then [0] else Map.toList o |> List.collect (fun (k,v) -> removeRedObjects v)
    | JSNumber n -> [int n]
    | JSArray  a -> List.collect removeRedObjects a
    | _          -> []

let partTwo = json().Parse input |> removeRedObjects |> sum // 96852

// vim:ft=fs
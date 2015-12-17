module Day09

#load "Ancillary.fsx"
open Ancillary

// From /F# for Scientists/, p. 166-167.
let rec distribute e = function
    | [] -> [[e]]
    | x::xs as ys -> (e::ys)::[for zs in distribute e xs -> x::zs]

// From /F# for Scientists/, p. 166-167.
let rec permute = function
    | [] -> [[]]
    | x::xs -> List.collect (distribute x) (permute xs)

type Route = { origin : string; destination : string; distance : int }

let makeRoute = function
    | [| originCity; "to"; destinationCity; "="; distance |] ->
      { origin = originCity; destination = destinationCity; distance = int distance }
    | _ -> failwith "Undefined"

let routes = inputFile "09" |> (readLines >> map words >> map makeRoute)

let originCities = routes |> map (fun r -> r.origin)
let destinationCities = routes |> map (fun r -> r.destination)
let cities' = concat [originCities; destinationCities]
let cities = Seq.distinct cities'
let possibleRoutes = permute <| List.ofSeq cities

let isRoute originCity destinationCity (r:Route) =
    (r.origin = originCity && r.destination = destinationCity) ||
    (r.origin = destinationCity && r.destination = originCity)

let rec findDistances = function
    | c::d::cs -> let rd = routes |> Seq.find (isRoute c d) |> fun r -> r.distance
                   in rd::findDistances (d::cs)
    | _ -> []

let partOne = possibleRoutes |> (map findDistances >> map sum >> Seq.min) // 251
let partTwo = possibleRoutes |> (map findDistances >> map sum >> Seq.max) // 898

// vim:ft=fs
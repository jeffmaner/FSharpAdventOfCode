module Day03

let move = function
    | '^' -> ( 0, -1)
    | 'v' -> ( 0,  1)
    | '>' -> ( 1,  0)
    | '<' -> (-1,  0)
    |  _  -> failwith "Undefined"

let nextHouse (x,y) c =
    let m = move c
     in (x + fst m, y + snd m)

let directions =
  @"c:/users/jeff.maner/source/repos/fsharp/adventofcode/03.input"
  |> System.IO.File.ReadAllText
  |> List.ofSeq

// Part One.
let deliver ds =
    let initialHouse = (0,0)
    let rec visitHouses h ds' =
        match ds' with
        | []      -> []
        | d::ds'' -> let house = nextHouse h d
                      in house :: visitHouses house ds''

    let houses = visitHouses initialHouse ds
     in initialHouse :: houses |> (List.distinct >> List.length)

let test1 = 2 = deliver ['>']
let test2 = 4 = deliver ['^';'>';'v';'<']
let test3 = 2 = deliver ['^';'v';'^';'v';'^';'v';'^';'v';'^';'v']

List.reduce (&&) [test1; test2; test3]

deliver directions // 2592

// Part Two.
let deliverWithHelp ds =
    let initialHouse = (0,0)
    let rec visitHouses santaHouse roboHouse ds' =
        match ds' with
        | []          -> []
        | (s,r)::ds'' -> let santaHouse' = nextHouse santaHouse s
                         let roboHouse' = nextHouse roboHouse r
                          in santaHouse' :: roboHouse' :: visitHouses santaHouse' roboHouse' ds''
    let toTuple ss = match ss with
                     | x::y::[] -> (x,y)
                     | _        -> failwith "Undefined"

    let newDirections = List.chunkBySize 2 ds |> List.map toTuple
    let houses = visitHouses initialHouse initialHouse newDirections
     in initialHouse :: initialHouse :: houses |> (List.distinct >> List.length)

let test4 =  3 = deliverWithHelp ['^';'v']
let test5 =  3 = deliverWithHelp ['^';'>';'v';'<']
let test6 = 11 = deliverWithHelp ['^';'v';'^';'v';'^';'v';'^';'v';'^';'v']

List.reduce (&&) [test4; test5; test6]

deliverWithHelp directions // 2360

// vim:ft=fs
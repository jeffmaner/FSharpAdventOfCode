module Day10

#load "ancillary.fsx"
open Ancillary

let inline head xs = List.head xs

let input = "1113122113"

let lookAndSay ns i =
    let some = (<) 0 << length
    let count xs = (length xs, head xs)
    let norm t = (string (fst t)) + (string (snd t))
    let join (xs:string seq) = System.String.Join("", xs)
    let g = List.rev >> filter some >> map count >> map norm >> join
    let rec f (cs:char list) a b =
        match cs with
        | x::y::xs when x=y && length a > 0 && x=head a -> f (y::xs) (x::a) b
        | x::y::xs when x=y                             -> f (y::xs) [x]    (a::b)
        | x::y::xs when length a > 0 && x=head a        -> f  xs     [y]    ((x::a)::b)
        | x::y::xs                                      -> f (y::xs) []     ([x]::(a::b))
        | x::[]    when length a > 0                    -> f []      []     ((x::a)::b)
        | x::[]                                         -> f []      []     ([x]::b)
        | x::xs                                         -> f xs      [x]    (a::b)
        | []                                            -> a::b
     in f (charList ns) [] [] |> g

let partOne = (input,[1..40]) ||> fold lookAndSay |> length // 360154
let partTwo = (input,[1..50]) ||> fold lookAndSay |> length // 5103798

// vim:ft=fs
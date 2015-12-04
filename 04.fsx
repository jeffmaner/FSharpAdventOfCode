module Day04

open System.Security.Cryptography

let md5 (bytes:byte array) =
    use md5 = MD5.Create ()
     in ("", md5.ComputeHash bytes)
        ||> Array.fold (fun s b -> s + (b.ToString "x2"))

// let hash = md5 "Hello, World!"B

let lowestPositiveNumber requirement key =
    let byteArray = Array.ofSeq >> Array.map byte
    let rec lpn n =
        let hash = byteArray (key + string n) |> md5
         in if hash.StartsWith requirement
            then n
            else lpn (n+1)

     in lpn 1

// lowestPositiveNumber "00000" "abcdef" // 609043
// lowestPositiveNumber "00000" "pqrstuv" // 1048970

let key = "bgvyzdsv"

lowestPositiveNumber "00000"  key // 254575
lowestPositiveNumber "000000" key // 1038736

// vim:ft=fs
module JsonParser
// Yanked from http://www.fssnip.net/bq.

(*[omit:(Parser Monad omitted. Code available here: https://bitbucket.org/ZachBray/parsad)]*)

open System.Text.RegularExpressions
open System

type text = string
type error = string

[<AutoOpen>]
module String =
   let isEmpty(str:string) =
      str.Trim().Length = 0

   let (|Empty|_|) str =
      if isEmpty str then Some()
      else None

type 'a Parser = 
   | Parser of (text -> error ref -> ('a * text) option)
   member x.Evaluate(text, error) =
      let (Parser f) = x
      f text error
   member x.Parse text =
      let error = ref ""
      match x.Evaluate(text, error) with
      | Some (y, Empty) -> y
      | Some _ | None ->
         failwith !error

type ParserBuilder() =
   let parse patterns text =
      let pattern = 
         patterns |> Seq.map (sprintf "(%s)")
         |> String.concat ""
      let regex = Regex (sprintf "^\s*%s" pattern, RegexOptions.Singleline)
      let matchAttempt = regex.Match text
      let groups =
         [ for group in matchAttempt.Groups -> group.Value ]
      match groups with
      | [] -> []
      | x::xs -> xs

   let parsePattern pattern (f:string -> 'a Parser) text error =
      match text |> parse [pattern; ".*"]  with
      | [value; rest] ->
         let g = f value
         g.Evaluate(rest, error)
      | _ -> 
         error := sprintf "Expected '%s' but found '%s'" pattern text
         None
         
   let parseInfix (left:unit -> 'a Parser) op (right:unit -> 'b Parser) (f:('a*string*'b) -> 'c Parser) text error =
      match text |> parse [".*"; op; ".*"] with
      | [x; op; y] ->
         match left().Evaluate(x, error) with
         | Some(x, Empty) ->
            match right().Evaluate(y, error) with
            | Some(y, rest) -> 
               f(x, op, y).Evaluate(rest, error)
            | None -> 
               error := sprintf "Expected expression but found '%s'" y
               None
         | Some _ | None ->
            error := sprintf "Expected expression but found '%s'" x
            None
      | _ ->
         error := sprintf "Expected '<x> %s <y>' but found '%s'" op text
         None

   let parseAny (parsers:(unit -> 'a Parser) list) (f: 'a -> 'b Parser) text error =
      parsers |> Seq.tryPick (fun parser ->
         match parser().Evaluate(text, error) with
         | Some(x, rest) ->
            let g = f x
            g.Evaluate(rest, error)
         | None -> None
      )

   member b.Bind (parsers, f) =
      Parser(parseAny parsers f)
   member b.Bind ((left, op, right), f) =
      Parser(parseInfix left op right f)
   member b.Bind (parser, f) = 
      b.Bind([parser], f)
   member b.Bind (pattern:string, f) = 
      Parser(parsePattern pattern f)
   member b.Return x =
      Parser(fun text error -> Some(x, text))
   member b.ReturnFrom(parsers:_ list) =
      b.Bind(parsers, b.Return)
   member b.ReturnFrom(parser:unit -> _ Parser) =
      b.Bind(parser, b.Return)

let parser = ParserBuilder()

(*[/omit]*)

type expr =
   | JSObject of Map<string, expr>
   | JSNumber of float
   | JSString of string
   | JSArray of expr list
   | JSBool of bool
   | JSNull

let jsBool () = parser {
   let! b = "true|false"
   return JSBool <| Boolean.Parse b
}

let jsNumber() = parser {
   let! n = "[-+]?[0-9]*\.?[0-9]+"
   return JSNumber <| Double.Parse n
}

let jsString() = parser {
   let! str = "\"[^\"]*\""
   return JSString <| str
}

let jsNull() = parser {
   let! _ = "null"
   return JSNull
}

let justAn (pattern:string) (k:unit -> _) () = parser {
   let! _ = pattern
   return! k
}

let followedBy (pattern:string) (k:'a -> unit -> _) (f:unit -> _) () = parser {
   let! x = f
   let! _ = pattern
   return! k x
}

let listOf (startSymbol:string) f endSymbol () =
   let rec listOf (xs:_ list) (f:unit -> _) () = 
      parser {
         return! [
            f |> followedBy "," (fun x -> listOf (x::xs) f)
            f |> followedBy endSymbol (fun x () -> parser { return x::xs })
            justAn endSymbol (fun () -> parser { return xs })
         ]
      }
   parser {
      let! _ = startSymbol
      return! listOf List.empty f
   }

let jsArray f () = parser {
   let! xs = listOf "\[" f "\]"
   return JSArray xs
}

let jsAssign (varValue:unit -> _) () = parser {
   let! name = "\"[^\"]*\""
   let! _ = "\:"
   let! value = varValue
   return name, value
}

let jsObj f () = parser {
   let! assignments = listOf "\{" (jsAssign f) "\}"
   return assignments |> Map.ofList |> JSObject
}

let rec json () = parser {
   return! [
      jsObj json
      jsArray json
      jsNumber
      jsString
      jsBool
      jsNull
   ]
}

// Example
let exampleJson = @"
{  ""name"": ""Zach"",
   ""age"": 24,
   ""isMale"": true,
   ""bosses"": [
      { ""name"": ""Phil"" },
      { ""name"": ""Mark"" }
   ]
}"

printfn "%A" (json().Parse exampleJson)

// vim:ft=fs
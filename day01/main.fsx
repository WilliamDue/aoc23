open System

let input =
  Console.In.ReadToEnd ()
  |> (fun a -> a.Split "\n")
  |> Array.filter (fun a -> a <> "")

let to_num a =
  List.map string [List.head a; List.last a]
  |> String.Concat
  |> int

try
  let only_digits = List.filter Char.IsDigit << Seq.toList
  input
  |> Array.map (to_num << only_digits)
  |> Array.fold (+) 0
  |> printfn "Part 1: %i"
with _ -> ()

type parser<'a> = list<char> -> option<'a * list<char>>

let (<|>) (f : 'a parser) (g : 'a parser) : 'a parser =
  fun str ->
    match f str with
      | Some (x, str') -> Some (x, str')
      | None -> g str

let get : char parser =
  fun str ->
    match str with
      | [] -> None
      | x :: xs -> Some (x, xs)

let rec get_string' (pattern : char list) : char list parser =
  (fun str ->
   match (str, pattern) with
   | (_, []) -> Some ([], str)
   | (x::_, y::_) when x = y ->
     get str
     |> Option.bind (
          fun (c, str') ->
            get_string' (List.tail pattern) str'
            |> Option.map (fun (cs, str'') -> (c::cs, str''))
          )
   | _ -> None
  )

let to_string : char list -> string =
  Array.ofList >> System.String.Concat

let get_string (pattern : string) : string parser =
  fun str ->
    get_string' (Seq.toList pattern) str
    |> Option.map (fun (a, str') -> (to_string a, str')) 

let rec find_parsable (p : 'a parser) : 'a list parser =
  fun str ->
    match (str, p str) with
      | ([], _) -> Some ([], [])
      | (_, None) -> List.tail str |> find_parsable p
      | (_, Some (c, str')) ->
          find_parsable p str'
          |> Option.map (fun (cs, str'') -> (c::cs, str''))                                                              

let get_digit : string parser =
  fun str ->
    match get str with
      | Some (a, str') when Char.IsDigit a -> Some (string a, str')
      | _ -> None

let numbers : string parser =
  get_string "one" <|>
  get_string "two" <|>
  get_string "three" <|>
  get_string "four" <|>
  get_string "five" <|>
  get_string "six" <|>
  get_string "seven" <|>
  get_string "eight" <|>
  get_string "nine" <|>
  get_digit

let parse_numbers : string -> string list =
  Seq.toList
  >> find_parsable numbers
  >> Option.defaultValue ([], [])
  >> fst

let to_number (str : string) : char =
  match str with
    | "one" -> '1'
    | "two" -> '2'
    | "three" -> '3'
    | "four" -> '4'
    | "five" -> '5'
    | "six" -> '6'
    | "seven" -> '7'
    | "eight" -> '8'
    | "nine" -> '9'
    | "1" -> '1'
    | "2" -> '2'
    | "3" -> '3'
    | "4" -> '4'
    | "5" -> '5'
    | "6" -> '6'
    | "7" -> '7'
    | "8" -> '8'
    | "9" -> '9'
    | _ -> failwith "This cannot happen."

let solve2 : string -> int =
  parse_numbers
  >> List.map to_number
  >> to_num

input
|> Array.map solve2
|> Array.fold (+) 0
|> printfn "Part 2: %A"

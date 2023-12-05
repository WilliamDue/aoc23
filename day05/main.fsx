open System
open System.Text.RegularExpressions

let rx = Regex(@"[0-9]+", RegexOptions.Compiled)

let findInts str =
  seq {
    for m in rx.Matches(str) do
      yield (bigint (uint64 m.Value))
  }

let parseMap (str : string) =
  let mappings =
    str
    |> findInts
    |> List.ofSeq
    |> List.chunkBySize 3
    |> List.map (fun ls -> (ls.[0], ls.[1], ls.[2]))

  mappings

let parse (str : string) =
  let splitted =
    str
    |> (fun a -> a.Split "\n\n")
    |> Array.filter (fun a -> a <> "")

  let seeds =
    splitted.[0]
    |> findInts
    |> List.ofSeq

  let mappings =
    splitted.[1..]
    |> Array.map parseMap
    |> List.ofArray

  (seeds, mappings)

let input =
  Console.In.ReadToEnd ()
  |> parse

let useMapping n (dest, src, range) =
  if src <= n && n < src + range
  then dest + (n - src) |> Some
  else None

let rec findMapping n ls =
  match ls with
    | [] -> n
    | x::xs ->
      match useMapping n x with
        | None -> findMapping n xs
        | Some v -> v

let (seeds,  maps) = input

let findLocation seed = List.fold findMapping seed maps

seeds
|> List.map findLocation
|> List.min
|> printfn "Part 1: %A" 

let maps' = List.rev maps

let useMapping' n (dest, src, range) =
  if dest <= n && n < dest + range
  then src + (n - dest) |> Some
  else None

let rec findMapping' n ls =
  match ls with
    | [] -> n
    | x::xs ->
      match useMapping' n x with
        | None -> findMapping' n xs
        | Some v -> v

let new_seeds =
  List.chunkBySize 2 seeds
  |> List.map (fun a -> (a.[0], a.[1]))

let isSeed seed =
  List.exists (fun (a, b) -> a <= seed && seed < a + b) new_seeds
  
let findSeed loc = List.fold findMapping' loc maps'

seq { 10000000UL .. 100000000UL }
|> Seq.map (findSeed << bigint)
|> Seq.tryFind isSeed
|> printfn "%A"

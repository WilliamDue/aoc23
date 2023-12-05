open System
open System.Text.RegularExpressions

let rx = Regex(@"[0-9]+", RegexOptions.Compiled)

let findInts str =
  seq {
    for m in rx.Matches(str) do
      yield (uint64 m.Value)
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

let (seeds, maps) = input

let findLocation seed = List.fold findMapping seed maps

seeds
|> List.map findLocation
|> List.min
|> printfn "Part 1: %A"

let newRange (a, b) (dest, src, range) =
  let lower = max a src
  let upper = min (a + b) (src + range)
  let m = if lower < a then [] else [(lower - a, lower)]
  let n = if upper < a + b then [] else [((a + b) - upper, upper)]
  let t = if upper <= lower then [] else [(dest + (lower - src), upper - lower)]
  m @ n @ t

let rec findMapping' ns ls =
  List.collect (fun n -> List.collect (newRange n) ls) ns

let new_seeds =
  seeds
  |> List.chunkBySize 2
  |> List.map (fun a -> (a.[0], a[1]))

let findLocations seeds' = List.fold findMapping' seeds' maps

new_seeds
|> findLocations
|> printfn "%A"

printfn "%A" (79, 79 + 14)
printfn "%A" (50, 50 + 48)

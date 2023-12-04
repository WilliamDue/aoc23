open System
open System.Text.RegularExpressions

let rx = Regex(@"[0-9]+", RegexOptions.Compiled)

let findInts str =
  seq {
    for m in rx.Matches(str) do
      yield (int m.Value)
  }

let parseLine (str : string) =
  let card_nums = str.Split "|"
  let card : string = (card_nums.[0].Split ":").[1]
  let nums : string = card_nums.[1]
  let toSet = Set.ofSeq << findInts
  (toSet card, toSet nums)

let input =
  Console.In.ReadToEnd ()
  |> (fun a -> a.Split "\n")
  |> Array.filter (fun a -> a <> "")
  |> Array.map parseLine

let card_counts =
  input
  |> Array.map (fun (a, b) ->
                Set.intersect a b 
                |> Set.count)

card_counts
|> Array.map (fun a -> pown 2 (a - 1))
|> Array.fold (+) 0
|> printfn "Part 1: %i"

let indices =
  List.zip ([1..(Array.length card_counts)]) (List.ofArray card_counts)

let counts =
  Map.ofList indices
  |> Map.map (fun _ _ -> 1)

let aux c map j = 
  Map.change j (fun a ->
                match a with
                | Some s -> Some (s + c * 1)
                | None -> None) map

let folder counts (i, count) =
  if count = 0
  then counts
  else
    let value = Map.tryFind i counts |> Option.defaultValue 0
    List.fold (aux value) counts [i + 1..i + count]

List.fold folder counts indices
|> Map.toList
|> List.map snd
|> List.fold (+) 0
|> printfn "Part 2: %i"

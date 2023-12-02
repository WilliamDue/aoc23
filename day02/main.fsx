open System

type color = Red | Blue | Green

let try_insert key elem map =
  if Map.containsKey key map
  then map
  else Map.add key elem map

let parse_line (str : string) =
  let game_sets = str.Split ": "
  let game = game_sets.[0]
  let sets' = game_sets.[1]
  let game_id = int <| (game.Split " ").[1]

  let parse_elem (str' : string) : color * int =
    match str'.Split " " with
      | [|a; "red"|] -> (Red, int a)
      | [|a; "blue"|] -> (Blue, int a)
      | [|a; "green"|] -> (Green, int a)
      | _ -> failwith "This does not happen."    
  
  let sets =
    sets'.Split "; "
    |> List.ofArray
    |> List.map (fun (a : string) ->
                  a.Split ", "
                  |> Array.map parse_elem
                  |> Map.ofArray
                  |> try_insert Red 0
                  |> try_insert Blue 0
                  |> try_insert Green 0)
  
  (game_id, sets)

let max_draw = Map.ofList [(Red, 12); (Green, 13); (Blue, 14)]  

let is_possible config set =
  Map.forall (fun k e -> Map.find k set <= e) config

let input =
  Console.In.ReadToEnd ()
  |> (fun a -> a.Split "\n")
  |> List.ofArray
  |> List.filter (fun a -> a <> "")
  |> List.map parse_line

input
|> List.filter (List.forall (is_possible max_draw) << snd)
|> List.map fst
|> List.fold (+) 0
|> printfn "Part 1: %i"

let max_set set set' =
  Map.map (fun k v -> max v (Map.find k set')) set

let identity = Map.ofList [(Red, 0); (Blue, 0); (Green, 0)]

input
|> List.map (List.fold (*) 1
             << List.map snd
             << Map.toList
             << List.fold max_set identity
             << snd)
|> List.fold (+) 0
|> printfn "Part 2: %i"

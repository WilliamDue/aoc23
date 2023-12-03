open System

let input =
  Console.In.ReadToEnd ()
  |> (fun a -> a.Split "\n")
  |> Array.filter (fun a -> a <> "")
  |> Array.map Seq.toArray

let height = Array.length input
let width = Array.length input.[0] 

let indices = List.allPairs [0..width - 1] [0..height - 1]

let rec chunkByPred' acc p ls =
  match acc, ls with
    | [], y::ys -> chunkByPred' (y::acc) p ys
    | x::_, y::ys when p x y -> chunkByPred' (y::acc) p ys
    | _, y::ys -> acc :: chunkByPred' [y] p ys
    | _, [] -> if List.isEmpty acc then [] else [acc]

let rec chunkByPred p ls =
  chunkByPred' [] p ls
  |> List.map List.rev

let isOneMore (a, i) (b, j) = i + 1 = j && a = b

let neighbours (i, j) =
  List.allPairs [-1..1] [-1..1]
  |> List.except [(0, 0)]
  |> List.map (fun (x, y) -> (i + x, j + y))
  |> List.filter (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height)

let isSymbol (i, j) =
  let v = input.[i].[j]
  not (Char.IsDigit v) && v <> '.'

let isNeighbourSymbol =
  List.exists isSymbol << neighbours

let toString : char list -> string =
  Array.ofList >> String.Concat

let toInt =
  List.map (fun (i, j) -> input.[i].[j])
  >> toString
  >> int

let numberIndices =
  List.partition (fun (i, j) -> Char.IsDigit input.[i].[j] ) indices
  |> fst
  |> chunkByPred isOneMore

numberIndices
|> List.filter (List.exists isNeighbourSymbol)
|> List.map toInt
|> List.fold (+) 0
|> printfn "Part 1: %i"

let exactly2 ls p =
  let overlap = neighbours p
  List.filter (fun a -> List.exists (fun b -> List.exists (fun c -> c = b) a) overlap) ls

List.filter (fun (i, j) -> input.[i].[j] = '*') indices
|> List.map (exactly2 numberIndices)
|> List.filter (fun a -> List.length a = 2)
|> List.map (List.fold (*) 1 << List.map toInt)
|> List.fold (+) 0
|> printfn "Part 2: %i"

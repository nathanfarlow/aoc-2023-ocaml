open! Core
open! Common

type card = { winning : int list; mine : int list }

let num_matching card =
  let winning = Set.of_list (module Int) card.winning in
  List.filter card.mine ~f:(Set.mem winning) |> List.length

let part1 cards =
  List.map cards ~f:num_matching
  |> List.filter ~f:(( < ) 0)
  |> List.sum (module Int) ~f:(fun n -> Int.pow 2 (n - 1))
  |> printf "%d\n"

let part2 cards =
  let cards = Array.of_list cards in
  let num_copies =
    Memo.recursive ~hashable:Int.hashable (fun num_copies i ->
        match get_opt cards i with
        | Some card ->
            let num_direct = num_matching card in
            let num_indirect =
              List.range ~start:`exclusive ~stop:`inclusive i (i + num_direct)
              |> List.map ~f:num_copies
              |> List.sum (module Int) ~f:Fn.id
            in
            num_direct + num_indirect
        | None -> 0)
  in
  Array.mapi cards ~f:(fun i _ -> 1 + num_copies i)
  |> Array.sum (module Int) ~f:Fn.id
  |> printf "%d\n"

let parse s =
  trim_and_split_lines s
  |> List.map ~f:(fun line ->
         split_at ~on:": +" ~i:1 line
         |> split ~on:" | "
         |> List.map ~f:(fun s -> split ~on:" +" s |> List.map ~f:int_of_string)
         |> function
         | [ winning; mine ] -> { winning; mine }
         | _ -> assert false)

let () = run (with_input_file ~part1 ~part2 ~parse)

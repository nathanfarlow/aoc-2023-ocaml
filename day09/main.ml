open! Core
open! Common

let rec pairs = function
  | [] -> []
  | [ _ ] -> []
  | a :: b :: t -> (a, b) :: pairs (b :: t)

let rec next_value row ~f =
  let diffs = pairs row |> List.map ~f:(fun (a, b) -> b - a) in
  if List.for_all row ~f:(( = ) 0) then 0 else f row (next_value diffs ~f)

let go ~f = List.sum (module Int) ~f:(next_value ~f) >> printf "%d\n"

let part1 =
  go ~f:(fun row next_value ->
      let last_value = List.last_exn row in
      last_value + next_value)

let part2 =
  go ~f:(fun row next_value ->
      let first_value = List.hd_exn row in
      first_value - next_value)

let parse =
  trim_and_split_lines >> List.map ~f:(split >> List.map ~f:Int.of_string)

let () = run (with_input_file ~part1 ~part2 ~parse)

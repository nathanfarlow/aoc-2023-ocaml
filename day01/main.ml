open! Core
open! Common

let part1 lines =
  let first_digit s = String.find s ~f:Char.is_digit |> Option.value_exn in
  let last_digit s = String.rev s |> first_digit in
  let f s = int_of_string (sprintf "%c%c" (first_digit s) (last_digit s)) in
  List.sum (module Int) lines ~f |> printf "%d\n"

let part2 lines =
  let dict =
    [
      ("one", "1");
      ("two", "2");
      ("three", "3");
      ("four", "4");
      ("five", "5");
      ("six", "6");
      ("seven", "7");
      ("eight", "8");
      ("nine", "9");
    ]
    @ List.init 9 ~f:(fun n ->
          let n = Int.to_string (n + 1) in
          (n, n))
    |> String.Map.of_alist_exn
  in
  let find_first s dict compare =
    let open Option.Let_syntax in
    Map.mapi dict ~f:(fun ~key:pattern ~data:_ ->
        String.substr_index_all s ~may_overlap:true ~pattern)
    |> Map.filter_map ~f:(List.min_elt ~compare)
    |> Map.to_alist
    |> List.min_elt ~compare:(Comparable.lift compare ~f:snd)
    >>| fst >>| Map.find_exn dict |> Option.value_exn
  in
  let f s =
    let first = find_first s dict Int.compare in
    let last = find_first s dict (Comparable.reverse Int.compare) in
    int_of_string [%string "%{first}%{last}"]
  in
  List.sum (module Int) lines ~f |> printf "%d\n"

let () = run (with_input_file ~part1 ~part2 ~preprocess:trim_and_split_lines)

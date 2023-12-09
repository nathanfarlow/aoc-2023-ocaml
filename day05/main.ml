open! Core
open! Common

type range = { source : int; dest : int; length : int }
type map = { to_ : string; ranges : range list }
type problem = { seeds : int list; maps : map Map.M(String).t }

let rec map ~from ~to_ i maps =
  if String.equal from to_ then i
  else
    let { ranges; to_ = from; _ } = Map.find_exn maps from in
    let new_i =
      List.find_map ranges ~f:(fun { source; dest; length } ->
          if i >= source && i < source + length then Some (dest + (i - source))
          else None)
      |> Option.value ~default:i
    in
    map ~from ~to_ new_i maps

let part1 problem =
  List.map problem.seeds ~f:(fun seed ->
      map ~from:"seed" ~to_:"location" seed problem.maps)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> printf "%d\n"

let part2 problem =
  (* You don't have to be smart if you're patient 😌 *)
  List.groupi problem.seeds ~break:(fun i _ _ -> i % 2 = 0)
  |> List.concat_map ~f:(function
       | [ start; len ] -> [ (start, len) ]
       | _ -> assert false)
  |> List.map ~f:(fun (start, len) ->
         Sequence.range start (start + len)
         |> Sequence.map ~f:(fun i ->
                map ~from:"seed" ~to_:"location" i problem.maps)
         |> Sequence.min_elt ~compare:Int.compare
         |> Option.value_exn)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> printf "%d\n"

let parse s =
  let chunks = split s ~on:"\n\n" in
  let seeds =
    chunks |> List.hd_exn |> split_at ~on:": " ~i:1 |> split
    |> List.map ~f:Int.of_string
  in
  let maps =
    List.tl_exn chunks
    |> List.map ~f:(fun s ->
           let lines = trim_and_split_lines s in
           let from, to_ =
             List.hd_exn lines |> split_at ~i:0 |> split ~on:"-to-" |> function
             | [ a; b ] -> (a, b)
             | _ -> assert false
           in
           let ranges =
             List.tl_exn lines
             |> List.map ~f:(fun s ->
                    split s |> List.map ~f:Int.of_string |> function
                    | [ a; b; c ] -> { dest = a; source = b; length = c }
                    | _ -> assert false)
           in
           (from, { to_; ranges }))
    |> Map.of_alist_exn (module String)
  in
  { seeds; maps }

let () = run (with_input_file ~part1 ~part2 ~parse)

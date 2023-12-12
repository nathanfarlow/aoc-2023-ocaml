(* [@@@warning "-26-27-32-69"] *)

open! Core
module Timer = Timer
module Math = Math

(* Infix things *)

include Option.Let_syntax

let ( >> ) f g x = g (f x)

(* Array utils *)

let get_opt arr i =
  if i < 0 || i >= Array.length arr then None else Some (Array.unsafe_get arr i)

(* String utils *)

let whitespace = "[ \t\n\r]+"
let split ?(on = whitespace) = Str.split (Str.regexp on)

let split_at ?(on = whitespace) ~i s =
  let split = split ~on s in
  match List.nth split i with
  | Some s -> s
  | None -> raise_s [%message "split_at" (split : string list) (i : int)]

let capture ~re s =
  try
    let _ = Str.search_forward (Str.regexp re) s 0 in
    Some (Str.matched_group 1 s)
  with _ -> None

let capture_exn ~re s =
  match capture ~re s with
  | Some s -> s
  | None -> failwith [%string "unable to match `%{re}` in `%{s}`"]

let remove_chars ~chars = String.filter ~f:(fun c -> not (String.mem chars c))

(* Runtime things *)

let trim_and_split_lines =
  String.strip >> String.split_lines >> List.map ~f:String.strip

let with_input_file ~part1 ~part2 ~parse =
  Command.basic ~summary:"Advent of code"
    (let%map_open.Command f =
       choose_one ~if_nothing_chosen:Raise
         [
           flag "-part1" no_arg ~doc:"Run part 1"
           |> map ~f:(fun b -> Option.some_if b part1);
           flag "-part2" no_arg ~doc:"Run part 2"
           |> map ~f:(fun b -> Option.some_if b part2);
         ]
     and input = anon ("input" %: string) in
     fun () -> In_channel.read_all input |> parse |> f)

let run = Command_unix.run

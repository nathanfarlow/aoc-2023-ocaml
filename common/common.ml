open! Core

let split ~on = Str.split (Str.regexp on)

let split_at s ~on ~i =
  let s = split ~on s in
  List.nth_exn s i

let capture ~re s =
  try
    let _ = Str.search_forward (Str.regexp re) s 0 in
    Some (Str.matched_group 1 s)
  with _ -> None

let capture_exn ~re s =
  match capture ~re s with
  | Some s -> s
  | None -> failwith [%string "unable to match `%{re}` in `%{s}`"]

let read_all file = In_channel.read_all file

let trim_and_split_lines s =
  String.strip s |> String.split_lines |> List.map ~f:String.strip

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
     fun () -> read_all input |> parse |> f)

let run = Command_unix.run

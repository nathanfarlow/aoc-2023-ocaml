open! Core

let read_all file = In_channel.read_all file

let with_input_file ?(preprocess = Fn.id) ~part1 ~part2 () =
  Command.basic ~summary:"Generate an MD5 hash of the input data"
    (let%map_open.Command f =
       choose_one ~if_nothing_chosen:Raise
         [
           flag "-part1" no_arg ~doc:"Run part 1"
           |> map ~f:(fun b -> Option.some_if b part1);
           flag "-part2" no_arg ~doc:"Run part 2"
           |> map ~f:(fun b -> Option.some_if b part2);
         ]
     and input = anon ("input" %: string) in
     fun () -> read_all input |> preprocess |> f)

let run = Command_unix.run

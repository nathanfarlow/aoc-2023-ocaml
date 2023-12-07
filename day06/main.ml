open! Core
open! Common

type race = { time : int; dist : int }

(* No math just code*)
let both_parts =
  List.map ~f:(fun race ->
      List.range 0 race.time
      |> List.count ~f:(fun held_down ->
             held_down * (race.time - held_down) > race.dist))
  >> List.fold ~init:1 ~f:Int.( * )
  >> printf "%d\n"

let parse =
  trim_and_split_lines
  >> List.map
       ~f:
         (split_at ~on:": +" ~i:1 >> split ~on:" +" >> List.map ~f:int_of_string)
  >> (function
       | [ times; dists ] -> List.zip_exn times dists | _ -> assert false)
  >> List.map ~f:(fun (time, dist) -> { time; dist })

let () = run (with_input_file ~part1:both_parts ~part2:both_parts ~parse)

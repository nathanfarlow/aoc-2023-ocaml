open! Core

let state = ref 0.
let start () = state := Core_unix.gettimeofday ()

let delta () =
  let now = Core_unix.gettimeofday () in
  now -. !state

let lap () =
  let now = Core_unix.gettimeofday () in
  let delta = now -. !state in
  state := now;
  delta

let lap_display ?ops () =
  let delta = lap () in
  (match ops with
  | None -> printf "lap: %fs\n" delta
  | Some operations ->
      let ops_per_second = Int.to_float operations /. delta in
      printf "lap: %fs (%f ops/s)\n" delta ops_per_second);
  Out_channel.flush stdout

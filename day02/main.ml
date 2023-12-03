open! Core
open! Common

type round = { red : int option; green : int option; blue : int option }
type game = { id : int; rounds : round list }

let parse s =
  let parse_game line =
    let parse_round s =
      let get color =
        capture ~re:("\\([0-9]+\\) " ^ color) s |> Option.map ~f:int_of_string
      in
      { red = get "red"; green = get "green"; blue = get "blue" }
    in
    let id = capture_exn ~re:"Game \\(.*\\):" line |> int_of_string in
    let rounds =
      split_at line ~on:": " ~i:1 |> split ~on:";" |> List.map ~f:parse_round
    in
    { id; rounds }
  in
  trim_and_split_lines s |> List.map ~f:parse_game

let part1 games =
  List.filter games ~f:(fun { rounds; _ } ->
      let is_valid_round { red; green; blue } =
        let ( <= ) color value =
          Option.value_map color ~default:true ~f:(( >= ) value)
        in
        red <= 12 && green <= 13 && blue <= 14
      in
      List.for_all rounds ~f:is_valid_round)
  |> List.sum (module Int) ~f:(fun { id; _ } -> id)
  |> printf "%d\n"

let part2 games =
  List.sum
    (module Int)
    games
    ~f:(fun { rounds; _ } ->
      List.map rounds ~f:(fun { red; green; blue } -> (red, green, blue))
      |> List.unzip3
      |> fun (rs, gs, bs) ->
      let max_exn l =
        List.filter_opt l
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
      in
      max_exn rs * max_exn gs * max_exn bs)
  |> printf "%d\n"

let () = run (with_input_file ~part1 ~part2 ~parse)

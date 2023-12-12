(* [@@@warning "-26-27-32-69"] *)

open! Core
open! Common

module Direction = struct
  type t = Left | Right

  let of_char c =
    match c with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwithf "Invalid direction: %c" c ()
end

module Node = struct
  type t = { name : string; left : string; right : string }

  let get { left; right; _ } = function
    | Direction.Left -> left
    | Right -> right

  let of_string s =
    match remove_chars ~chars:"=(,)" s |> split with
    | [ name; left; right ] -> { name; left; right }
    | _ -> failwithf "Invalid node: %s" s ()
end

let path insns nodes ~start =
  let open Sequence.Generator in
  let rec aux insns cur_node =
    match Sequence.next insns with
    | None -> return ()
    | Some (dir, insns) ->
        let node = Map.find_exn nodes (Node.get cur_node dir) in
        yield node >>= fun () -> aux insns node
  in
  let start = Map.find_exn nodes start in
  yield start >>= fun () -> aux insns start

let part1 (insns, nodes) =
  path insns nodes ~start:"AAA"
  |> Sequence.Generator.run
  |> Sequence.take_while ~f:(fun n -> String.( <> ) n.Node.name "ZZZ")
  |> Sequence.length |> printf "%d\n"

let part2 (insns, nodes) =
  (* I paid for the whole computer, so I'm going to use the whole computer *)
  let unzip sequences =
    Sequence.unfold_step ~init:sequences ~f:(fun sequences ->
        let next = List.map sequences ~f:Sequence.next in
        if List.exists next ~f:Option.is_none then Done
        else
          let next = List.filter_opt next in
          Yield (List.map next ~f:fst, List.map next ~f:snd))
  in
  Map.keys nodes
  |> List.filter ~f:(String.is_suffix ~suffix:"A")
  |> List.map ~f:(fun start -> path insns nodes ~start)
  |> List.map ~f:Sequence.Generator.run
  |> unzip
  |> Sequence.take_while ~f:(fun nodes ->
         List.exists nodes ~f:(fun { name; _ } ->
             not @@ String.is_suffix name ~suffix:"Z"))
  |> Sequence.length |> printf "%d\n"

let parse s =
  split s ~on:"\n\n" |> function
  | [ instructions; nodes ] ->
      let insns =
        String.to_list instructions
        |> List.map ~f:Direction.of_char
        |> Sequence.cycle_list_exn
      in
      let nodes =
        trim_and_split_lines nodes |> List.map ~f:Node.of_string
        |> List.map ~f:(fun n -> (n.name, n))
        |> String.Map.of_alist_exn
      in
      (insns, nodes)
  | _ -> assert false

let () = run (with_input_file ~part1 ~part2 ~parse)

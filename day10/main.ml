[@@@warning "-26-27-32-69"]

open! Core
open! Common

let memo (type x) ?(hashable = Hashtbl.Hashable.poly) ~f ~on_cycle =
  let module X = Hashable.Make_plain_and_derive_hash_fold_t (struct
    type t = x

    let { Hashtbl.Hashable.hash; compare; sexp_of_t } = hashable
  end) in
  let memo = X.Table.create () in
  let evaluating = X.Hash_set.create () in
  let rec g x y =
    match Hashtbl.find memo x with
    | Some res -> res
    | None ->
        if Hash_set.mem evaluating x then on_cycle x
        else (
          Hash_set.add evaluating x;
          let data = f g x y in
          Hash_set.remove evaluating x;
          (* Don't cache computations if we are still evaluating another node.
             This ensures consistency across the cached results *)
          (* if Hash_set.is_empty evaluating then Hashtbl.set memo ~key:x ~data; *)
          Hashtbl.set memo ~key:x ~data;
          data)
  in
  g

type dir = N | E | S | W [@@deriving sexp]

let get arr i j = get_opt arr i >>= fun row -> get_opt row j

let part1 grid =
  let can_move_into i j dir =
    match get grid i j with
    | None -> false
    | Some c -> (
        match (c, dir) with
        | '|', (N | S)
        | '-', (E | W)
        | 'L', (N | E)
        | 'J', (N | W)
        | '7', (S | W)
        | 'F', (S | E)
        | 'S', _ ->
            true
        | _ -> false)
  in
  let distances =
    Array.make_matrix ~dimx:(Array.length grid)
      ~dimy:(Array.length grid.(0))
      100
  in
  let dfs dfs (i, j) dist =
    match get grid i j with
    | None -> ()
    | Some c ->
        print_endline (sprintf "%d %d %d" i j dist);
        distances.(i).(j) <- Int.min distances.(i).(j) dist;
        let move i j dir =
          if can_move_into i j dir then dfs (i, j) (dist + 1)
        in
        move (i - 1) j S;
        move (i + 1) j N;
        move i (j - 1) E;
        move i (j + 1) W
  in
  let i, j =
    Array.find_mapi_exn grid ~f:(fun i ->
        Array.find_mapi ~f:(fun j c -> Option.some_if (Char.equal c 'S') (i, j)))
  in
  memo ~f:dfs (i, j) 0 ~on_cycle:(fun _ -> ());
  let max_dist =
    Array.fold distances ~init:0 ~f:(fun acc row ->
        Array.fold row ~init:acc ~f:Int.max)
  in
  (* print the grid *)
  Array.iter distances ~f:(fun row ->
      Array.iter row ~f:(fun dist ->
          printf "%2d " dist;
          ());
      print_endline "");
  printf "%d\n" max_dist

let part2 _ = ()
(* let fib fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2) in *)
(* let fib = memo ~f:fib ~on_cycle:(fun _ -> 0) in *)
(* let fuck fuck x = *)
(*   fuck x; *)
(*   () *)
(* in *)
(* let fuck = memo ~f:fuck ~on_cycle:(fun _ -> ()) in *)
(* fuck (); *)
(* printf "%d\n" (fib 50) *)

let parse s =
  trim_and_split_lines s |> List.map ~f:String.to_array |> Array.of_list

let () = run (with_input_file ~part1 ~part2 ~parse)

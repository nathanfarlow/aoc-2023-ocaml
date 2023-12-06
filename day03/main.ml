open! Core
open! Common

let is_symbol c = not (Char.is_digit c || Char.equal c '.')

let get arr i j =
  get_opt arr i >>= (fun row -> get_opt row j) |> Option.value ~default:'.'

let get_adj i j =
  List.concat_map [ -1; 0; 1 ] ~f:(fun di ->
      List.map [ -1; 0; 1 ] ~f:(fun dj -> (i + di, j + dj)))

let read_line grid i =
  let rec aux j nums state =
    match (state, get_opt grid.(i) j) with
    | `Chilling, None -> nums
    | `Reading num, None -> num :: nums
    | `Chilling, Some c ->
        if Char.is_digit c then aux j nums (`Reading ((i, j), 0, []))
        else aux (j + 1) nums `Chilling
    | `Reading (start, n, adj), Some c ->
        let char_to_int = Char.to_string >> Int.of_string in
        if Char.is_digit c then
          let adj = adj @ get_adj i j in
          aux (j + 1) nums (`Reading (start, (n * 10) + char_to_int c, adj))
        else aux (j + 1) ((start, n, adj) :: nums) `Chilling
  in
  aux 0 [] `Chilling

let part1 grid =
  Array.mapi grid ~f:(fun i _ -> read_line grid i)
  |> Array.to_list |> List.concat
  |> List.filter_map ~f:(fun (_, n, adj) ->
         if List.exists adj ~f:(fun (i, j) -> is_symbol (get grid i j)) then
           Some n
         else None)
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"

module Int2 = struct
  type t = int * int

  include Tuple.Comparable (Int) (Int)
end

let part2 grid =
  Array.mapi grid ~f:(fun i _ -> read_line grid i)
  |> Array.to_list |> List.concat
  |> List.map ~f:(fun ((start_i, start_j), n, adj) ->
         List.filter_map adj ~f:(fun (i, j) ->
             if Char.equal (get grid i j) '*' then
               Some ((i, j), ((start_i, start_j), n))
             else None))
  |> List.concat
  |> Map.of_alist_multi (module Int2)
  |> Map.map
       ~f:(List.dedup_and_sort ~compare:(Comparable.lift Int2.compare ~f:fst))
  |> Map.map ~f:(fun l ->
         if List.length l = 2 then List.fold l ~init:1 ~f:(fun a p -> a * snd p)
         else 0)
  |> Map.data
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"

let parse =
  trim_and_split_lines >> List.to_array >> Array.map ~f:String.to_array

let () = run (with_input_file ~part1 ~part2 ~parse)

open! Core
open! Common

let is_symbol c = not (Char.is_digit c || Char.equal c '.')

let get arr i j =
  get_opt arr i >>= (fun row -> get_opt row j) |> Option.value ~default:'.'

let adjacent ~f i j =
  List.concat_map [ -1; 0; 1 ] ~f:(fun di ->
      List.map [ -1; 0; 1 ] ~f:(fun dj -> f (i + di) (j + dj)))

let is_adj grid i j = adjacent ~f:(get grid) i j |> List.exists ~f:is_symbol

let read_line grid i =
  let rec aux j nums state =
    match (state, get_opt grid.(i) j) with
    | `Chilling, None -> nums
    | `Reading num, None -> num :: nums
    | `Chilling, Some c ->
        if Char.is_digit c then aux j nums (`Reading (0, false))
        else aux (j + 1) nums `Chilling
    | `Reading (n, near_symbol), Some c ->
        let char_to_int = Char.to_string >> Int.of_string in
        if Char.is_digit c then
          let near_symbol = near_symbol || is_adj grid i j in
          aux (j + 1) nums (`Reading ((n * 10) + char_to_int c, near_symbol))
        else aux (j + 1) ((n, near_symbol) :: nums) `Chilling
  in
  aux 0 [] `Chilling
  |> List.filter_map ~f:(fun (n, near_symbol) ->
         if near_symbol then Some n else None)

let part1 grid =
  Array.mapi grid ~f:(fun i _ -> read_line grid i)
  |> Array.to_list |> List.concat
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"

let part2 _s = ()

let parse =
  trim_and_split_lines >> List.to_array >> Array.map ~f:String.to_array

let () = run (with_input_file ~part1 ~part2 ~parse)

open! Core
open! Common

module Card = struct
  type t = int [@@deriving compare]

  let jack = 11

  let of_char = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | c when Char.is_digit c -> Char.to_int c - Char.to_int '0'
    | _ -> assert false
end

module Hand = struct
  let counts t =
    List.sort_and_group t ~compare:Card.compare
    |> List.map ~f:List.length
    |> List.sort ~compare:(Comparable.reverse Int.compare)

  let compare =
    Comparable.lift [%compare: int list * Card.t list] ~f:(fun t ->
        (counts t, t))

  let of_string s = String.to_list s |> List.map ~f:Card.of_char
end

module Joker_hand = struct
  include Hand

  let compare =
    let f x =
      let num_jacks = List.count x ~f:(( = ) Card.jack) in
      let counts =
        List.filter x ~f:(( <> ) Card.jack) |> counts |> function
        | l when num_jacks = 0 -> l
        | x :: xs -> (x + num_jacks) :: xs
        | [] -> [ num_jacks ]
      in
      let weak_jokers =
        List.map x ~f:(fun n -> if n = Card.jack then 0 else n)
      in
      (counts, weak_jokers)
    in
    Comparable.lift [%compare: int list * Card.t list] ~f
end

let go compare players =
  List.sort players ~compare:(Comparable.lift compare ~f:fst)
  |> List.mapi ~f:(fun i (_hand, bid) -> bid * (i + 1))
  |> List.sum (module Int) ~f:Fn.id
  |> printf "%d\n"

let part1 = go Hand.compare
let part2 = go Joker_hand.compare

let parse s =
  trim_and_split_lines s
  |> List.map ~f:(fun line ->
         split line |> function
         | [ hand; bid ] -> (Hand.of_string hand, Int.of_string bid)
         | _ -> assert false)

let () = run (with_input_file ~part1 ~part2 ~parse)

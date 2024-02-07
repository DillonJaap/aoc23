(*
   Solution after looking at this solution:
   https://github.com/tjdevries/advent_of_code/blob/master/2023/bin/day03.ml
*)
open Core

type symbol =
  { col : int
  ; row : int
  }

let get_symbols_in_line line row =
  List.filter_mapi (String.to_list line) ~f:(fun col ch ->
    match ch with
    | '0' .. '9' -> None
    | '.' -> None
    | _ -> Some { col; row })
;;

type part =
  { num : string
  ; col_start : int
  ; col_end : int
  ; row : int
  }

let get_parts_in_line line row =
  let make_part start finish =
    { num = String.slice line start finish; col_start = start; col_end = finish - 1; row }
  in
  let rec aux idx start acc =
    match idx, start with
    | idx, None when idx >= String.length line -> acc
    | idx, Some s when idx >= String.length line ->
      aux (idx + 1) None (make_part s idx :: acc)
    | _ ->
      (match String.get line idx, start with
       | '0' .. '9', None -> aux (idx + 1) (Some idx) acc
       | '0' .. '9', Some _ -> aux (idx + 1) start acc
       | _, Some s -> aux (idx + 1) None (make_part s idx :: acc)
       | _, None -> aux (idx + 1) start acc)
  in
  aux 0 None []
;;

let get_all fname ~getter =
  let lines = In_channel.read_lines fname in
  let rec aux lines row acc =
    match lines with
    | [] -> acc
    | hd :: tl -> aux tl (row + 1) (List.append acc (getter hd row))
  in
  aux lines 0 []
;;

let is_adjacent part symbol =
  symbol.col >= part.col_start - 1
  && symbol.col <= part.col_end + 1
  && symbol.row >= part.row - 1
  && symbol.row <= part.row + 1
;;

let get_stars_in_line line row =
  List.filter_mapi (String.to_list line) ~f:(fun col ch ->
    match ch with
    | '*' -> Some { col; row }
    | _ -> None)
;;

(* Part 1 *)
let () =
  let fname = "./inputs/day3.txt" in
  let symbols = get_all ~getter:get_symbols_in_line fname in
  let parts = get_all ~getter:get_parts_in_line fname in
  let total =
    List.fold parts ~init:0 ~f:(fun count part ->
      let is_part =
        List.fold symbols ~init:false ~f:(fun acc symbol ->
          if is_adjacent part symbol then true else acc)
      in
      match is_part with
      | true -> count + Int.of_string part.num
      | false -> count)
  in
  Format.print_int total
;;

let () =
  let fname = "./inputs/day3.txt" in
  let stars = get_all ~getter:get_stars_in_line fname in
  let parts = get_all ~getter:get_parts_in_line fname in
  let total =
    List.fold stars ~init:0 ~f:(fun sum star ->
      let ratios =
        List.fold parts ~init:[] ~f:(fun acc part ->
          match is_adjacent part star with
          | true -> Int.of_string part.num :: acc
          | false -> acc)
      in
      match ratios with
      | [] | [ _ ] -> sum
      | _ -> List.fold ratios ~init:1 ~f:(fun acc r -> acc * r) + sum)
  in
  Format.printf "\n%d" total
;;

open Core

type card =
  { got_numbers : int list
  ; want_numbers : int list
  ; quantity : int
  }

let parse_line line =
  let line =
    match String.split ~on:':' line with
    | _ :: tl :: _ -> tl
    | _ -> failwith "Invalid line, split on :"
  in
  let lstring, rstring =
    match String.lsplit2 ~on:'|' line with
    | Some (r, l) -> r, l
    | None -> failwith "Invalid line, split on |"
  in
  let to_int s =
    match s with
    | "" -> None
    | s -> Some (Int.of_string s)
  in
  { got_numbers = String.split ~on:' ' lstring |> List.filter_map ~f:to_int
  ; want_numbers = String.split ~on:' ' rstring |> List.filter_map ~f:to_int
  ; quantity = 1
  }
;;

let _ =
  let fname = "inputs/day4.txt" in
  let lines = In_channel.read_lines fname in
  let points =
    List.fold lines ~init:0 ~f:(fun acc l ->
      let card = parse_line l in
      let line_total =
        List.fold card.got_numbers ~init:0 ~f:(fun card_total n ->
          let res = List.exists card.want_numbers ~f:(fun want -> want = n) in
          match res, card_total with
          | false, _ -> card_total
          | _, 0 -> 1
          | _, _ -> card_total * 2)
      in
      acc + line_total)
  in
  printf "\n~~~~~ Day 4 Part1, total points: %d ~~~~~\n" points
;;

let add_n_cards list idx n =
  List.mapi list ~f:(fun i card ->
    match i with
    | _ when i = idx -> { card with quantity = card.quantity + n }
    | _ -> card)
;;

let add_cards_to_next_n l start_idx n ~amount =
  let end_idx =
    match start_idx + n with
    | _ when start_idx + n > List.length l -> List.length l
    | _ -> start_idx + n
  in
  let rec aux cur_idx end_idx l =
    match cur_idx with
    | cur_idx when cur_idx < end_idx ->
      aux (cur_idx + 1) end_idx (add_n_cards l cur_idx amount)
    | _ -> l
  in
  aux start_idx end_idx l
;;

let process_cards cards =
  let rec aux cards cur_idx =
    let card =
      match List.nth cards cur_idx with
      | Some card -> card
      | None -> failwith (Printf.sprintf "Card index %d out of bounds" cur_idx)
    in
    let total_matches =
      (* // todo figure out how to use this *)
      let got = Set.of_list (module Int) card.got_numbers in
      let want = Set.of_list (module Int) card.want_numbers in
      Set.inter got want |> Set.length
    in
    printf "Total matches for card at index %d is %d\n" cur_idx total_matches;
    let cards =
      add_cards_to_next_n cards (cur_idx + 1) total_matches ~amount:card.quantity
    in
    if cur_idx = List.length cards - 1 then cards else aux cards (cur_idx + 1)
  in
  aux cards 0
;;

let _ =
  let fname = "inputs/day4.txt" in
  let lines = In_channel.read_lines fname in
  let cards =
    List.fold lines ~init:[] ~f:(fun acc l -> parse_line l :: acc)
    |> List.rev
    |> process_cards
  in
  let total =
    List.foldi cards ~init:0 ~f:(fun i acc card ->
      printf "cards for card %d has quantity %d\n" (i + 1) card.quantity;
      acc + card.quantity)
  in
  printf "\n~~~~~ Day 4 Part2, total cards: %d ~~~~~\n" total
;;

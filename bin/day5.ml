open Core

type range =
  { dest_start : int
  ; source_start : int
  ; length : int
  }
(* [@@deriving show] *)

type map =
  { input : string
  ; output : string
  ; ranges : range list
  }
(* [@@deriving show]*)

let in_source_range r i = i >= r.source_start && i <= r.source_start + r.length
let _in_dest_range r i = i >= r.dest_start && i <= r.dest_start + r.length

let get_dest_number r i =
  if not (i >= r.dest_start && i <= r.dest_start + r.length)
  then (
    print_endline "hello";
    -1)
  else (
    printf "%d\n" (i - r.source_start + r.dest_start);
    i - r.source_start + r.dest_start)
;;

(* TODO update this on use fold_until *)
let map_get map seed =
  List.fold map.ranges ~init:None ~f:(fun acc r ->
    match in_source_range r seed, acc with
    | _, Some s -> Some s
    | true, _ -> Some (get_dest_number r seed)
    | false, _ -> None)
;;

(* TODO update this on use fold_until *)
let get_location maps seed =
  List.fold maps ~init:seed ~f:(fun s m ->
    match map_get m s with
    | None ->
      print_endline "hello again";
      -1
    | Some v -> v)
;;

let parse_seeds line =
  let pat = Re.Perl.compile_pat {|\d+|} in
  Re.matches pat line |> List.map ~f:(fun a -> Int.of_string a)
;;

let parse_map s =
  let parse_from_to map =
    let res =
      String.chop_suffix_if_exists map ~suffix:" map:"
      |> String.substr_replace_all ~pattern:"-to-" ~with_:" "
      |> String.lsplit2 ~on:' '
    in
    match res with
    | None -> Error "failed on parse from-to line"
    | Some s -> Ok s
  in
  let parse_ranges ranges =
    List.fold ranges ~init:[] ~f:(fun acc range ->
      let vals = String.split range ~on:' ' in
      let dest_start, source_start, length =
        match List.nth vals 0, List.nth vals 1, List.nth vals 2 with
        | Some dest, Some source, Some length ->
          Int.of_string dest, Int.of_string source, Int.of_string length
        | _, _, _ -> 0, 0, 0
      in
      { dest_start; source_start; length } :: acc)
  in
  let lines = String.split_lines s in
  let ranges = List.tl lines in
  let map = List.hd lines in
  match map, ranges with
  | None, None | None, _ | _, None -> Error "failed to parse map - no lines parsed"
  | Some map, Some ranges ->
    let in_out = parse_from_to map in
    let ranges = parse_ranges ranges in
    (match in_out, ranges with
     | Error e, _ -> Error e
     | Ok (input, output), ranges -> Ok { input; output; ranges })
;;

let () =
  let data =
    In_channel.read_all "./inputs/day5.txt"
    |> String.substr_replace_all ~pattern:"\n\n" ~with_:"|"
    |> String.split ~on:'|'
  in
  let _seeds =
    parse_seeds
      (match List.hd data with
       | None -> ""
       | Some s -> s)
  in
  print_endline "";
  (*
     List.iter
     (match List.tl data with
     | None -> []
     | Some s -> s)
     ~f:(fun d ->
     match parse_map d with
     | Error e -> print_endline e
     | Ok m -> print_endline (show_map m));
  *)
  let maps =
    List.map
      (match List.tl data with
       | None -> []
       | Some s -> s)
      ~f:(fun d ->
        match parse_map d with
        | Error _ -> { input = ""; output = ""; ranges = [] }
        | Ok m -> m)
  in
  printf "%d\n" (get_location maps 79)
;;

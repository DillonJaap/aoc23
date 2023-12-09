open Core

module Part1 = struct
  let get_num line =
    let first = String.find line ~f:(fun c -> Char.is_digit c) in
    let last = String.rev line |> String.find ~f:(fun c -> Char.is_digit c) in
    match first, last with
    | None, None -> 0
    | Some n, None | None, Some n -> Char.to_int n
    | Some n, Some m -> Char.escaped n ^ Char.escaped m |> int_of_string
  ;;

  let run fname =
    let ints =
      let lines = In_channel.read_lines fname in
      List.fold lines ~init:[] ~f:(fun acc line -> get_num line :: acc)
    in
    List.fold ~init:0 ~f:(fun acc n -> acc + n) ints
  ;;
end

module Part2 = struct
  let num_map =
    [ "one", "1"
    ; "two", "2"
    ; "three", "3"
    ; "four", "4"
    ; "five", "5"
    ; "six", "6"
    ; "seven", "7"
    ; "eight", "8"
    ; "nine", "9"
    ]
  ;;

  let swap_min str =
    let _, min_str =
      List.fold
        num_map
        ~init:(-1, ("", ""))
        ~f:(fun acc num_map ->
          let indexes =
            String.substr_index_all str ~may_overlap:true ~pattern:(fst num_map)
          in
          match List.min_elt indexes ~compare:Int.compare with
          | None -> acc
          | Some min -> if fst acc = -1 || min < fst acc then min, num_map else acc)
    in
    String.substr_replace_first str ~pattern:(fst min_str) ~with_:(snd min_str)
  ;;

  let swap_max str =
    let _, max_str =
      List.fold
        num_map
        ~init:(-1, ("", ""))
        ~f:(fun acc num_map ->
          let indexes =
            String.substr_index_all str ~may_overlap:true ~pattern:(fst num_map)
          in
          match List.max_elt indexes ~compare:Int.compare with
          | None -> acc
          | Some max -> if fst acc = -1 || max > fst acc then max, num_map else acc)
    in
    String.substr_replace_all str ~pattern:(fst max_str) ~with_:(snd max_str)
  ;;

  let get_num line =
    let first = swap_min line |> String.find ~f:(fun c -> Char.is_digit c) in
    let last = swap_max line |> String.rev |> String.find ~f:(fun c -> Char.is_digit c) in
    match first, last with
    | None, None -> 0
    | Some n, None | None, Some n -> Char.to_int n
    | Some n, Some m -> Char.escaped n ^ Char.escaped m |> int_of_string
  ;;

  let run fname =
    let ints =
      let lines = In_channel.read_lines fname in
      List.fold lines ~init:[] ~f:(fun acc line -> get_num line :: acc)
    in
    List.fold ~init:0 ~f:(fun acc n -> acc + n) ints
  ;;
end

let () =
  printf "\nDay 1:\n";
  Part1.run "inputs/day1.txt" |> printf "%d\n";
  Part2.run "inputs/day1.txt" |> printf "%d\n"
;;

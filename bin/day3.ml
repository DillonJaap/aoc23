open Core

type symbol = int * int

type acc_input =
  { cur_x : int
  ; cur_y : int
  ; cords : symbol list
  }

let cordinates_of_symbols input =
  let result =
    String.fold input ~init:{ cur_x = 0; cur_y = 0; cords = [] } ~f:(fun acc ch ->
      match ch with
      | '\n' -> { acc with cur_x = 0; cur_y = acc.cur_y + 1 }
      | c when (not (Char.is_digit c)) && (not (Char.is_alpha c)) && Char.( <> ) c '.' ->
        { acc with cur_x = acc.cur_x + 1; cords = (acc.cur_x, acc.cur_y) :: acc.cords }
      | _ -> { acc with cur_x = acc.cur_x + 1 })
  in
  result.cords
;;

type part =
  { num : string
  ; cords : int * int * int
  }

type acc_input2 =
  { cur_x : int
  ; cur_y : int
  ; cur_part : string
  ; start_x : int
  ; cords : part list
  }

let cordinates_of_parts input =
  let result =
    String.fold
      input
      ~init:{ cur_x = 0; cur_y = 0; start_x = -1; cur_part = ""; cords = [] }
      ~f:(fun acc ch ->
        match ch with
        | '\n' ->
          (match acc.start_x with
           | -1 -> { acc with cur_x = 0; cur_y = acc.cur_y + 1 }
           | _ ->
             { cur_x = 0
             ; cur_y = acc.cur_y + 1
             ; start_x = -1
             ; cur_part = ""
             ; cords =
                 { num = acc.cur_part; cords = acc.start_x, acc.cur_x - 1, acc.cur_y }
                 :: acc.cords
             })
        | c when Char.is_digit c ->
          (match acc.start_x with
           | -1 ->
             { acc with
               cur_x = acc.cur_x + 1
             ; start_x = acc.cur_x
             ; cur_part = Char.escaped c
             }
           | _ ->
             { acc with cur_x = acc.cur_x + 1; cur_part = acc.cur_part ^ Char.escaped c })
        | _ ->
          (match acc.start_x with
           | -1 -> { acc with cur_x = acc.cur_x + 1 }
           | _ ->
             { acc with
               cur_x = acc.cur_x + 1
             ; start_x = -1
             ; cur_part = ""
             ; cords =
                 { num = acc.cur_part; cords = acc.start_x, acc.cur_x - 1, acc.cur_y }
                 :: acc.cords
             }))
  in
  result.cords
;;

let is_adjacent (part : part) ~(symbols : symbol list) =
  let x_beg, x_end, y = part.cords in
  List.fold_until
    symbols
    ~init:false
    ~finish:(fun _ -> false)
    ~f:(fun _ sym ->
      let sym_x, sym_y = sym in
      if sym_x >= x_beg - 1 && sym_x <= x_end + 1 && sym_y >= y - 1 && sym_y <= y + 1
      then Stop true
      else Continue false)
;;

let () =
  let data = In_channel.read_all "./inputs/day3.txt" in
  let symbols = cordinates_of_symbols data in
  let parts = cordinates_of_parts data in
  let total =
    List.fold parts ~init:0 ~f:(fun count part ->
      if is_adjacent part ~symbols
      then (
        print_endline part.num;
        count + Int.of_string part.num)
      else count)
  in
  print_endline "";
  print_endline (string_of_int total)
;;

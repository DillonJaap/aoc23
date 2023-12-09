open Core

type aux_input =
  { cur_x : int
  ; cur_y : int
  ; cords : (int * int) list
  }

let cordinates_of_symbols input =
  let aux =
    String.fold input ~init:{ cur_x = 0; cur_y = 0; cords = [] } ~f:(fun acc ch ->
      match ch with
      | '#' | '*' | '$' ->
        { acc with cur_x = acc.cur_x + 1; cords = (acc.cur_x, acc.cur_y) :: acc.cords }
      | '\n' -> { acc with cur_x = 0; cur_y = acc.cur_y + 1 }
      | _ -> { acc with cur_x = acc.cur_x + 1 })
  in
  aux.cords
;;

let is_adjacent cords ~symbols =
  let x_beg, x_end, y = cords in
  List.fold_until symbols ~init:false ~f:(fun _ sym ->
    if sym.cur_x >= x_beg - 1
       && sym.cur_x <= x_end + 1
       && sym.cur_y >= y - 1
       && sym.cur_y <= y + 1
    then Stop true
    else Continue false)
;;

String.Search_pattern.

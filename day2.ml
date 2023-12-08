open Core

module Part1 = struct
  type cubes =
    { red : int
    ; blue : int
    ; green : int
    }

  type game =
    { num : int
    ; cube_sets : cubes list
    }

  let parse_line line =
    let parts = String.split ~on:':' line in
    let game_str = List.nth parts 0 in
    let cube_set_str = List.nth parts 1 in
    let game_num =
      match game_str with
      | None -> 0
      | Some s ->
        let m = Re.exec (Re.Posix.compile_pat {|([0-9]+)|}) s in
        Re.Group.get m 0 |> int_of_string
    in
    let num_of_color str ~color =
      let m = Re.exec_opt (Re.Posix.compile_pat (sprintf {|([0-9]+) %s|} color)) str in
      match m with
      | None -> 0
      | Some s ->
        (match Re.Group.get_opt s 1 with
         | None -> 0
         | Some s -> int_of_string s)
    in
    let cubes =
      match cube_set_str with
      | None -> [ { red = 0; blue = 0; green = 0 } ]
      | Some s ->
        String.split ~on:';' s
        |> List.fold ~init:[] ~f:(fun acc str ->
          { red = num_of_color str ~color:"red"
          ; blue = num_of_color str ~color:"blue"
          ; green = num_of_color str ~color:"green"
          }
          :: acc)
    in
    { num = game_num; cube_sets = cubes }
  ;;

  let is_possible ?(max_cubes = { red = 12; blue = 14; green = 13 }) input =
    not
      (max_cubes.red < input.red
       || max_cubes.blue < input.blue
       || max_cubes.green < input.green)
  ;;

  let highest_cubes cubes =
    List.fold cubes ~init:{ red = 0; blue = 0; green = 0 } ~f:(fun acc cubes ->
      { red = (if cubes.red > acc.red then cubes.red else acc.red)
      ; green = (if cubes.green > acc.green then cubes.green else acc.green)
      ; blue = (if cubes.blue > acc.blue then cubes.blue else acc.blue)
      })
  ;;

  let run fname =
    let lines = In_channel.read_lines fname in
    List.fold lines ~init:0 ~f:(fun acc l ->
      let cur_game = parse_line l in
      let highest = highest_cubes cur_game.cube_sets in
      if is_possible highest then acc + cur_game.num else acc)
  ;;
end

module Part2 = struct
  let power (cube_set : Part1.cubes) = cube_set.red * cube_set.blue * cube_set.green

  let run fname =
    let lines = In_channel.read_lines fname in
    List.fold lines ~init:0 ~f:(fun acc l ->
      let cur_game = Part1.parse_line l in
      let pow = Part1.highest_cubes cur_game.cube_sets |> power in
      acc + pow)
  ;;
end

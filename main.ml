open Core

let () =
  printf "\nDay 1:\n";
  Day1.Part1.run "inputs/day1.txt" |> printf "%d\n";
  Day1.Part2.run "inputs/day1.txt" |> printf "%d\n";
  printf "\nDay 2:\n";
  Day2.Part1.run "inputs/day2.txt" |> printf "%d\n";
  Day2.Part2.run "inputs/day2.txt" |> printf "%d\n"
;;

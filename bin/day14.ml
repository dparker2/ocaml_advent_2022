module Position = struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    if compare y1 y2 == 0 then compare x1 x2 else compare y1 y2
end

module Positions = Set.Make (Position)

type cave = { mutable sand : Positions.t; mutable rocks : Positions.t }

let rec range a b = if a > b then range b a else List.init (1 + b - a) (( + ) a)
let add_pos set pos = Positions.add pos set

let add_shape x1 y1 x2 y2 set =
  if x1 == x2 then
    range y1 y2 |> List.map (fun y -> (x1, y)) |> List.fold_left add_pos set
  else range x1 x2 |> List.map (fun x -> (x, y1)) |> List.fold_left add_pos set

let ints_of_pair string =
  string |> String.split_on_char ',' |> List.map int_of_string

let rec read_rocks set = function
  | s :: "->" :: e :: rest -> (
      match (ints_of_pair s, ints_of_pair e) with
      | [ x1; y1 ], [ x2; y2 ] ->
          read_rocks (add_shape x1 y1 x2 y2 set) (e :: rest)
      | _ -> invalid_arg "Invalid coord")
  | [ _ ] -> set
  | _ -> invalid_arg "Something wrong"

let read_input () =
  let rec rec_read_lines list =
    try rec_read_lines (String.split_on_char ' ' (read_line ()) :: list)
    with End_of_file -> list
  in
  rec_read_lines []

let _debug_cave cave =
  for y = 0 to 165 do
    for x = 450 to 550 do
      if Positions.mem (x, y) cave.sand then print_char 'o'
      else if Positions.mem (x, y) cave.rocks then print_char '#'
      else print_char '.'
    done;
    print_newline ()
  done

let is_air cave pos =
  not (Positions.mem pos cave.sand || Positions.mem pos cave.rocks)

let rec part1_sand cave maxy x y =
  if y > maxy then raise Exit
  else
    match
      [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
      |> List.filter (is_air cave)
    with
    | (x2, y2) :: _ -> part1_sand cave maxy x2 y2
    | [] -> (x, y)

let rec part2_sand cave floory x y =
  if y == floory then (x, y)
  else if Positions.mem (500, 0) cave.sand then raise Exit
  else
    match
      [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
      |> List.filter (is_air cave)
    with
    | (x2, y2) :: _ -> part2_sand cave floory x2 y2
    | [] -> (x, y)

let simulate cave sand_strategy =
  try
    while true do
      cave.sand <- Positions.add (sand_strategy 500 0) cave.sand
    done
  with Exit -> ()

let () =
  let cave = { sand = Positions.empty; rocks = Positions.empty } in
  cave.rocks <- read_input () |> List.fold_left read_rocks cave.rocks;
  let _, maxy = Positions.max_elt cave.rocks in
  simulate cave (part1_sand cave maxy);
  cave.sand |> Positions.cardinal |> Printf.printf "Part 1: %d\n";
  (* _debug_cave cave; *)
  cave.sand <- Positions.empty;
  simulate cave (part2_sand cave (maxy + 1));
  (* _debug_cave cave; *)
  cave.sand |> Positions.cardinal |> Printf.printf "Part 2: %d\n"

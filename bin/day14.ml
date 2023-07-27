let cave = Hashtbl.create 10000
let rec range a b = if a > b then range b a else List.init (1 + b - a) (( + ) a)
let add_rock pos = Hashtbl.add cave pos '#'

let add_shape x1 y1 x2 y2 =
  if x1 == x2 then
    range y1 y2 |> List.map (fun y -> (x1, y)) |> List.iter add_rock
  else range x1 x2 |> List.map (fun x -> (x, y1)) |> List.iter add_rock

let ints_of_pair string =
  string |> String.split_on_char ',' |> List.map int_of_string

let rec add_rocks = function
  | s :: "->" :: e :: rest -> (
      match (ints_of_pair s, ints_of_pair e) with
      | [ x1; y1 ], [ x2; y2 ] ->
          add_shape x1 y1 x2 y2;
          add_rocks (e :: rest)
      | _ -> invalid_arg "Invalid coord")
  | [ _ ] -> ()
  | _ -> invalid_arg "Something wrong"

let read_input () =
  let rec rec_read_lines list =
    try rec_read_lines (String.split_on_char ' ' (read_line ()) :: list)
    with End_of_file -> list
  in
  rec_read_lines []

let _debug_cave () =
  for y = 0 to 165 do
    for x = 450 to 550 do
      try print_char (Hashtbl.find cave (x, y))
      with Not_found -> print_char '.'
    done;
    print_newline ()
  done

let next_pos x y =
  [ (x, y + 1); (x - 1, y + 1); (x + 1, y + 1) ]
  |> List.filter (fun pos -> not (Hashtbl.mem cave pos))

let rec part1_sand maxy x y =
  if y > maxy then raise Exit
  else
    match next_pos x y with
    | (x2, y2) :: _ -> part1_sand maxy x2 y2
    | [] -> (x, y)

let rec part2_sand maxy x y =
  if y == maxy + 1 then (x, y)
  else if Hashtbl.mem cave (500, 0) then raise Exit
  else
    match next_pos x y with
    | (x2, y2) :: _ -> part2_sand maxy x2 y2
    | [] -> (x, y)

let simulate inputs sand_strategy =
  Hashtbl.reset cave;
  inputs |> List.iter add_rocks;
  let maxy = Hashtbl.fold (fun (_, y) _ c -> max c y) cave 0 in
  let i = ref 0 in
  try
    while true do
      Hashtbl.add cave (sand_strategy maxy 500 0) 'o';
      i := !i + 1
    done;
    !i
  with Exit -> !i

let () =
  let inputs = read_input () in
  simulate inputs part1_sand |> Printf.printf "Part 1: %d\n";
  (* _debug_cave (); *)
  simulate inputs part2_sand |> Printf.printf "Part 2: %d\n"
(* _debug_cave () *)

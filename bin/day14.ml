let make_cave () = Array.make_matrix 170 100 '.'
let cave = make_cave ()

let put_horizontal y x1 x2 =
  for i = min x1 x2 to max x1 x2 do
    cave.(y).(i - 450) <- '#'
  done

let put_vertical x y1 y2 =
  for j = min y1 y2 to max y1 y2 do
    cave.(j).(x - 450) <- '#'
  done

let ints_of_pair string =
  string |> String.split_on_char ',' |> List.map int_of_string

let rec read_rocks = function
  | s :: "->" :: e :: rest -> (
      match (ints_of_pair s, ints_of_pair e) with
      | [ x1; y1 ], [ x2; y2 ] ->
          if x1 == x2 then put_vertical x1 y1 y2 else put_horizontal y1 x1 x2;
          read_rocks (e :: rest)
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
  for y = 0 to Array.length cave - 1 do
    for x = 0 to Array.length cave.(0) - 1 do
      print_char cave.(y).(x)
    done;
    print_newline ()
  done

let simulate () =
  let rec fall i j =
    if cave.(i + 1).(j) == '.' then fall (i + 1) j
    else if cave.(i + 1).(j - 1) == '.' then fall (i + 1) (j - 1)
    else if cave.(i + 1).(j + 1) == '.' then fall (i + 1) (j + 1)
    else cave.(i).(j) <- 'o'
  in
  let rec drop_sand i =
    try
      fall 0 50;
      drop_sand i + 1
    with Invalid_argument _ -> i
  in
  drop_sand 0

let () =
  read_input () |> List.iter read_rocks;
  simulate () |> Printf.printf "Part 1: %d\n";
  _debug_cave ()

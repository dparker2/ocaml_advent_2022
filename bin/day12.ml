module FrontierNode = struct
  type t = int * int * int

  let compare (f1, i1, j1) (f2, i2, j2) =
    if (i1, j1) = (i2, j2) then 0
    else if f1 == f2 then compare (i1, j1) (i2, j2)
    else compare f1 f2
end

module Frontier = Set.Make (FrontierNode)

let read_map () =
  let rec read_row map =
    try
      let string = read_line () in
      let row = Array.init (String.length string) (String.get string) in
      read_row (row :: map)
    with End_of_file -> map
  in
  read_row [] |> List.rev |> Array.of_list

let find_start_end map =
  let s = ref (-1, -1) in
  let g = ref (-1, -1) in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if map.(i).(j) == 'S' then s := (i, j)
      else if map.(i).(j) == 'E' then g := (i, j)
    done
  done;
  (!s, !g)

let find_all_a map =
  let _, list =
    map
    |> Array.fold_left
         (fun (i, acc) arr ->
           let _, new_acc =
             arr
             |> Array.fold_left
                  (fun (j, acc) char ->
                    if char = 'S' || char = 'a' then (j + 1, (i, j) :: acc)
                    else (j + 1, acc))
                  (0, acc)
           in
           (i + 1, new_acc))
         (0, [])
  in
  list

let height = function
  | 'S' -> Char.code 'a'
  | 'E' -> Char.code 'z'
  | char -> Char.code char

let neighbors map i j =
  let maxh = height map.(i).(j) + 1 in
  [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
  |> List.filter (fun (x, y) ->
         try height map.(x).(y) <= maxh with Invalid_argument _ -> false)

let manhattan_dist (i, j) (x, y) = abs (x - i) + abs (y - j)

let min_path map starts goal =
  let dists =
    Array.make_matrix (Array.length map) (Array.length map.(0)) max_int
  in
  let expand i j frontier =
    neighbors map i j
    |> List.fold_left
         (fun frontier (x, y) ->
           let dist = dists.(i).(j) + 1 in
           if dist < dists.(x).(y) then (
             dists.(x).(y) <- dist;
             Frontier.add (dist + manhattan_dist (x, y) goal, x, y) frontier)
           else frontier)
         frontier
  in
  let rec rec_min_path frontier =
    let f, i, j = Frontier.min_elt frontier in
    if (i, j) = goal then f
    else Frontier.remove (f, i, j) frontier |> expand i j |> rec_min_path
  in
  List.iter
    (fun (i, j) ->
      try dists.(i).(j) <- 0
      with Invalid_argument _ -> Printf.printf "INVALID (%d,%d)\n" i j)
    starts;
  starts
  |> List.fold_left
       (fun acc (s1, s2) ->
         Frontier.add (manhattan_dist (s1, s2) goal, s1, s2) acc)
       Frontier.empty
  |> rec_min_path

let () =
  let map = read_map () in
  let start, goal = find_start_end map in
  min_path map [ start ] goal |> Printf.printf "Part 1: %d\n";
  min_path map (find_all_a map) goal |> Printf.printf "Part 2: %d\n"

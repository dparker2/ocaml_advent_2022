let part1_line = 2000000
let part2_distress_max = 4000000
let points_tuple sx sy bx by = (sx, sy, bx, by)

let read_input () =
  let rec rec_read list =
    try
      let points =
        Scanf.scanf "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d "
          points_tuple
      in
      rec_read (points :: list)
    with End_of_file -> list
  in
  rec_read []

let sensor_range (sx, sy, bx, by) = (sx, sy, abs (sx - bx) + abs (sy - by))

let intervals_at line (x, y, r) =
  let dx = r - abs (y - line) in
  if dx > 0 then Some (x - dx, x + dx) else None

let merge_intervals list =
  list
  |> List.sort (fun (l, _) (l2, _) -> compare l l2)
  |> List.fold_left
       (fun acc (l, h) ->
         match acc with
         | [] -> [ (l, h) ]
         | (l2, h2) :: rest ->
             if h2 < l - 1 then (l, h) :: acc else (l2, max h h2) :: rest)
       []

let fold_sum exclude acc (l, h) =
  let minus =
    List.filter (fun bx -> bx >= l && bx <= h) exclude |> List.length
  in
  acc + (1 + h - l) - minus

let fold_beacons_at aty acc (_, _, x, y) =
  if y = aty && not (List.exists (fun e -> e = x) acc) then x :: acc else acc

let in_distress x = x > 0 && x < part2_distress_max

let rec distress_x = function
  | [] -> None
  | (l, h) :: rest ->
      if in_distress l then Some (l - 1)
      else if in_distress h then Some (h + 1)
      else distress_x rest

let find_distress_beacon sensor_ranges =
  let pos = ref (-1, -1) in
  try
    for i = 0 to part2_distress_max do
      match
        sensor_ranges
        |> List.filter_map (intervals_at i)
        |> merge_intervals |> distress_x
      with
      | None -> ()
      | Some x ->
          pos := (x, i);
          raise Exit
    done;
    !pos
  with Exit -> !pos

let tuning_freq (x, y) = (x * 4000000) + y

let () =
  let points = read_input () in
  let beacons_on_line =
    points |> List.fold_left (fold_beacons_at part1_line) []
  in
  let sensor_ranges = points |> List.map sensor_range in
  sensor_ranges
  |> List.filter_map (intervals_at part1_line)
  |> merge_intervals
  |> List.fold_left (fold_sum beacons_on_line) 0
  |> Printf.printf "Part 1: %d\n";
  sensor_ranges |> find_distress_beacon |> tuning_freq
  |> Printf.printf "Part 2: %d\n"

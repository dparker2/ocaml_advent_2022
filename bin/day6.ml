(*
Let T(i) be the length of the unique run of s from s[0..i]
T(0) = 1
T(i) = min(1 + T(i-1), distance backwards to s[i])
*)

let find_unique_run line goal =
  let n = String.length line in
  let table = Array.make n 1 in
  let answer = ref 0 in
  (try
     for i = 1 to n - 1 do
       (match String.rindex_from_opt line (i - 1) line.[i] with
       | None -> table.(i) <- table.(i - 1) + 1
       | Some j -> table.(i) <- min (table.(i - 1) + 1) (i - j));
       if table.(i) == goal then (
         answer := i + 1;
         raise Exit (* Early exit as we aren't finding max run or anything*))
     done
   with Exit -> ());
  !answer

let () =
  let line = read_line () in
  let part1 = find_unique_run line 4 in
  Printf.printf "Part 1: %d\n" part1;
  let part2 = find_unique_run line 14 in
  Printf.printf "Part 2: %d\n" part2

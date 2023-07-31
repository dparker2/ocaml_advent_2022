type valve = { id : string; flow : int; tunnels : int list }
type state = { at : int; closed : int; released : int; minutes : int }

type problem = {
  graph : valve array;
  openables : int list;
  dists : int array array;
}

let floyd_warshall valves =
  let n = Array.length valves in
  let dist = Array.make_matrix n n n in
  for u = 0 to n - 1 do
    dist.(u).(u) <- 0;
    valves.(u).tunnels |> List.iter (fun v -> dist.(u).(v) <- 1)
  done;
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        dist.(i).(j) <- min dist.(i).(j) (dist.(i).(k) + dist.(k).(j))
      done
    done
  done;
  dist

let problem_of_input inputs =
  let tunnel_indices =
    List.map (fun vid ->
        Util.index_of (String.starts_with ~prefix:("Valve " ^ vid)) inputs)
  in
  let valve_of_input input =
    Scanf.sscanf input "Valve %s has flow rate=%d; %s %s to %s %s@!"
      (fun v f _ _ _ t ->
        { id = v; flow = f; tunnels = Util.list_of_str t |> tunnel_indices })
  in
  let flow_indices (i, l) v = (i + 1, if v.flow > 0 then i :: l else l) in
  let valves = inputs |> List.map valve_of_input |> Array.of_list in
  {
    graph = valves;
    openables = valves |> Array.fold_left flow_indices (0, []) |> snd;
    dists = floyd_warshall valves;
  }

let rec solve problem state answers =
  (match Hashtbl.find_opt answers state.closed with
  | Some x ->
      if state.released > x then
        Hashtbl.replace answers state.closed state.released
  | None -> Hashtbl.add answers state.closed state.released);
  let open_valve i v =
    let offset = 1 lsl i in
    if state.closed land offset = 0 then 0
    else
      let d = problem.dists.(state.at).(v) in
      let rmin = max 0 (state.minutes - d - 1) in
      solve problem
        {
          at = v;
          closed = state.closed lxor offset;
          released = state.released + (rmin * problem.graph.(v).flow);
          minutes = rmin;
        }
        answers
  in
  if state.minutes <= 0 || state.closed = 0 then state.released
  else
    problem.openables |> List.mapi open_valve
    |> List.fold_left max state.released

let find_part2 answers closed =
  Hashtbl.fold
    (fun k v acc ->
      match Hashtbl.find_opt answers (closed lxor k) with
      | Some v2 -> max (v + v2) acc
      | None -> acc)
    answers 0

let () =
  let problem = Util.list_of_input () |> problem_of_input in
  let start =
    Util.index_of (fun v -> v.id = "AA") (Array.to_list problem.graph)
  in
  let closed_set = lnot (-1 lsl List.length problem.openables) in
  let answers = Hashtbl.create 20000 in

  solve problem
    { at = start; closed = closed_set; released = 0; minutes = 30 }
    answers
  |> Printf.printf "Part 1: %d\n";

  Hashtbl.clear answers;
  solve problem
    { at = start; closed = closed_set; released = 0; minutes = 26 }
    answers
  |> ignore;
  find_part2 answers closed_set |> Printf.printf "Part 2: %d\n"

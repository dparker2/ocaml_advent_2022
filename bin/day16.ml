type valve = { id : string; flow : int; tunnels : int list }
type state = { at : int; unopened : int; released : int; minutes : int }

type problem = {
  graph : valve array;
  openables : int list;
  dists : int array array;
}

let _extend_problem graph_l string =
  Scanf.sscanf string "Valve %s has flow rate=%d; %s %s to %s %s@!"
    (fun v f _ _ _ t -> (v, f, Util.list_of_str t) :: graph_l)

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

let rec solve problem state =
  let open_valve i v =
    let offset = 1 lsl i in
    if state.unopened land offset == 0 then 0
    else
      let d = problem.dists.(state.at).(v) in
      let rmin = state.minutes - d - 1 in
      if rmin <= 0 then 0
      else
        solve problem
          {
            at = v;
            unopened = state.unopened lxor offset;
            released = state.released + (rmin * problem.graph.(v).flow);
            minutes = rmin;
          }
  in
  match state with
  | { unopened = 0; _ } | { minutes = 0; _ } -> state.released
  | _ ->
      problem.openables |> List.mapi open_valve
      |> List.fold_left max state.released

let () =
  let problem = Util.list_of_input () |> problem_of_input in
  let initial_state =
    {
      at = Util.index_of (fun v -> v.id = "AA") (Array.to_list problem.graph);
      unopened = lnot (-1 lsl List.length problem.openables);
      released = 0;
      minutes = 30;
    }
  in
  solve problem initial_state |> Printf.printf "Part 1: %d\n"

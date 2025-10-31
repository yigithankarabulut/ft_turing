open Yojson.Basic.Util

type t_transition =
{
	name     : string;
	read     : string;
	to_state : string;
	write    : string;
	action   : string;
}

type t_machine =
{
	name        : string;
	alphabet    : string array;
	blank       : string;
	states      : string array;
	initial     : string;
	finals      : string array;
	transitions : t_transition array;
}

type t_machine_state =
{
  state      : string;
  tape       : char array;
  position   : int;
}

let error (msg: string) =
  Printf.eprintf "[ERROR]: %s\n" msg;
  exit (1)

let array_contains (arr: string array) (x: string) =
  Array.exists (fun y -> y = x) arr

let chtostr (c: char) = String.make 1 c

let read_file (path: string) : string =
	try
		let file = open_in (path) in
		let rec loop (content: string) : string =
			try loop (content ^ (input_line file) ^ "\n")
			with End_of_file -> close_in (file); content
		in loop ("")
	with Sys_error msg -> error (msg)

let strtoarr (str: string) : char array =
	Array.init (String.length str) (fun i -> str.[i])

let get_member (name: string) (json: Yojson.Basic.t) =
	match json |> Yojson.Basic.Util.member name with
	| `Null ->
			error ("Missing key '" ^ name ^ "' in JSON.")
	| v -> v

let init_transitions (json: Yojson.Basic.t) =
  json |> Yojson.Basic.Util.to_assoc |> List.map (fun (state, t_list) ->
    t_list |> Yojson.Basic.Util.to_list |> List.map (fun trans ->
      {
        name = state;
        read = trans |> get_member "read" |> Yojson.Basic.Util.to_string;
        to_state = trans |> get_member "to_state" |> Yojson.Basic.Util.to_string;
        write = trans |> get_member "write" |> Yojson.Basic.Util.to_string;
        action = trans |> get_member "action" |> Yojson.Basic.Util.to_string;
      }
    )
  ) |> List.flatten |> Array.of_list

let json_to_machine (json: Yojson.Basic.t) : t_machine =
{
  name = get_member "name" json |> Yojson.Basic.Util.to_string;
  alphabet = get_member "alphabet" (json)
		|> Yojson.Basic.Util.to_list
		|> List.map Yojson.Basic.Util.to_string
		|> Array.of_list;
  blank = get_member "blank" json |> Yojson.Basic.Util.to_string;
  states = get_member "states" json |> Yojson.Basic.Util.to_list
		|> List.map Yojson.Basic.Util.to_string
		|> Array.of_list;
  initial = get_member "initial" json |> Yojson.Basic.Util.to_string;
  finals = get_member "finals" json
		|> Yojson.Basic.Util.to_list
		|> List.map Yojson.Basic.Util.to_string
		|> Array.of_list;
  transitions = get_member "transitions" json |> init_transitions;
}

let find_transition (ts: t_transition array) (name: string) (read: char) =
	let rec loop (i: int) =
		if i >= Array.length ts then None
		else if ts.(i).name = name && ts.(i).read = chtostr read then
			Some ts.(i)
		else loop (i + 1) in loop 0

let print_tape (input: char array) (index: int) (t: t_transition) =
	let helper = ref 0 in
	Printf.printf "[";
	Array.iter (fun x ->
		if !helper = index then Printf.printf "<%c>" x
		else Printf.printf " %c " x;
		incr helper
	) input;
	Printf.printf "] [%s %c %s]\n"
		t.to_state
		t.write.[0]
		t.action

let is_final (finals: string array) (to_state: string) : bool =
	Array.exists (fun f -> f = to_state) finals

let check_bounds action index input_len =
	if action = "LEFT" && index - 1 < 0 then begin
		error ("Index moved LEFT out of bounds")
	end else if action = "RIGHT" && index + 1 >= input_len then begin
		error ("Index moved RIGHT out of bounds")
	end

let states_equal s1 s2 =
  s1.state = s2.state && 
  s1.position = s2.position &&
  Array.length s1.tape = Array.length s2.tape &&
  Array.for_all2 (=) s1.tape s2.tape

let state_history = ref []

let check_infinite_loop current_state =
  if List.exists (states_equal current_state) !state_history then begin
    error "Infinite loop detected: machine returned to a previous state"
  end else begin
    state_history := current_state :: !state_history;
    if List.length !state_history > 5000 then
      state_history := List.rev (List.tl (List.rev !state_history))
  end

let rec run (m: t_machine) (input: char array) (state: string) (index: int) =
  let current_state = { state = state; tape = Array.copy input; position = index } in
  check_infinite_loop current_state;
  
  match find_transition m.transitions state input.(index) with
  | None ->
    error ("Transition not found for (state: " ^ state ^ ", read: " ^ chtostr input.(index) ^ ")");
  | Some t ->
    print_tape input index t;
    let new_input = Array.mapi (fun i c ->
      if i = index then t.write.[0] else c
    ) input in
    
    if is_final m.finals t.to_state then new_input
    else begin
      check_bounds t.action index (Array.length input);
      let next_index = if t.action = "LEFT" then index - 1 else index + 1 in
      run m new_input t.to_state next_index
    end

let print_header (name: string) =
	let name_length: int = String.length name in
	let rec repeat (c: char) (n: int) = if n > 0 then (Printf.printf "%c" c; repeat c (n-1)) in
	repeat '*' (name_length + 40); print_newline ();
	repeat '*' 1; repeat ' ' (19);
	Printf.printf "%s" name;
	repeat ' ' (19); repeat '*' 1; print_newline ();
	repeat '*' (name_length + 40); print_newline ()

let start_machine (m: t_machine) (input: char array) =
	print_header m.name;
	state_history := [];
	let result = run m (Array.copy input) m.initial 0 in
	Array.iter (Printf.printf "%c") result;
	print_newline ()

let check_arg (argc: int) (argv: string array) : unit =
	let print_usage_and_exit () =
		Printf.printf "Try '%s -h' or '%s --help' for more information.\n" argv.(0) argv.(0);
		exit (1)
	in
	if argc != 2 && argc != 3 then print_usage_and_exit ();
	if (argc = 2) then
	(
		if (argv.(1) = "-h" || argv.(1) = "--help") then
		(
			print_endline ("usage: " ^ argv.(0) ^ " [-h] jsonfile input");
			print_endline ("");
			print_endline ("positional arguments:");
			print_endline ("  jsonfile   json description of the machine");
			print_endline ("");
			print_endline ("  input      input of the machine");
			print_endline ("");
			print_endline ("optional arguments:");
			print_endline ("  -h, --help show this help message and exit");
			exit (0)
		);
		print_usage_and_exit ();
	)

let check_file_ext (path: string) (ext: string) : unit =
	let lenp = String.length (path) in
	let lene = String.length (ext) in
	if not (lenp >= lene && String.sub (path) (lenp - lene) (lene) = ext) then
		error ("file must be '.json'")

let can_file_be_open (path: string) : unit =
	try
		let file = open_in (path) in
		close_in (file)
	with Sys_error _ ->
		error ("cannot open file: " ^ path)

let validate_blank_in_alphabet (m: t_machine) =
  if not (array_contains m.alphabet m.blank) then
    error ("Blank character '" ^ m.blank ^ "' not found in alphabet.")

let validate_initial (m: t_machine) =
  if not (array_contains m.states m.initial) then
    error ("Initial state '" ^ m.initial ^ "' not found in states.");

	let trans_states =
		let names = Array.map (fun (t: t_transition) -> t.name) m.transitions in
		Array.to_list names |> List.sort_uniq compare
  in
  if not (List.mem m.initial trans_states) then
    error ("Initial state '" ^ m.initial ^ "' not found in transitions.")

let validate_finals (m: t_machine) =
  Array.iter (fun final_state ->
    if not (array_contains m.states final_state) then
      error ("Final state '" ^ final_state ^ "' not found in states.");
		let in_transitions =
			Array.exists (fun (t: t_transition) -> t.name = final_state) m.transitions
    in
    if in_transitions then
      error ("Final state '" ^ final_state ^ "' should not appear in transitions.")
  ) m.finals

let validate_transitions (m: t_machine) =
  Array.iter (fun t ->
    if not (array_contains m.alphabet t.read) then
      error ("Transition '" ^ t.name ^ "' has invalid read symbol: " ^ t.read);
    if not (array_contains m.alphabet t.write) then
      error ("Transition '" ^ t.name ^ "' has invalid write symbol: " ^ t.write);

    if not (array_contains m.states t.to_state) then
      error ("Transition '" ^ t.name ^ "' refers to unknown state: " ^ t.to_state);

    if t.action <> "LEFT" && t.action <> "RIGHT" then
      error ("Transition '" ^ t.name ^ "' has invalid action: " ^ t.action)
  ) m.transitions

let validate_state_usage (m: t_machine) =
	let transition_states: string array =
		Array.map (fun (t: t_transition) -> t.name) m.transitions
  in
  Array.iter (fun st ->
    if not (Array.exists (fun tname -> tname = st) transition_states)
       && not (Array.exists (fun f -> f = st) m.finals)
    then
      error ("State '" ^ st ^ "' defined but never used in transitions.")
  ) m.states;

  Array.iter (fun tname ->
    if not (array_contains m.states tname)
    then error ("Transition defined for unknown state '" ^ tname ^ "'.")
  ) transition_states

let validate_machine (m: t_machine) =
	validate_blank_in_alphabet (m);
  validate_initial (m);
  validate_finals (m);
  validate_state_usage (m);
  validate_transitions (m)

let validate_input (m: t_machine) (input: char array) =
  let alphabet_chars = Array.map (fun s -> s.[0]) m.alphabet in
  let blank_char = m.blank.[0] in
  let lastch = alphabet_chars.(Array.length alphabet_chars - 1) in

  Array.iter (fun c ->
    if not (Array.exists (fun a -> a = c) alphabet_chars) then
      error ("Input contains character not in alphabet: " ^ chtostr c);

    if c = blank_char then
      error ("Input contains blank character: " ^ chtostr c)
  ) input;

  if input.(Array.length input - 1) <> lastch then
    error ("Input must end with the last alphabet character: " ^ chtostr lastch)

let is_empty (str: string) =
	if (String.length (str) = 0) then
		error ("file is empty")

let main (argc: int) (argv: string array) =
	check_arg (argc) (argv);
	let jsonfile = argv.(1) in
	check_file_ext (jsonfile) (".json");
	can_file_be_open (jsonfile);
	let content = read_file (jsonfile) in
	is_empty (content);
	let json = Yojson.Basic.from_string (content) in
	let m: t_machine = json_to_machine (json) in
	validate_machine (m);
	let input = strtoarr (argv.(2)) in
	validate_input (m) (input);
	start_machine (m) (input); ()

let (): unit = main (Array.length Sys.argv) (Sys.argv)

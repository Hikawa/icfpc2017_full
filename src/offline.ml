open Core
module J = Yojson.Basic

    (*
let () = Random.self_init()
let () = Random.init 0
*)

let send j =
  let s = J.to_string j in
  let buf = (Int.to_string @@ String.length s) ^ ":" ^ s in
  let p = ref 0 in
  while !p < String.length buf do
    p := !p + (Unix.write Unix.stdout ~pos:!p ~len:((String.length buf) - !p) ~buf)
  done


let _ = Unix.fcntl_setfl Unix.stdin Unix.Open_flags.rdonly

exception Channel_error

let receive() =
  let rec read_count n =
    let Some c = In_channel.(input_char stdin) in
    if c = ':' then n
    else read_count (n*10 + ((Char.to_int c) - (Char.to_int '0'))) in
  let count = read_count 0 in
  let buf = String.make (count+1) ' ' in
  match In_channel.(really_input stdin) ~buf ~pos:0 ~len:count with
  | None -> raise Channel_error
  | Some () ->
    J.from_string buf

let apply_move game r =
  let moves = Game.Move.ts_of_j (Game.map game) r  in
  List.iter moves ~f:(Game.apply_move game)

let () = send @@ `Assoc [("me", `String "sol")]
let _ = receive()
let r = receive()

type command =
  | Stop of J.json
  | Move of J.json

let () = try
    let game = Game.t_of_j r in
    send @@ `Assoc [
      ("ready", `Int (Game.me game));
      ("state", Game.state_of_t game)]
  with
  | Game.Parse_error e ->
    match r with
    | `Assoc l -> begin
        let command = ref None and
        state = ref None in
        List.iter l ~f:(fun f ->
            match f with
            | ("stop", r) -> command := Some (Stop r)
            | ("move", r) -> command := Some (Move r)
            | ("state", st) -> state := Some st
            | _ -> ());
        match !command with
        | None -> ()
        | Some (Stop r) -> ()
        | Some (Move r) ->
          begin
            match !state with
            | Some state -> begin
                let game = Game.t_of_state state in
                apply_move game r;
                let my_move = Game.next_move game in
                Game.apply_move game my_move;
                match Game.Move.to_j (Game.map game) my_move with
                | `Assoc fields -> send @@ `Assoc (List.rev @@ ("state", Game.state_of_t game) :: fields)
                | _ -> ()
              end
            | None -> raise (Game.Parse_error "No state")
          end
      end
    | _ -> raise (Game.Parse_error (J.to_string r))

open Core
module J = Yojson.Basic

let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let () = Unix.connect socket (Unix.ADDR_INET ((Unix.Inet_addr.of_string "129.215.197.1"), (Int.of_string Sys.argv.(1))))

let send j =
  let s = J.to_string j in
  print_endline @@ "send: " ^ s;
  let _ = Unix.write socket ((Int.to_string @@ String.length s) ^ ":" ^ s) in
  ()

let buf_size = 1024 * 64
let buf = String.make buf_size ' '
let pos = ref 0

let receive() =
  let p = ref 0 in
  let rec read_count n =
    while !p >= !pos do
      pos := !pos + (Unix.recv socket ~buf ~pos:!pos ~len:(buf_size - !pos) ~mode:[])
    done;
    let c = String.nget buf !p in
    p := !p + 1;
    if c = ':' then n
    else read_count (n*10 + ((Char.to_int c) - (Char.to_int '0'))) in
  let count = read_count 0 in
  while !p + count > !pos do
    pos := !pos + (Unix.recv socket ~buf ~pos:!pos ~len:(buf_size - !pos) ~mode:[])
  done;
  let result = J.from_string @@ String.sub buf ~pos:!p ~len:count in
  String.blit ~src:buf ~src_pos:(!p + count) ~dst:buf ~dst_pos:0 ~len:(buf_size - count - !p);
  pos := !pos - !p - count;
  print_endline @@ "receive: " ^ (J.to_string result);
  result

let () = send @@ `Assoc [("me", `String "sol")]
let r = receive()
let game = Game.t_of_j @@ receive()
let () = send @@ `Assoc [("ready", `Int (Game.me game))]
let step() = begin
  let r = receive() in
  let process r =
    let moves = Game.Move.ts_of_j (Game.map game) r  in
    List.iter moves ~f:(Game.apply_move game);
    let my_move = Game.next_move game (Game.me game) in
    Game.apply_move game my_move;
    send @@ Game.Move.to_j (Game.map game) my_move in
  match r with
  | `Assoc [("stop", r)] -> process r; false
  | `Assoc [("move", r)] -> process r; true
  | _ -> raise (Game.Parse_error "format")
end
let () = while step() do ()
done
let () = for i = 0 to (Game.players game) - 1 do
    print_endline @@ (Int.to_string i) ^ " - " ^ (Int.to_string @@ Game.score game i)
  done

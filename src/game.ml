type world = World.t

open Core

module J = Yojson.Basic

exception Parse_error of string

module Mine = struct
  module VH = Hashtbl.Make(World.Site)

  type t = {
    id: int;
    distances: int VH.t;
  }

  let id m = m.id
  let distances m = m.distances
  let distance m v = VH.find m.distances v

  let create id = {
    id;
    distances = VH.create()
  }

  let j_of_t m =
    `List (List.map (VH.to_alist m.distances)
             ~f:(fun (k, v) -> `List [(`Int (World.Site.id k)); (`Int v)]))

  let t_of_j w i j =
    match j with
    | `List ds ->
      {
        id = i;
        distances = match
            VH.of_alist @@ List.map ds ~f:(fun e ->
                match e with
                | `List [(`Int k); (`Int v)] -> (World.site w k, v)
                | _ -> raise (Parse_error "state")) with
        | `Ok v -> v
        | _ -> raise (Parse_error "duplicate")
      }
    | _ -> raise (Parse_error "state")

end
type mine = Mine.t

module Move = struct
  open Core
  module J = Yojson.Basic

  type action =
    | Claim of int
    | Pass
    | Splurge of int list
    | Opt of int

  type t = {
    punter: int;
    action: action;
  }

  let claim punter r = {
    punter;
    action = Claim r
  }

  let pass punter = {
    punter;
    action = Pass
  }

  let splurge punter rvs = {
    punter;
    action = Splurge rvs
  }

  let action m = m.action

  let river m =
    match m.action with
    | Claim r -> Some r
    | Pass -> None
    | _ -> None

  let punter m = m.punter

  let t_of_j w = function
    | `Assoc [field] ->
      begin
        match field with
        | ("claim", `Assoc fields) -> begin
            let punter = ref None
            and source = ref None
            and target = ref None in
            List.iter fields ~f:(fun f ->
                match f with
                | ("punter", `Int id) -> punter := Some id
                | ("source", `Int s) -> source := Some s
                | ("target", `Int t) -> target := Some t
                | _ -> ());
            match !punter with
            | None -> raise (Parse_error "No punter")
            | Some punter ->
              match !source with
              | None -> raise (Parse_error "No source")
              | Some source ->
                match !target with
                | None -> raise (Parse_error "No target")
                | Some target ->
                  claim punter @@ World.river_index w (source, target)
          end
        | ("pass", `Assoc fields) -> begin
            let punter = ref None in
            List.iter fields ~f:(fun f ->
                match f with
                | ("punter", `Int id) -> punter := Some id
                | _ -> ());
            match !punter with
            | None -> raise (Parse_error "No punter")
            | Some punter ->
              pass punter
          end
        | ("splurge", `Assoc fields) -> begin
            let punter = ref None in
            let route = ref [] in
            List.iter fields ~f:(fun f ->
            match f with
            | ("punter", `Int id) -> punter := Some id
            | ("route", `List l) -> route := List.map l ~f:(fun e -> match e with
                | `Int p -> p
                |  _ -> raise (Parse_error ";;;"))
            | _ -> ());
            match !punter with
            | None -> raise (Parse_error "No punter")
            | Some punter ->
              let rvs = ref [] in
              let rec loop = function
                | [] -> ()
                | [hd] -> ()
                | x :: ((y :: tl) as r) ->
                  rvs := (World.river_index w (x, y)) :: !rvs;
                  loop r
              in
              loop !route;
              splurge punter !rvs
          end
        | ("option", `Assoc fields) -> begin
            let punter = ref None in
            List.iter fields ~f:(fun f ->
                match f with
                | ("punter", `Int id) -> punter := Some id
                | _ -> ());
            match !punter with
            | None -> raise (Parse_error "No punter")
            | Some punter ->
              pass punter
          end
        | _ -> raise (Parse_error "format")
      end
    | _ -> raise (Parse_error "move format")

  let ts_of_j w = function
    | `Assoc fields -> begin
        let result = ref None in
        List.iter fields ~f:(fun f ->
            match f with
            | ("moves", `List ms) ->
              result := Some (List.map ms ~f:(t_of_j w))
            | ("scores", _) -> ()
            | _ -> raise (Parse_error "need moves"));
        match !result with
        | None -> raise (Parse_error "no moves")
        | Some result -> result
      end
    | _ -> raise (Parse_error "moves format")

  let to_j w m: J.json =
    match m.action with
    | Claim r -> `Assoc [("claim",
                          let r = World.river w r in
                          `Assoc
                            [
                              ("punter", `Int m.punter);
                              ("source", `Int (World.River.source r));
                              ("target", `Int (World.River.target r));
                            ])]
    | Pass -> `Assoc [("pass", `Assoc [("punter", `Int m.punter)])]
    | _ -> `Assoc [("pass", `Assoc [("punter", `Int m.punter)])]
end

module D = Dijkstra.Simple(World)

module IM = Map.Make(Int)
module IS = Set.Make(Int)

let start_time = ref None

type pl = {
  islands: IS.t IM.t;
  links: int IM.t;
  score: int;
}

type t = {
  world: world;
  me: int;
  mines: mine array;
  pls: pl array;
  mutable hint: int option
}

let map g = g.world
let me g = g.me
let players g = Array.length g.pls
let pl g p = g.pls.(p)
let set_pl g p v = g.pls.(p) <- v
let links g p = g.pls.(p).links
let islands g p = g.pls.(p).islands
let score g p = g.pls.(p).score
let site_island g p s = match IM.find (links g p) s with
  | None -> s
  | Some v -> v
let site_island' pl s = match IM.find pl.links s with
  | None -> s
  | Some v -> v
let island g p i = match IM.find (islands g p) i with
  | None -> IS.singleton i
  | Some v -> v
let island' pl i = match IM.find pl.islands i with
  | None -> IS.singleton i
  | Some v -> v
let iter_island g p i f = match IM.find (islands g p) i with
  | None -> f i
  | Some sites -> IS.iter sites ~f
let iter_island' pl i f = match IM.find pl.islands i with
  | None -> f i
  | Some sites -> IS.iter sites ~f

let unite_islands i1 is1 i2 is2 =
  let r = IS.union is1 is2 in
  if i1 < i2 then (i1, r) else (i2, r)

let island_mines g is =
  let r = ref [] in
  Array.iteri (World.mines g.world) ~f:(fun i m ->
      if IS.mem is m then r := i :: !r);
  !r

let mines g = g.mines
let mine_dist g m v =
  match Mine.distance (mines g).(m) v with
  | Some d -> d
  | None -> 0


let sqr x = x * x

let rivers g = Array.length @@ World.all_rivers g.world

let pl_unite_islands' g pl xi yi =
  if xi <> yi then begin
    let xis = island' pl xi and
    yis = island' pl yi in
    let xms = island_mines g xis and
      yms = island_mines g yis in
    let (k, xy) = unite_islands xi xis yi yis in
    let new_links = ref pl.links in
    let new_score = ref pl.score in
    IS.iter xis ~f:(fun s ->
        if k <> xi then
          new_links := IM.add !new_links ~key:s ~data:k;
        List.iter yms ~f:(fun im ->
            new_score := !new_score + (sqr @@ mine_dist g im (World.site g.world s))));
    IS.iter yis ~f:(fun s ->
        if k <> yi then
          new_links := IM.add !new_links ~key:s ~data:k;
        List.iter xms ~f:(fun im ->
            new_score := !new_score + (sqr @@ mine_dist g im (World.site g.world s))));
    {
      islands = IM.add (IM.remove (IM.remove pl.islands xi) yi) ~key:k ~data:xy;
      links = !new_links;
      score = !new_score
    }
  end else pl

let pl_unite_by_river' g pl rv =
  pl_unite_islands' g pl (site_island' pl (World.River.source rv)) (site_island' pl (World.River.target rv))

module IntTuple = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1
    | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let hash (x, y) = let s = x + y in y + s*(s + 1)/2
end
module ITH = Hashtbl.Make(IntTuple)

let pl_unite_islands g pl xi yi memo =
  match ITH.find memo (xi, yi) with
  | Some v -> v
  | None ->
    let r = pl_unite_islands' g pl xi yi in
    ITH.set memo ~key:(xi, yi) ~data:r;
    r

let pl_unite_by_river g pl rv memo =
  pl_unite_islands g pl (site_island' pl (World.River.source rv)) (site_island' pl (World.River.target rv)) memo

let g_best_pi = ref (-1)
exception Fast

let check_time() =
  let tm = Time.Span.to_sec (Time.diff (Time.now()) (Option.value_exn !start_time)) in
  if tm > 0.9 then raise Fast


let apply_move g m =
  match Move.action m with
  | Move.Claim r ->
    let rv = World.river (map g) r in
    World.River.claim rv @@ Move.punter m;
    let p = Move.punter m in
    let pl = pl g p in
    set_pl g p @@ pl_unite_by_river' g pl rv
  | Move.Splurge rvs ->
    List.iter rvs ~f:(fun r ->
      let rv = World.river (map g) r in
      World.River.claim rv @@ Move.punter m;
      let p = Move.punter m in
      let pl = pl g p in
      set_pl g p @@ pl_unite_by_river' g pl rv)
  | _ -> ()


module IH = Hashtbl.Make(Int)

exception Impossible

let river_race g p max_steps sources =
  let min_score = ref 0 in
  let opened = IH.create() in
  let last_winner = ref None in
  IH.iteri sources ~f:(fun ~key:si ~data:pl ->
      IH.set opened ~key:si ~data:(si, pl);
      last_winner := Some si
    );
  let rec loop opened t =
    check_time();
    if t >= max_steps then Option.value_exn !last_winner
    else begin
      min_score := !min_score + 1;
      let new_opened = IH.create() in
      IH.iteri opened ~f:(fun ~key:w ~data:(si, pl) ->
          let s = World.site g.world si in
          let memo = ITH.create() in
          last_winner := Some si;
          Array.iteri (World.Site.river_ids s) ~f:(fun i ri ->
              let rv = World.river g.world ri in
              if World.River.owner rv = None then begin
                let n = World.site_succ g.world s i in
                let npl = pl_unite_by_river g pl rv memo in
                if npl.score > !min_score then begin
                  min_score := npl.score;
                  IH.clear new_opened;
                  IH.set new_opened ~key:n ~data:(si, npl);
                end else if npl.score = !min_score then
                  IH.set new_opened ~key:n ~data:(si, npl)
              end));
      match IH.length new_opened with
      | 0 -> Option.value_exn !last_winner
      | 1 ->
        IH.iteri new_opened ~f:(fun ~key:w ~data:(si, pl) -> last_winner := Some si);
        Option.value_exn !last_winner
      | _ ->
        loop new_opened (t + 1)
    end
  in loop opened 0

    (*
let randomelement = function
  | [] -> None
  | l ->
    let n = Random.int (List.length l) in
    List.nth l n
*)

let next_move' g p =
  let base_pl = pl g p in
  let next_pls = Array.create ~len:(Array.length @@ World.all_rivers g.world) base_pl in
  let memo = ITH.create() in
  g_best_pi := -1;
  Array.iteri (World.all_rivers (map g)) ~f:(fun i rv ->
      if i mod 1000 = 999 then check_time();
      match World.River.owner rv with
      | None -> begin
          next_pls.(i) <- pl_unite_by_river g base_pl rv memo;
          if !g_best_pi < 0 || next_pls.(i).score > next_pls.(!g_best_pi).score then
            g_best_pi := i
        end
      | _ -> ());
  check_time();
  let mine_islands = ref IM.empty in
  Array.iteri (World.mines (map g)) ~f:(fun im m ->
      let is = site_island' base_pl m in
      mine_islands := IM.add !mine_islands ~key:is ~data:(IH.create()));
  let colors = Array.create ~len:(Array.length (World.sites (map g))) ((-1), (-1)) in
  IM.iteri !mine_islands ~f:(fun ~key ~data:opened ->
      IS.iter (island' base_pl key) ~f:(fun e ->
          colors.(e) <- (key, (-1));
          IH.set opened ~key:e ~data:(-1)));
  let connects = (IH.create(), IH.create()) in

  let racing rvs =
    let connects = ref [] and
    expand_points = IH.create() and
    expand_rivers = IH.create() and
    dummies = ref [] in
    List.iter rvs ~f:(fun ri ->
        let rv = World.river g.world ri in
        let xi = site_island' base_pl (World.River.source rv) and
          yi = site_island' base_pl (World.River.target rv) in
        if xi = yi then
          dummies := ri :: !dummies
        else begin
          let xis = island' base_pl xi and
          yis = island' base_pl yi in
          let xms = island_mines g xis and
          yms = island_mines g yis in
          if List.is_empty xms && List.is_empty yms then
            dummies := ri :: !dummies
          else if (not @@ List.is_empty xms) && (not @@ List.is_empty yms) then
            connects := ri :: !connects
          else let point = if List.is_empty xms then World.River.source rv else World.River.target rv in
            begin
              IH.set expand_points ~key:point ~data:next_pls.(ri);
              IH.set expand_rivers ~key:point ~data:ri
            end
        end);
    match !connects with
    | hd :: _ -> Move.claim p hd
    | [] -> begin
        match IH.length expand_points with
        | 0 ->
          begin
            match !dummies with
            | [] -> raise Impossible
            | (hd :: _) -> Move.claim p hd
          end
        | 1 ->
          let r = ref None in
          IH.iter expand_rivers ~f:(fun v ->
              r := Some v);
          Move.claim p @@ Option.value_exn !r
        | _ ->
          match IH.find expand_rivers @@ river_race g p 40 expand_points with
          | None -> raise Impossible
          | Some v -> Move.claim p v
      end in

  let simple() =
    let best_ps = ref [] in
    Array.iteri (World.all_rivers (map g)) ~f:(fun i rv ->
        if i mod 1000 = 999 then check_time();
        match World.River.owner rv with
        | None -> begin
            match !best_ps with
            |  [] -> best_ps := [i]
            | ((hd :: _) as l) ->
              if next_pls.(i).score > next_pls.(hd).score then
                best_ps := [i]
              else if next_pls.(i).score = next_pls.(hd).score then
                best_ps := i :: l
          end
        | _ -> ());
    racing !best_ps
  in

  let rec loop() =
    check_time();
    let something = ref false in
    IM.iteri !mine_islands ~f:(fun ~key:is ~data:opened ->
        let new_opened = IH.create() in
        IH.iteri opened ~f:(fun ~key:s ~data:path ->
            let ss = World.site g.world s in
            Array.iteri (World.Site.river_ids ss) ~f:(fun i ri ->
                let rv = World.river g.world ri in
                let path = if path < 0 then ri else path in
                if World.River.owner rv = None then begin
                  let n = World.site_succ g.world ss i in
                  let (oc, opath) = colors.(n) in
                  if oc < 0 then begin
                    colors.(n) <- (is, path);
                    IH.set new_opened n path;
                    something := true
                  end else if oc > is then begin
                    IH.set (fst connects) ~key:path ~data:();
                    if opath >= 0 then
                      IH.set (fst connects) ~key:opath ~data:()
                  end else if oc < is then begin
                    IH.set (snd connects) ~key:path ~data:();
                    if opath >= 0 then
                      IH.set (snd connects) ~key:opath ~data:();
                  end else if IH.mem new_opened n && (opath < 0 || next_pls.(path).score > next_pls.(opath).score) then begin (*update with better path*)
                    colors.(n) <- (is, path)
                  end
                end));
        mine_islands := IM.add !mine_islands ~key:is ~data:new_opened);
    if not @@ IH.is_empty @@ fst connects then
      fst connects
    else if not @@ IH.is_empty @@ snd connects then
      snd connects
    else if !something then loop()
    else IH.create()
  in
  let paths = loop() in
  if IH.is_empty paths then
    simple()
  else begin
    let best_ps = ref [] in
    IH.iteri paths ~f:(fun ~key:i ~data:() ->
        let rv = World.river g.world i in
        match World.River.owner rv with
        | None -> begin
            match !best_ps with
            |  [] -> best_ps := [i]
            | ((hd :: _) as l) ->
              if next_pls.(i).score > next_pls.(hd).score then
                best_ps := [i]
              else if next_pls.(i).score = next_pls.(hd).score then
                best_ps := i :: l
          end
        | _ -> ());
    check_time();
    racing !best_ps
  end



let next_move g =
  start_time := Some (Time.now());
  try
    next_move' g g.me
  with
  | _  -> begin
      match !g_best_pi with
      | (-1) -> Move.pass g.me
      | v -> Move.claim g.me v
    end

let state_of_t g =
  let state_of_island v =
    `List (List.map (IS.to_list v) ~f:(fun i -> `Int i)) in
  let r = [
    ("world", World.j_of_t g.world);
    ("me", `Int g.me);
    ("mines", `List (List.map (Array.to_list g.mines) ~f:Mine.j_of_t));
    ("pls", `List (List.map (Array.to_list g.pls) ~f:(fun p ->
         `Assoc [("score", `Int p.score);
                 ("islands", `List (List.map (IM.to_alist p.islands) ~f:(fun (k, v) ->
                      `List [(`Int k); (state_of_island v)])));
                ])));
  ] in
  match g.hint with
  | None -> `Assoc r
  | Some h -> `Assoc (("hint", `Int h) :: r)


let t_of_state : J.json -> t = function
  | `Assoc fields ->
    let pls_of_state : J.json -> pl array = function
      | `List els ->
        Array.of_list (List.map els ~f:(fun p ->
            match p with
            | `Assoc fields -> begin
                let islands_of_state : J.json -> IS.t IM.t = function
                  | `List l -> begin
                      match IM.of_alist (List.map l ~f:(fun p ->
                          match p with
                          | `List [(`Int k); (`List v)] ->
                            (k, IS.of_list (List.map v ~f:(fun p -> match p with
                                 | `Int v -> v
                                 | _ -> raise (Parse_error "plsstate"))))
                          | _ -> raise (Parse_error "plsstate"))) with
                      | `Ok v -> v
                      | _ -> raise (Parse_error "Dupl")
                    end
                  | _ -> raise (Parse_error "plsstate")
                in
                let links_of_islands islands =
                  let r = ref IM.empty in
                  IM.iteri islands ~f:(fun ~key ~data ->
                      IS.iter data ~f:(fun s ->
                          r := IM.add !r ~key:s ~data:key));
                  !r
                in

                let score = ref None and
                  islands = ref None in
                List.iter fields ~f:(fun f ->
                    match f with
                    | ("score", `Int v) -> score := Some v
                    | ("islands", v) -> islands := Some (islands_of_state v)
                    | _ -> ());
                match !score with
                | None -> raise (Parse_error "no score")
                | Some score ->
                  match !islands with
                  | None -> raise (Parse_error "no links")
                  | Some islands -> {
                      score;
                      islands;
                      links = links_of_islands islands
                    }
              end
            | _ -> raise (Parse_error "plsstate")))
      | _ -> raise (Parse_error "plsstate") in

    let
      world = ref None and
      me = ref None and
      pls = ref None and
      mines = ref None and
      hint = ref None in
    List.iter fields ~f:(fun f -> match f with
        | ("world", v) -> world := Some (World.t_of_j v)
        | ("me", `Int v) -> me := Some v
        | ("pls", v) -> pls := Some (pls_of_state v)
        | ("mines", `List v) -> mines := Some v
        | ("hint", `Int v) -> hint := Some v
        | _ -> ());
    begin
      match !world with
      | None -> raise (Parse_error "no wrold")
      | Some world ->
        match !me with
        | None -> raise (Parse_error "no me")
        | Some me ->
          match !pls with
          | None -> raise (Parse_error "no pls")
          | Some pls ->
            match !mines with
            | None -> raise (Parse_error "no mines")
            | Some mines -> {
                world;
                me;
                pls;
                mines = Array.of_list (List.mapi mines ~f:(Mine.t_of_j world));
                hint = !hint
              }
    end
  | _ -> raise (Parse_error "state")


let t_of_j: J.json -> t = function
  | `Assoc fields -> begin
      let me = ref None and
      count = ref None and
      world = ref None in
      let app: (string * J.json) -> unit = function
        | ("punter", `Int id) -> me := Some id
        | ("punters", `Int c) -> count := Some c
        | ("map", v) -> world := Some v
        | ("state", _) -> ()
        | _ -> () in
      List.iter fields ~f:app;
      match !me with
      | None -> raise (Parse_error "no me")
      | Some me ->
        match !count with
        | None -> raise (Parse_error "no count")
        | Some count ->
          match !world with
          | None -> raise (Parse_error "no world")
          | Some world ->
            let world = World.t_of_j world in
            let result = {
              world;
              me;
              mines = Array.map (World.mines world) ~f:(fun m ->
                  let r = Mine.create m in
                  D.shortest_paths world (World.site world m) (Mine.distances r);
                  r);
              pls = Array.create ~len:count {
                  islands = IM.empty;
                  links = IM.empty;
                  score = 0;
                };
              hint = None
            } in (*
            begin
              match Move.river (next_move' result me) with
              | Some ri ->
                result.hint <- Some ri
              | _ -> ()
            end;*)
            result
    end
  | _ -> raise (Parse_error "format")

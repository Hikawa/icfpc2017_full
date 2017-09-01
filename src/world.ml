open Core

module J = Yojson.Basic

module Site = struct
  type t = {
    id: int;
    rivers: int array;
    is_mine: bool;
  }

  let id s = s.id
  let river_ids s = s.rivers
  let river_id s i = s.rivers.(i)

  let create id rivers is_mine = {
    id;
    rivers;
    is_mine;
  }

  let degree s = Array.length s.rivers

  let compare x y = Int.compare (id x) (id y)
  let hash s = Int.hash @@ id s
  let equal x y = Int.equal (id x) (id y)
  let t_of_sexp s = create (Int.t_of_sexp s) [||] false
  let sexp_of_t s = Int.sexp_of_t @@ id s

end
module V = Site
type site = Site.t

module River = struct
  type t = {
    id: int;
    mutable owner: int option;
    source: int;
    target: int;
  }

  let id r = r.id
  let owner r = r.owner
  let source r = r.source
  let target r = r.target

  let create id ?(owner=None) source target = {
    id;
    owner;
    source;
    target;
  }

  type label = int option
  let label = owner
  let src = source
  let dst = target

  let claim r p = r.owner <- Some p
  let to_string r = "[" ^ (Int.to_string r.source) ^ " - " ^ (Int.to_string r.target) ^ "]"
end
module E = River
type river = River.t

type t = {
  sites: site array;
  all_rivers: river array;
  mines: int array
}

let sites w = w.sites
let site_count w = Array.length @@ sites w
let site w i = w.sites.(i)
let all_rivers w = w.all_rivers
let mines w = w.mines

let river w i = w.all_rivers.(i)

let river_index w (s, t) =
  match Array.find (all_rivers w) ~f:(fun r ->
      ((River.source r = s) && (River.target r = t)) || ((River.source r = t) && (River.target r = s))) with
  | Some r -> River.id r
  | None -> raise Not_found

let site_succ w s i =
  let r = river w (Site.river_id s i) in
  if Site.id s = River.source r then River.target r else River.source r

let iter_vertex f w =
  for i = 0 to (site_count w) - 1 do
    f @@ site w i
  done

let iter_succ f w s =
  for i = 0 to (Site.degree s) - 1 do
    f @@ site w @@ site_succ w s i
  done

exception Parse_error of string

let t_of_j = function
  | `Assoc fields ->
    let sites = ref None and
    rivers = ref None and
    mines = ref None in
    List.iter fields ~f:(fun f ->
        match f with
        | ("sites", `List s) -> sites := Some s
        | ("rivers", `List r) -> rivers := Some r
        | ("mines", `List m) -> mines := Some m
        | _ -> ());
    begin match !sites with
      | None -> raise (Parse_error "no sites")
      | Some sites ->
        match !rivers with
        | None -> raise (Parse_error "no rivers")
        | Some rivers ->
          match !mines with
          | None -> raise (Parse_error "no mines")
          | Some mines ->
            let site_count = ref 0 in
            List.iter sites ~f:(fun s ->
                match s with
                | `Assoc fields -> begin
                    List.iter fields ~f:(fun f ->
                        match f with
                        | ("id", `Int id) -> if !site_count < id + 1 then site_count := id + 1
                        | _ -> ())
                  end
                | _ -> raise (Parse_error "sites format"));
            let result = {
              sites = Array.create ~len:!site_count @@ Site.create 0 [||] false;
              all_rivers = Array.create ~len:(List.length rivers) @@ River.create 0 0 0;
              mines = Array.create ~len:(List.length mines) 0;
            } in
            List.iteri mines ~f:(fun i m ->
                match m with
                | `Int v -> result.mines.(i) <- v
                | _ -> raise (Parse_error "mines foramt"));
            List.iteri rivers ~f:(fun i r ->
                match r with
                | `Assoc fields -> begin
                    let source = ref None and
                    target = ref None and
                    owner = ref None in
                    List.iter fields ~f:(fun f ->
                        match f with
                        | ("source", `Int v) -> source := Some v
                        | ("target", `Int v) -> target := Some v
                        | ("owner", `Int v) -> owner := Some v
                        | _ -> ());
                    match !source with
                    | None -> raise (Parse_error "no source")
                    | Some source ->
                      match !target with
                      | None -> raise (Parse_error "no target")
                      | Some target ->
                        result.all_rivers.(i) <- River.create i ~owner:(!owner) source target
                  end
                | _ -> raise (Parse_error "reiver format"));
            List.iter sites ~f:(fun s ->
                match s with
                | `Assoc fields -> begin
                    let id = ref None in
                    List.iter fields ~f:(fun f ->
                        match f with
                        | ("id", `Int v) -> id := Some v
                        | _ -> ());
                    match !id with
                    | None -> raise (Parse_error "no id in site")
                    | Some id ->
                      result.sites.(id) <- Site.create
                          id
                          (Array.filter_map result.all_rivers ~f:(fun r ->
                               if River.source r = id || River.target r = id then
                                 Some (River.id r)
                               else None))
                        @@ Option.is_some @@ Array.find result.mines ~f:(fun m -> m = id)
                  end
                | _ -> raise (Parse_error "sites format"));
            result
    end;
  | _ -> raise (Parse_error "Map is not object")

let j_of_t w =
  `Assoc [
    ("sites", `List (List.map (Array.to_list w.sites) ~f:(fun s ->
         `Assoc [("id", `Int (Site.id s))])));
    ("rivers", `List (List.map (Array.to_list @@ all_rivers w) ~f:(fun r ->
         let v = [("source", `Int (River.source r));
                  ("target", `Int (River.target r))] in
         `Assoc (match River.owner r with
             | None -> v
             | Some owner -> ("owner", `Int owner) :: v))));
    ("mines", `List (List.map (Array.to_list @@ mines w) ~f:(fun m ->
         `Int m)))
  ]
(*let make_handshake w = J.to_string @@ `Assoc [("me", `Int (me w))]*)

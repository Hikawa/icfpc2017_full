open Core

module Simple(G: Sgraph.S) = struct
  module VH = Hashtbl.Make(G.V)

  let shortest_paths graph ?(iter_succ=G.iter_succ) source distances =
    VH.set distances source 0;
    let opened = VH.create() in
    VH.set opened source ();
    let rec loop opened dist = if not @@ VH.is_empty opened then begin
        let new_opened = VH.create() in
        VH.iter_keys opened ~f:(fun v ->
            iter_succ (fun x ->
                match VH.find distances x with
                | Some _ -> ()
                | None ->
                  VH.set new_opened x ();
                  VH.set distances x dist)
              graph v);
        loop new_opened (dist + 1)
      end in
    loop opened 1

  let wave graph ?(iter_succ=G.iter_succ) ~f source =
    let visited = VH.create() in
    VH.set visited source ();
    let opened = VH.create() in
    VH.set opened source ();
    let rec loop opened = if not @@ VH.is_empty opened then begin
        let new_opened = VH.create() in
        VH.iter_keys opened ~f:(fun v ->
            f v;
            iter_succ (fun x ->
                match VH.find visited x with
                | Some _ -> ()
                | None ->
                  VH.set new_opened x ();
                  VH.set visited x ())
              graph v);
        loop new_opened
        end in
      loop opened
end

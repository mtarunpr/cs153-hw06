(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Stdlib.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"

  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* TASK: complete the flow function for alias analysis. 

   - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     (as the value being stored) may be aliased
   - Other instructions do not define pointers

 *)
let insn_flow ((u, i) : uid * insn) (d : fact) : fact =
  match i with
  | Alloca _ -> UidM.add u SymPtr.Unique d
  | Load (Ptr (Ptr _), _) -> UidM.add u SymPtr.MayAlias d
  | Call (ret_ty, _, args) ->
    let f d (ty, arg) =
      match ty, arg with
      | Ptr _, Id id -> UidM.add id SymPtr.MayAlias d
      | _ -> d
    in
    let d =
      match ret_ty with
      | Ptr _ -> UidM.add u SymPtr.MayAlias d
      | _ -> d
    in
    List.fold_left f d args
  | Bitcast (ty1, op, ty2) ->
    let d =
      match ty1, op with
      | Ptr _, Id id -> UidM.add id SymPtr.MayAlias d
      | _ -> d
    in
    let d =
      match ty2 with
      | Ptr _ -> UidM.add u SymPtr.MayAlias d
      | _ -> d
    in d
  | Gep (Ptr _, op, idxs) -> 
    let d =
      match op with
      | Id id -> UidM.add id SymPtr.MayAlias d
      | _ -> d
    in UidM.add u SymPtr.MayAlias d
  | Store (Ptr _, Id id, _) ->
      UidM.add id SymPtr.MayAlias d
  | _ -> d


(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int = 
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* TASK: complete the "combine" operation for alias analysis.

       The alias analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful.

       It may be useful to define a helper function that knows how to take the
       meet of two SymPtr.t facts.
    *)
    let combine (ds : fact list) : fact =
      let combine' (d1 : fact) (d2 : fact) : fact =
        let f _ sp_opt1 sp_opt2 =
          match sp_opt1, sp_opt2 with
          | Some sp1, Some sp2 -> if SymPtr.compare sp1 sp2 < 0 then sp_opt2 else sp_opt1
          | Some sp, None | None, Some sp -> Some sp
          | None, None -> None
        in
        UidM.merge f d1 d2
      in
      if ds = [] then UidM.empty else
      List.fold_left combine' (List.hd ds) (List.tl ds)
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg


open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare (a:t) (b:t) =
      match a, b with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"

    
  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate 
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)
let bop_to_fn (bop : Ll.bop) =
  let int_to_int64_arg f =
    fun x y -> f x (Int64.to_int y)
  in
  match bop with
  | Add -> Int64.add
  | Sub -> Int64.sub
  | Mul -> Int64.mul
  | Shl -> int_to_int64_arg Int64.shift_left
  | Lshr -> int_to_int64_arg Int64.shift_right_logical
  | Ashr -> int_to_int64_arg Int64.shift_right
  | And -> Int64.logand
  | Or -> Int64.logor
  | Xor -> Int64.logxor

let cnd_to_fn (cnd : Ll.cnd) =
  let fn =
    match cnd with
    | Eq -> (=)
    | Ne -> (<>)
    | Slt -> (<)
    | Sle ->  (<=)
    | Sgt -> (>)
    | Sge -> (>=)
  in
  fun x y -> if fn (Int64.compare x y) 0 then 1L else 0L

let insn_flow (u, i : uid * insn) (d : fact) : fact =
  let add_mapping ll_to_fn ll (sc1 : SymConst.t) (sc2 : SymConst.t) = 
    match sc1, sc2 with
    | Const c1, Const c2 -> UidM.add u (SymConst.Const ((ll_to_fn ll) c1 c2)) d
    | UndefConst, _ | _, UndefConst -> UidM.add u SymConst.UndefConst d
    | NonConst, _ | _, NonConst -> UidM.add u SymConst.NonConst d
  in

  let handle_bop_cnd ll_to_fn ll op1 op2 =
    match op1, op2 with
    | Const c1, Const c2 ->
      UidM.add u (SymConst.Const ((ll_to_fn ll) c1 c2)) d
    | Id id, Const c ->
      add_mapping ll_to_fn ll (UidM.find id d) (Const c)
    | Const c, Id id ->
      add_mapping ll_to_fn ll (Const c) (UidM.find id d) 
    | Id id1, Id id2 ->
      add_mapping ll_to_fn ll (UidM.find id1 d) (UidM.find id2 d)
    | _ -> UidM.add u SymConst.NonConst d
  in

  match i with
  | Binop (bop, _, op1, op2) ->
    handle_bop_cnd bop_to_fn bop op1 op2
  | Icmp (cnd, _, op1, op2) ->
    handle_bop_cnd cnd_to_fn cnd op1 op2
  | Store _ | Call (Void, _, _) -> UidM.add u SymConst.UndefConst d
  | _ -> UidM.add u SymConst.NonConst d

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow
    
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  = 
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the meet over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds : fact list) : fact = 
      let combine' (d1 : fact) (d2 : fact) : fact =
        let f _ sc_opt1 sc_opt2 =
          match sc_opt1, sc_opt2 with
          | Some sc1, Some sc2 -> if SymConst.compare sc1 sc2 < 0 then sc_opt2 else sc_opt1
          | Some sc, None | None, Some sc -> Some sc
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
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right 
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper 
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in
  

  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    failwith "Constprop.cp_block unimplemented"
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg

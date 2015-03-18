module LambdaJS.ConstraintSolver

open Permissions
open Syntax
open Text.PrettyPrint
open Text.PrettySeqs
open AbstractTypes
open ConstraintGenerator
open System.Collections.Generic
open System.Linq
open Utils.Collections
open Utils.Log
open AbstractOp


//lst : Set<Constraint>, cache : Map<Label, Expr>, lambdas : Map<Lambda, Expr>, refs : Map<Ref, Expr> , vars : id list, perms : Set<Permission>

let cnt = ref 0

let iteration = ref 0
let time = ref (int64 0)
let chrono = System.Diagnostics.Stopwatch()

type Graph (cache : Map<Label, Expr>, lambdas : Map<Lambda, Expr>, refs : Map<Ref, Expr> , vars : id list, perms : Permission) = 
  let d   = Dictionary<CElem, ValAt>()
  let e   = Dictionary<CElem, ResizeArray<Constraint>>()
  let p   = Dictionary<PElem, Permission>()
  let pe  = Dictionary<PElem, ResizeArray<Constraint>>()
  let w   = Queue<CElem>()
  do for l in cache do 
      do d.Add(C l.Key, ValAt.empty)
      do p.Add(P l.Key, Permission.Bottom)
      do e.Add(C l.Key, ResizeArray<Constraint>())
      do pe.Add(P l.Key, ResizeArray<Constraint>())
  do for v in vars do 
      do d.Add(R v, ValAt.empty)
      do e.Add(R v, ResizeArray<Constraint>())
  do for r in refs do 
      do d.Add(Mu (PermissionBox.Bottom, r.Key), ValAt.empty)
      do e.Add(Mu (PermissionBox.Bottom, r.Key), ResizeArray<Constraint>())
  member self.D  = d
  member self.E  = e
  member self.P  = p
  member self.Pe = pe
  member self.W  = w
  member self.GetCached l = self.GetData(C l)
  member self.GetGamma  v = self.GetData(R v)
  member self.GetRef    r = self.GetData(Mu (PermissionBox.Bottom, r))
  member self.GetData el = self.D.[el]
  member self.refs = refs
  member self.lambdas = lambdas
  member self.Initialize lst = 
    for cc in lst do 
      do incr iteration
      if !iteration % 10000 = 0 then
        do alert "inserted %d of %d constraints" (!iteration) (lst.Count())
      match cc with
      | Constraint.TIncl (t, p)   -> 
//        if t.strings.Count > 0 then
//          do incr cnt
//          do alert "obj %s" (t.strings.First().pretty.pretty)
        do self.Add p t
      | Constraint.Incl (p1, p2)  -> 
        do self.E.[p1].Add cc
      | Constraint.Impl (t, p)    -> 
        match t, p with
        | TIncl (t, p), Incl(p1, p2) -> 
          do self.E.[p].Add cc
          do self.E.[p1].Add cc
        | TIncl (t, p), TIncl(t1, p2) -> 
          do self.E.[p].Add cc
        | TIncl _, PIncl _ -> 
          //WARNING permission inclusion not implemented
          do ()
        | _ -> failwith "Bad implication constraint."
      | Constraint.PIncl (t, p)   -> 
        do () //WARNING Permission Inclusion
      | Constraint.OpIncl (op, ps, p1)  -> 
        for p in ps do
          do self.E.[p].Add cc
      | Constraint.RecordIncl (incl, p) -> 
        match incl with
        | RecordOp.DelFieldAt (p1, p2) -> 
          do self.E.[p1].Add cc
          do self.E.[p2].Add cc
        | RecordOp.GetFieldAt (p1, p2) -> 
          do self.E.[p1].Add cc
          do self.E.[p2].Add cc
        | RecordOp.SetFieldAt (p1, p2, p3) -> 
          do self.E.[p1].Add cc
          do self.E.[p2].Add cc
          do self.E.[p3].Add cc
  member self.Solve () = 
    do alert "resolvng: %d elements" cnt.Value
    do iteration := 0
    while self.W.Count > 0 do
      do incr iteration
      if !iteration % 1000 = 0 then
        do chrono.Stop()
        do alert "%d: remaining %d elements in %f" !iteration self.W.Count (float chrono.ElapsedMilliseconds / 1000.)
        do chrono.Restart()
        //do iteration := 0
      let q = self.W.Dequeue()
//      if q = C 6145 then
//        alert "polok"
      for cc in self.E.[q] do
        match cc with
        | Constraint.Incl (p1, p2) -> 
          do self.Add p2 self.D.[p1]
        | Constraint.Impl (c1, c2) -> 
          match c1, c2 with
          | TIncl (t, p), Incl (p1, p2) -> 
            if self.D.[p].Contains t then
              do self.Add p2 self.D.[p1]
//            else
//              if t.lambdas.Count > 0 then
//                do alert "Lambda %d" (t.lambdas.First())
          | TIncl (t, p), TIncl(t1, p2) -> 
            if self.D.[p].Contains t then
              do self.Add p2 t1
          | _ -> failwith "Bad implication constraint."
        | Constraint.OpIncl (op, ps, p1) -> 
          let args = ps |> Seq.map (fun p -> self.D.[p]) |> Seq.toList
          let res = op_at op args
          do self.Add p1 res
        | Constraint.RecordIncl (op, p)  -> 
          match op with 
          | RecordOp.DelFieldAt (p1, p2) -> 
            let res = abs_del_rec self.D.[p1] self.D.[p2] self.GetData
            do self.Add p res 
          | RecordOp.SetFieldAt (p1, p2, p3) -> 
            let l3 = match p3 with Cache l -> l | _ -> failwith "RecordOp constraints must have third element as Cache"
            let res = abs_set_rec self.D.[p1] self.D.[p2] l3 //self.D.[p3]
            //do self.Replace p res
            do self.Add p res
          | RecordOp.GetFieldAt (p1, p2) -> 
            let res = abs_get_rec self.D.[p1] self.D.[p2] self.GetData
            do self.Add p res
        | Constraint.PIncl _ -> failwith "Perm inclusion not implemented yet"
        | Constraint.TIncl _ -> failwith "Term inclusion should not be here"
  member self.Add q d  = 
    let d_q = self.D.[q]
    if not <| d_q.Contains d then
      do self.D.[q] <- d_q + d
      //if not <| self.W.Contains q then
      do self.W.Enqueue q
  member self.pretty = 
    let id x = x
    pretty_map prettifier prettifier ";" <| dict_to_map id id self.D
    //t_int self.D.Count
//  member self.resolve () = 
//    let resolver elem = self.D.[elem]

      
let resolve_constraint (s : state) = 
  do printfn "pizza"
  let g, time = Utils.SpeedStats.execute_time 
                          ( fun () -> 
                            let g = Graph(s.cache, s.lambdas, s.refs, s.vars, s.perms)  
                            do g.Initialize(s.lst)
                            do g.Solve()
                            g)
  do printfn "resolution time: %f s" (float time / 1000.)
  let glob = g.GetRef(g.GetData(Gamma "$global").refs.First())
  do System.Console.Beep()
  do printfn "Done"
  //do printfn "%s" (text "result:" ./. g.pretty).pretty




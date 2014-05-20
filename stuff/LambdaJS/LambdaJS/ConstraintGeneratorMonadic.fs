﻿module LambdaJS.ConstraintGeneratorMonadic
//
//open LambdaJS.Permissions
//open Syntax
//open AbstractTypes
//open Traverse
//open Utils.Monad.Yield
//open Text.PrettyPrint
//
//module E = Syntax.Pattern
//
//
//type state private (lst : Set<Constraint>, cache : Map<Label, Expr>, lambdas : Map<Lambda, Expr>, refs : Map<Ref, Expr> , vars : id list, perms : Permission) = 
//  inherit Yieldable<Constraint>()
//  member val lst = lst with get
//  member val cache = cache with get
//  member val lambdas = lambdas with get
//  member val vars = vars with get
//  member val refs = refs with get
//  member val perms = perms with get
//  override self.add_yield c = 
//    state(Set.add c self.lst, self.cache, self.lambdas, self.refs, self.vars, self.perms) :> Yieldable<Constraint>
//  static member empty = state(Set.empty, Map.empty, Map.empty, Map.empty, [], Permission.Bottom)
//  static member promote (s : Traverse.state) = state(Set.empty, s.cache, s.lambdas, s.refs, List.ofSeq s.vars, Permission.Bottom)
//
//type SAMonad<'a> = YM<'a, Constraint, state>
//
//let M = builder<Constraint, state>()
//
//let MList = MList(M)
//
//let get_state : SAMonad<state> = fun s -> s, s
//
//let execute_get (f : SAMonad<'a>) s = let (r, s) = f s in r, s
//
//let get_lambdas : SAMonad<_> = 
//  M{
//    let! s = get_state
//    return s.lambdas
//  }
//
//let get_refs : SAMonad<_> = 
//  M{
//    let! s = get_state
//    return s.refs
//  }
//
//let get_perms : SAMonad<_> = 
//  M{
//    let! s = get_state
//    return s.perms
//  } 
//
//let rec generate (e : Expr) : SAMonad<_> = 
//  M{
//    let l = e.Label
//    //let C_l = C l
//    match e with
//    | E.String s                  -> 
//      let s_at = ValAt.from_string s
//      yield s_at <-- C l
//    | E.Number n                  -> 
//      let s_at = ValAt.from_num n
//      yield s_at <-- C l
//    | E.Bool b                    -> 
//      let s_at = ValAt.from_bool b
//      yield s_at <-- C l 
//    | E.Undefined                 -> 
//      let s_at = ValAt.from_undefined ()
//      yield s_at <-- C l 
//    | E.Null                      -> 
//      let s_at = ValAt.from_null ()
//      yield s_at <-- C l 
//    | E.Id x                      -> 
//      yield R x <== C l
//    | E.Lambda (ids, e0)          -> 
//      let lambda = match e.Tag with Some (Tag.Lambda l) -> l | _ -> failwith "Not tagged lambda... Something went wrong..."
//      do! generate e0
//      yield ValAt.from_lambda lambda <-- C l
//    | E.Object ps                 -> 
//      for (_, e_i) in ps do
//        let l_i = e_i.Label
//        do! generate e_i
//        yield P(l_i) |== P l
//      let binds = ps |> List.map (fun (str, e_i) -> str, e_i.Label)
//      yield ValAt.from_binds binds <-- C l
//    | E.Let (binds, e')           -> 
//      for (x_i, e_i) in binds do
//        do! generate e_i
//        let l_i = e_i.Label
//        yield P l_i |== P l
//        yield C l_i <== R x_i
//      do! generate e'
//      let l' = e'.Label
//      yield P l' |== P l
//      yield C l' <== C l
//    | E.App (e1, acts)            -> 
//      // WARNING test con |actuals| = |formals|
//      do! generate e1
//      let l1 = e1.Label
//      yield P e1.Label |== P l
//      for e_i in acts do
//        do! generate e_i
//        let l_i = e_i.Label
//        yield P l_i |== P l
//      let! lambdas = get_lambdas
//      for lambda in lambdas do
//        let l0 = lambda.Key
//        let (E.Lambda (ids, e0)) = lambda.Value
//        // test con |actuals| = |formals|... Remove if to check all
//        let t = ValAt.from_lambda l0 
//        if ids.Length = acts.Length then
//          for (f, a) in List.zip ids acts do
//            let l2 = a.Label
//            yield t <-- C l1 ==> (C l2 <== R f)
//          yield t <-- C l1 ==> (C l0 <== C l)
//          yield t <-- C l1 ==> (P l0 |== P l)
//    | E.Op (op, es)               -> 
//      // WARNING check cardinality
//      for e_i in es do
//        do! generate e_i
//        let l_i = e_i.Label
//        yield P l_i |== P l
//      let lis = es |> List.map (fun e -> C e.Label)
//      yield Constraint.OpIncl (op, lis, C l)
//    | E.If (e0, e1, e2)           -> 
//      do! generate e0
//      do! generate e1
//      do! generate e2
//      let l0 = e0.Label
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l0 |== P l
//      yield true_at <-- C l0 ==> (C l1 <== C l)
//      yield true_at <-- C l0 ==> (P l1 |== P l)
//      yield false_at <-- C l0 ==> (C l2 <== C l)
//      yield false_at <-- C l0 ==> (P l2 |== P l)
//    | E.While (e1, e2)            -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield true_at <-- C l1 ==> (C l2 <== C l)
//      yield true_at <-- C l1 ==> (P l2 |== P l)
//      yield false_at <-- C l1 ==> (ValAt.from_undefined () <-- C l)
//    | E.GetField (e1, e2)         -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield (Constraint.RecordIncl (RecordOp.GetFieldAt(C l1, C l2), C l))
//    | E.UpdateField (e1, e2, e3)  -> 
//      do! generate e1
//      do! generate e2
//      do! generate e3
//      let l1 = e1.Label
//      let l2 = e2.Label
//      let l3 = e3.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield P l3 |== P l
//      yield (Constraint.RecordIncl (RecordOp.SetFieldAt(C l1, C l2, C l3), C l))
//    | E.DeleteField (e1, e2)      -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield (Constraint.RecordIncl (RecordOp.DelFieldAt(C l1, C l2), C l))
//    | E.Ref e1                    -> 
//      do! generate e1
//      let l1 = e1.Label
//      let r = match e.Tag with Some (Tag.Ref r) -> r | _ -> failwith "Not tagged ref... Something went wrong..."
//      yield ValAt.from_ref r <-- C l
//      yield P l1 |== P l
//      // WARNING rho r rho s
//      let! ps = get_perms
//      yield C l1 <== Mu(Seq.last ps.Content, r)
//    | E.Deref e1                  -> 
//      do! generate e1
//      let l1 = e1.Label
//      yield P l1 |== P l
//      let! refs = get_refs
//      let! ps = get_perms
//      //abominiooooooooooooooooooooooooooooooooooooooooooooooooooooooo
//      for r in refs do
//        let r = r.Key
//        //pr deve esistere!!!!!!!!!!!!!!!!!!!
//        for pr in ps.Content do
//          yield ValAt.from_ref r <-- C l1 ==> (Mu (pr, r) <== C l)
//    | E.SetRef (e1, e2)           -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield C l2 <== C l
//      let! refs = get_refs
//      let! ps = get_perms
//      for r in refs do
//        let r = r.Key
//        for pr in ps.Content do
//          yield ValAt.from_ref r <-- C l1 ==> (C l2 <== Mu (pr, r))
//    // WARNING hic sunt leones
//    | E.Seq (e1, e2)              -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield C l2 <== C l
//    | E.Label (_, e1)             -> 
//      do! generate e1
//      let l1 = e1.Label
//      yield P l1 |== P l
//      yield C l1 <== C l
//    | E.Break (_, e1)             -> 
//      do! generate e1
//      let l1 = e1.Label
//      yield P l1 |== P l
//      yield C l1 <== C l
//    | E.Throw e1                  -> 
//      do! generate e1
//      let l1 = e1.Label
//      yield P l1 |== P l
//      yield C l1 <== C l
//    | E.Catch (e1, e2)            -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield C l1 <== C l
//      yield C l2 <== C l
//    | E.Finally (e1, e2)          -> 
//      do! generate e1
//      do! generate e2
//      let l1 = e1.Label
//      let l2 = e2.Label
//      yield P l1 |== P l
//      yield P l2 |== P l
//      yield C l1 <== C l
//      //yield C l2 <== C l
//    | E.Eval                      -> 
//      ()
//    | other                       -> 
//      failwith "why here?"
//  }
//
//let generate_constraints (Syntax.Program.P e) (s : Traverse.state) = 
//  let (_, state), time = Utils.SpeedStats.execute_time (fun () -> execute_get (generate e) (state.promote s))
//  Utils.Log.alert "Time not monadic %f sec"  (float time / 1000.)
//  state
//
//let show_stats (state : state) = 
//  let tIncl       = ref 0
//  let incl        = ref 0
//  let pIncl       = ref 0
//  let opIncl      = ref 0
//  let recordIncl  = ref 0
//  let impl        = ref 0
//  //let incr x = do x <- x + 1
//  for c in state.lst do
//    match c with
//    | TIncl _       -> do incr tIncl
//    | Incl  _       -> do incr incl
//    | PIncl _       -> do incr pIncl
//    | OpIncl _      -> do incr opIncl
//    | RecordIncl _  -> do incr recordIncl
//    | Impl _        -> do incr impl
//  let res = 
//    text "Total:"       <+> t_int state.lst.Count .|.
//    text "TIncl:"       <+> t_int !tIncl          .|.
//    text "Incl:"        <+> t_int !incl           .|.
//    text "PIncl:"       <+> t_int !pIncl          .|.
//    text "OpIncl:"      <+> t_int !opIncl         .|.
//    text "RecordIncl:"  <+> t_int !recordIncl     .|.
//    text "Impl:"        <+> t_int !impl
//  Utils.Log.alert "%s" res.pretty
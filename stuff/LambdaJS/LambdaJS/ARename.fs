module LambdaJS.ARename

open LambdaJS.Syntax
open Utils.Monad
open Text.PrettyPrint

module P = LambdaJS.Syntax.Pattern

type N = Expr
type E = EExpr

type id = string

type state = 
  {
    n   : int
    env : Map<id, id>
  }
  static member empty = { n = 0; env = Map.empty }
  static member merge s1 s2 = 
    { s1 with n = s2.n }
  static member merge_impl s1 : SM<unit> = 
    fun s -> 
      ((), state.merge s1 s)
    
and SM<'a> = M<'a, state>
let M = builder<state>()
let MList = MList M

let add_bind id : SM<id> = 
  fun s ->
    match Map.tryFind id s.env with
    | None -> 
      id, { s with env = Map.add id id s.env }
    | Some _ -> 
      let nid = sprintf "%s%d" id s.n
      let n = s.n + 1
      nid, { s with env = Map.add id nid s.env; n = n }

let get_bind id = 
  M{
    let! s = get_state
    match Map.tryFind id s.env with
    | None ->
      do Utils.Log.alert "Variable %s not found. Free variable?" id
      return id//failwith (sprintf "Error, found a free variable %s" id)
    | Some id -> return id 
  }

(*let de_let_rec (e : Expr) = 
    //let b_id ((id, _), _) = id
    let rec de_let_rec acc (e : Expr) = 
      match e.value with
      | E.ELet (binds, e1) -> 
        let are_all_new = binds |> List.exists (fun ((id, _), _) -> List.exists (fun ((id2, _), _) -> id = id2) acc)
        if are_all_new then 
          let acc = acc@binds
          de_let_rec acc e1
        else
          acc
      | _ -> 
        acc
    de_let_rec [] e
    

  let print_state = 
    M{
      let! s = get_state
      let map = s.env |> Map.toList |> List.map (fun (id, nid) -> text id <+> arrow <+> text nid)
      printfn "%s" <| (parens (xvcat ";" map)).pretty
    }*)


let rec traverse (e : Expr) = 
  M{
    let N ee = N(ee, e.Loc)
    match e.value with
    | E.EBool _ 
    | E.EEval 
    | E.ENull 
    | E.ENumber _ 
    | E.EString _ 
    | E.EUndefined -> return e
    | E.EApp (e1, es) -> 
      let! e1 = traverse e1
      let! es = MList.map traverse es
      return N(E.EApp(e1, es))
    | E.EBreak ((id, orig), e2) -> 
      let old_l = id
      let! new_l = get_bind old_l
      let! e2 = traverse e2
      return N(E.EBreak((new_l, old_l), e2))
    | E.ECatch (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.ECatch(e1, e2))
    | E.EFinally (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.EFinally(e1, e2))
    | E.EGetField (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.EGetField(e1, e2))
    | E.EDeleteField (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.EDeleteField(e1, e2))
    | E.ESeq (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.ESeq(e1, e2))
    | E.ESetRef (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.ESetRef(e1, e2))
    | E.EWhile (e1, e2) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      return N(E.EWhile(e1, e2))
    | E.EDeref e1 ->
      let! e1 = traverse e1
      return N (E.EDeref e1)
    | E.EId (id, orig) -> 
      let old_l = id
      let! new_l = get_bind old_l
      return N(E.EId (new_l, old_l))
    | E.EIf (e1, e2, e3) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      let! e3 = traverse e3
      return N <| E.EIf(e1, e2, e3)
    | E.ELabel ((id, orig), e1) -> 
      let! s = get_state
      let! n_id = add_bind id
      let! e1 = traverse e1
      let res = N <| E.ELabel ((n_id, id), e1)
      //do! state.merge_impl s
      return res
    | E.ELambda (binds, e1) -> 
      //do Utils.Log.alert_not_null "processing node with label %O" e.pretty
      let! s = get_state
      let f (id, orig) = 
        M{
          let! n_id = add_bind id
          //do Utils.Log.alert "Lambda: renamed %s -> %s" orig id
          return (n_id, id)
        }
      let! binds = MList.map f binds
      let! e1 = traverse e1
      //do! state.merge_impl s
      return N <| E.ELambda (binds, e1)
    | E.ELet (binds, e1) -> 
//      let! orig_s = get_state
//      let binds = de_let_rec e
//      let ids, _ = List.unzip binds
//      let! n_binds = MList.map (fun (id, old) -> M{ let! n_id = add_bind id in return n_id, id} ) ids
//      //do printfn "%s" (xvcat ";" (List.map (fun (n, o) -> text n <|> text "->" <|> text o) (List.filter (fun (x, y) -> x<>y)n_binds))).pretty
//      let! res = traverse_let e
//      do! state.merge_impl orig_s
//      return res
      let! s = get_state
      let f ((id, orig), e1) = 
        M{
          let! e1 = traverse e1
          let! n_id = add_bind id
          //do Utils.Log.alert "Let: renamed %s -> %s" orig id
          return (n_id, id), e1
        }
      let! binds = MList.map f binds
      let! e1 = traverse e1
      //do! state.merge_impl s
      return N <| E.ELet (binds, e1)
    | E.EObject ts ->
      let! ts =  MList.map (fun (l, e) -> M{ let! e = traverse e in return l, e }) ts
      return N <| E.EObject ts
    | E.EOp (op, es) -> 
      let! es = MList.map traverse es
      return N <| E.EOp (op, es)
    | E.ERef e1 -> 
      let! e1 = traverse e1
      return N <| E.ERef e1
    | E.EThrow e1 -> 
      let! e1 = traverse e1
      return N <| E.EThrow e1
    | E.EUpdateField (e1, e2, e3) -> 
      let! e1 = traverse e1
      let! e2 = traverse e2
      let! e3 = traverse e3
      return N <| E.EUpdateField (e1, e2, e3)
  }

//and traverse_let (e : Expr) = 
//  M{
//    match e.value with
//    | E.ELet (binds, e) -> 
//      do printfn "found...%s" (binds |> List.map (text << fst << fst) |> xvcat ";").pretty
//      do! print_state
//      let! binds = binds |> MList.map (fun ((id, _), e) -> M{ let! e = traverse e in let! nid = get_bind id in return (nid, id), e }) 
//      let! e = traverse_let e
//      return Expr(E.ELet (binds, e), e.Loc)
//    | _ -> return! traverse e
//  }

let rename (P e) = 
  let e, s = execute_get (traverse e) state.empty 
  do Utils.Log.alert "number of renamed_matches = %d of %d total variables" s.n s.env.Count
  P e
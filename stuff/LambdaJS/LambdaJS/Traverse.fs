module LambdaJS.Traverse

open LambdaJS.Syntax
open Utils.Monad
open Text.PrettyPrint

module E = Syntax.Pattern

//let mutable up_del_get  : Set<string> = Set.empty

type state = 
  {
    ref_cnt     : int
    lambda_cnt  : int
    labels_cnt  : int
    refs        : Map<Ref, Expr>
    lambdas     : Map<Lambda, Expr>
    cache       : Map<Label, Expr>
    vars        : id list
  }
  with 
    member private self.print (doc : bool) = 
      let c_count = text "cache Count:" <+> t_int self.labels_cnt
      let l_count = text "lambda Count:" <+> t_int self.lambda_cnt
      let l_count = text "refs Count:" <+> t_int self.ref_cnt
      let print_map map = 
        Map.fold (
                  fun s k (t : Expr) -> 
                    let d = t_gen k <|> (text ":  " <+> if doc then t.pretty_doc else (text t.pretty))
                    s@[d]
                 ) [] map
      let cache = text "cache: " <|> parens (xvcat ";" (print_map self.cache))
      let lambdas = text "lambdas: " <|> parens (xvcat ";" (print_map self.lambdas))
      let lambdas = text "refs: " <|> parens (xvcat ";" (print_map self.refs))
      (braces (c_count .+. cache .+. l_count .+. lambdas)).pretty
    member self.pretty_doc = 
      self.print true
    member self.pretty = 
      self.print false
    static member empty = 
      {
        ref_cnt     = 0
        lambda_cnt  = 0
        labels_cnt  = 0
        refs        = Map.empty
        lambdas     = Map.empty
        cache       = Map.empty
        vars        = []
      }

type SMonad<'a> = M<'a, state>

let M = builder()

let MList = Utils.Monad.List(M)

let add_ref (e : Expr) = 
  fun (s : state) ->
    let t = s.ref_cnt
    do e.Tag <- Some <| Tag.Ref t
    ((), { s with ref_cnt = s.ref_cnt + 1; refs = Map.add t e s.refs })
    
let add_lambda (e : Expr) = 
  fun (s : state) ->
    let t = s.lambda_cnt
    do e.Tag <- Some <| Tag.Lambda t
    ((), { s with lambda_cnt = s.lambda_cnt + 1; lambdas = Map.add t e s.lambdas })

let add_label (e : Expr) = 
  fun (s : state) ->
    let t = s.labels_cnt
    do e.Label <- t
    ((), { s with labels_cnt = s.labels_cnt + 1; cache = Map.add t e s.cache })

let add_var (id : id) = 
  fun (s : state) -> 
    if List.exists ((=) id) s.vars then
      do Utils.Log.alert "WARNING: variable %s aready present" id
    ((), { s with vars = id::s.vars })

let rec traverse f (e : Expr)  = 
  M{
    let traverse = traverse f
    let f e = 
      M{
        do! f e
        return! traverse e
      }
    match e with
    | E.String _
    | E.Number _ 
    | E.Bool _ 
    | E.Undefined 
    | E.Null 
    | E.Id _ -> ()
    | E.Lambda (ids, e)           -> 
      do! MList.iter add_var ids
      do! f e
    | E.Object ps                 -> 
      do! MList.iter f <| List.map snd ps
    | E.Op (_, es)                -> 
      do! MList.iter f es
    | E.App (c, acts)             -> 
      do! MList.iter f (c::acts)
    | E.Let (binds, e1)            -> 
      let ids, es = List.unzip binds
      do! MList.iter add_var ids
      do! MList.iter f es
      do! f e1
    | E.SetRef (e1, e2)           -> 
      do! f e1
      do! f e2
    | E.Ref e1                    -> 
      do! f e1
    | E.Deref e1                  -> 
      do! f e1
    | E.GetField (e1, e2)         -> 
      do! f e1
      do! f e2
    | E.UpdateField (e1, e2, e3)  -> 
      do! f e1
      do! f e2
      do! f e3
    | E.DeleteField (e1, e2)      -> 
      do! f e1
      do! f e2
    | E.Seq (e1, e2)              -> 
      do! f e1
      do! f e2
    | E.If (e1, e2, e3)           -> 
      do! f e1
      do! f e2
      do! f e3
    | E.While (e1, e2)            -> 
      do! f e1
      do! f e2
    | E.Label (_, e1)             -> 
      do! f e1
    | E.Break (_, e1)             -> 
      do! f e1
    | E.Throw e1                  -> 
      do! f e1
    | E.Catch (e1, e2)            -> 
      do! f e1
      do! f e2
    | E.Finally (e1, e2)          -> 
      do! f e1
      do! f e2
    | E.Eval                      -> 
      ()
    | other                       ->
      failwith "why here?"
    return! add_label e
  }


    
let add_labels_lambda (P e) = 
  let search_ref e = 
    M{
      match e with
      | E.Ref _ -> 
        return! add_ref e
      | E.Lambda (b, _) -> 
        return! add_lambda e
      | _ -> ()
    }
  let res = execute_get (traverse search_ref e) state.empty |> snd
  do Utils.Log.alert "cache size: %d" res.labels_cnt
  do Utils.Log.alert "gamma size: %d" (Set.ofList res.vars).Count
  do Utils.Log.alert "lambdas size: %d" res.lambda_cnt
//  do Utils.Log.alert "updates: %s" (Text.PrettySeqs.pretty_seq text ";" up_del_get).pretty
//  do Utils.Log.alert "update size: %d" up_del_get.Count
  res
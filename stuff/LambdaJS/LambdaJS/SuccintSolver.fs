module LambdaJS.SuccintSolver

open Syntax
open AbstractTypes
open AbstractOp
open System.Collections.Generic

type VarOrVal<'a, 'b> = 
  | Var of 'a
  | Val of 'b

//and Env<'a, 'b when 'a : equality and 'b : equality>() = 
//  member val Content : Dictionary<'a, 'b> = new Dictionary<'a, 'b>() with get, set
//  member self.unify (v : VarOrVal<'a, 'b>) a = 
//    match v with
//    | Val v -> if v = a then Some self else None
//    | Var v -> 
//      if self.Content.ContainsKey v && a = self.Content.[v] then
//        Some self
//      elif self.Content.ContainsKey v |> not then
//        do self.Content.Add(v, a)
//        Some self
//      else
//        None
module Env = 
  
  type Env<'a, 'b when 'a : comparison and 'b : comparison> = 
    | T of Map<'a, 'b option>
    member self.Content = let (T e) = self in e
  
  let empty = T Map.empty

  let lookup i (T env) = 
    match env.TryFind i with
    | None -> failwith "unbound symbol"
    | Some a -> a
  
  let getValue v env = 
    match v with
    | Var x -> lookup x env
    | Val a -> Some a
  
  let get_free x env = 
    match x with
    | Val _ -> []
    | Var x ->
      match lookup x env with
      | None -> [x]
      | Some _ -> []

  let get_free_pair xy env = 
    match xy with
    | Val _, Val _ -> []
    | Val _, ind -> get_free ind env
    | ind, Val _ -> get_free ind env
    | Var x, Var y -> 
      if x = y then [x] else [x; y]

  let get_free_list args env = 
    List.concat (List.map (fun a -> get_free a env) args) |> Set.ofList |> List.ofSeq

  let update (T env) x a = 
    T <| Map.add x (Some a) env

  let update_pair env (x1, x2) (a1, a2) = 
    update (update env x1 a1) x2 a2

  let update_list env xs ys = 
    if List.length xs <> List.length ys then
      Utils.Log.alert "lists have different size"
    let pairs = List.zip xs ys
    pairs |> List.fold (fun s (x, a) -> update env x a) env

  let rec split xs env = 
    match xs with
    | [] -> ([], [])
    | x::xs as l -> 
      match getValue x env with
      | Some a -> 
        let p, s = split xs env
        in a::p, s
      | None -> [], l  
  
  let unifyInd (T cnt as env) v a = 
    match v with
    | Val v -> if v = a then Some env else None
    | Var x -> 
      if cnt.ContainsKey x then
        match cnt.[x] with
        | Some b when b = a -> Some env
        | None -> 
          Some <| T (Map.add x (Some a) cnt )
        | _ -> None
      else
        do Utils.Log.alert "Warning! Occurrence of globally free var..."
        Some (T <| Map.add x (Some a) cnt)
  
  let rec unify env xs ys = 
    match xs, ys with
    | [], [] -> Some env
    | i::ii, a::aa ->
      match getValue i env with
      | Some b -> if a = b then unify env ii aa else None
      | None -> 
        let (Var x) = i
        let env = update env x a
        in unify env ii aa
    |  [], _ -> 
      do Utils.Log.alert "Error during unification: too few args"
      None
    |  _, [] -> 
      do Utils.Log.alert "Error during unification: too short result"
      None

  let rec unifyAll env args xs = 
    match xs with
    | [] -> empty
    | h::t -> 
      let result = unifyAll env args t
      match unify env args h with
      | Some (T env) -> 
        // WARNING hazardous... Spero di aver copiato bene...
        Map.fold (fun s k v -> update s k v ) result env
      | None -> result

    

type id = VarOrVal<string, int>

type Pre = 
  | R of id list
  | NotR of id list
  | And of Pre * Pre
  | Or  of Pre * Pre
  | Exists of id * Pre
  | Forall of id * Pre

and Clause = 
  | R of id list
  | True
  | And of Clause * Clause
  | Impl of Pre * Clause
  | Forall of id * Clause

and System = S of ResizeArray<Clause>

let result = ResizeArray<int>()

//let rec solve (S clauses) = 
//  for c in clauses do
//    do execute c Env<string, int>.empty

let rec execute (c : Clause) env = 
  match c with
  | R vs -> failwith "not yet ready"
  | True -> ()
  | And (cl1, cl2) -> 
    do execute cl1 env
    do execute cl2 env
  | Impl (pre, cl) -> 
    check pre (execute cl) env
  | Forall (id, cl) -> 
    let env' = failwith "not yet ready"
    in execute cl env'

and check pre next env = 
  match pre with
  | Pre.R vs -> failwith "not yet ready"
  | Pre.NotR vs -> failwith "not yet ready"
  | Pre.And (pre1, pre2) -> check pre1 (check pre2 next) env
  | Pre.Or  (pre1, pre2) -> failwith "not yet ready"
  | Pre.Exists (x, cl) -> 
    let g = failwith "not yet ready"
    let next' = next << g
    let env' = failwith "not yet ready"
    in check cl next' env'
  | Pre.Forall (x, cl) -> failwith "not yet ready"



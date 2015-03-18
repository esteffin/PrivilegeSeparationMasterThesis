module LambdaJS.AbstractOp

open Syntax
open Text.PrettyPrint
open Utils.Log
open Utils.Collections
open AbstractTypes

module Ty_names = 
  let func    = "function"
  let obj     = "object"
  let undef   = "undefined"
  let num     = "number"
  let str     = "string"
  let bool    = "boolean"
  let lambda  = "lambda"
  let null_   = "null"
  let ref     = "location"

type O = Op
let curry f (x, y) = f x y

let num_plus (x : ValAt) (y : ValAt)  = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x + y in s + res) Set.empty |> ValAt.from_num_at_set
  results

let str_plus (x : ValAt) (y : ValAt)  = 
  let pairs = cartesian x.strings y.strings
  let results = pairs |> Seq.map (curry (+)) |> Set.ofSeq |> ValAt.from_string_at_set
//  try 
//    do alert "----%s + %s = %s" (x.strings |> Seq.last).pretty.pretty (y.strings |> Seq.last).pretty.pretty (results.strings |> Seq.last).pretty.pretty
//  with _ -> ()
  results

let num_sub (x : ValAt) (y : ValAt)   = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x - y in s + res) Set.empty |> ValAt.from_num_at_set
  results
  
let num_times (x : ValAt) (y : ValAt) = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x * y in s + res) Set.empty |> ValAt.from_num_at_set
  results

let num_div (x : ValAt) (y : ValAt)   = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x / y in s + res) Set.empty |> ValAt.from_num_at_set
  results

let num_mod (x : ValAt) (y : ValAt)   = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x % y in s + res) Set.empty |> ValAt.from_num_at_set
  results

let num_band (x : ValAt) (y : ValAt)  = 
  //WARNING Numberic Binary And not implemented. Using Top as result
  ValAt.from_num_at_set NumAt.Top

let num_bor (x : ValAt)  (y : ValAt)  = 
  //WARNING "Numberic Binary Or not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top

let num_bxor (x : ValAt) (y : ValAt)  = 
  //WARNING "Numberic Binary Xor not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top

let num_not (x : ValAt)               = 
  //WARNING "Numberic Binary Not not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top

let num_shl (x : ValAt) _             = 
  //WARNING "Numberic Shift Left not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top

let num_shr (x : ValAt) _             = 
  //WARNING "Numberic Shift Right not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top

let num_lshr (x : ValAt) _            = 
  //WARNING "Numberic Logic Shift Right not implemented. Using Top as result"
  ValAt.from_num_at_set NumAt.Top
  
let num_less (x : ValAt) (y : ValAt)  = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x <^ y in s + res) Set.empty |> ValAt.from_bool_at_set
  results

let num_leq (x : ValAt) (y : ValAt)   = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x <=^ y in s + res) Set.empty |> ValAt.from_bool_at_set
  results

let num_great (x : ValAt) (y : ValAt) = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x >^ y in s + res) Set.empty |> ValAt.from_bool_at_set
  results

let num_geq (x : ValAt) (y : ValAt)   = 
  let pairs = cartesian x.nums y.nums
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x >=^ y in s + res) Set.empty |> ValAt.from_bool_at_set
  results
  
let str_less (x : ValAt) (y : ValAt)  = 
  let pairs = cartesian x.strings y.strings
  let results = pairs |> Seq.fold (fun s (x, y) -> let res = x <^ y in s + res) Set.empty |> ValAt.from_bool_at_set
  results
  
let abs_eq (x : ValAt) (y : ValAt)   = x ==^ y

let strict_eq (x : ValAt) (y : ValAt)= x ===^ y
  
let to_integer (x : ValAt) = x.nums |> ValAt.from_num_at_set

let to_uint32 (x : ValAt) = x.nums |> Set.fold (fun s x -> match x with Minus _ -> s + set [Zero; Plus] | _ -> Set.add x s) Set.empty |> ValAt.from_num_at_set

let to_int32 (x : ValAt) = x.nums |> ValAt.from_num_at_set

//WARNING da provare
let surface_typeof (x : ValAt) = 
  let res = 
      [
        if x.conds.Count > 0 then
          yield Ty_names.bool
        if x.nums.Count > 0 then
          yield Ty_names.num
        if x.strings.Count > 0 then
          yield Ty_names.str
        if x.Null then
          yield Ty_names.obj
        if x.Undefined then
          yield Ty_names.undef
        match x.objs with 
        | None -> () 
        | Some o ->
          yield Ty_names.obj
          if o.HasOwnProp (StringAt.from_e_string "$code") |> Set.contains TrueAt then
            yield Ty_names.func
      ] |> Seq.fold (fun s x -> StringAt.from_string_s x + s) Set.empty |> ValAt.from_string_at_set
  //do alert "********* surface typeof (%s) : %s" x.pretty.pretty res.pretty.pretty
  res

//WARNING da provare
let typeof (x : ValAt) = 
  let res = 
      [
        if x.conds.Count > 0 then
          yield Ty_names.bool
        if x.lambdas.Count > 0 then
          yield Ty_names.lambda
        if x.nums.Count > 0 then
          yield Ty_names.num
        if x.refs.Count > 0 then
          yield Ty_names.ref
        if x.strings.Count > 0 then
          yield Ty_names.str
        if x.Null then
          yield Ty_names.null_
        if x.Undefined then
          yield Ty_names.undef
        match x.objs with 
        | None -> () 
        | Some o ->
          yield Ty_names.obj
      ] |> Seq.fold (fun s x -> StringAt.from_string_s x + s) Set.empty |> ValAt.from_string_at_set
  //do alert "++++++typeof (%s) : %s" x.pretty.pretty res.pretty.pretty
  res

let prim_to_num (x : ValAt) = 
  let res = 
      [
        yield x.nums
        for c in x.conds do
          match c with
          | TrueAt -> 
            yield sset NumAt.Plus
          | FalseAt -> 
            yield sset Zero
        if x.Undefined then
          yield NumAt.Bottom
        if x.Null then
          yield sset NumAt.Zero
        for s in x.strings do
          //WARNING string parse??
          yield NumAt.Top
      ] |> Seq.fold (fun s n -> n + s) Set.empty |> ValAt.from_num_at_set
  res

let prim_to_string (x : ValAt) = 
  let res = 
      [
        //yield x.strings
        if x.Undefined then
          yield StringAt.from_string_s "undefined"
        if x.Null then
          yield StringAt.from_string_s "null"
        for c in x.conds do
          match c with
          | TrueAt -> 
            yield StringAt.from_string_s "true"
          | FalseAt -> 
            yield StringAt.from_string_s "false"
        for n in x.nums do 
          match n with
          | Zero -> 
            yield StringAt.from_string_s "0"
          | _ -> 
            yield sset StringAt.Top
          ] |> Seq.fold (fun s v -> v + s) x.strings |> ValAt.from_string_at_set
  res
  
let prim_to_bool (x : ValAt) = 
  let res = 
      [
        if x.Undefined then
          yield FalseAt
        if x.Null then
          yield FalseAt
        if x.nums.Count > 0 then
          yield FalseAt
        if Set.exists (fun n -> n = Plus || n = Minus) x.nums then
          yield TrueAt
        if x.objs <> None then
          yield TrueAt
        if x.strings.Count > 0 then
          yield FalseAt
          yield TrueAt
          ] |> Seq.fold (fun s v -> Set.add v s) x.conds |> ValAt.from_bool_at_set
  res

let is_prim (x : ValAt) = x.IsPrim  |> ValAt.from_bool_at_set

let has_own_prop (x : ValAt) (y : ValAt) = 
  match x.objs with
  | None -> BoolAt.Bottom |> ValAt.from_bool_at_set
  | Some x -> 
    let pairs = y.strings |> Seq.map (fun s -> x, s)
    let res = pairs |> Seq.fold (fun s (x, y) -> x.HasOwnProp y + s) Set.empty |> ValAt.from_bool_at_set
    res

let print_string (x : ValAt) = 
  for s in x.strings do
    printfn "%s" s.pretty.pretty
  ValAt.from_undefined()

let str_contains (x : ValAt) (y : ValAt) = 
  let pairs = cartesian x.strings y.strings
  let res = pairs |> Seq.fold (fun s (x, y) -> x.Contains y + s) Set.empty |> ValAt.from_bool_at_set
  res

//WARNING da sistemare come fa Giulia
let str_startswith (x : ValAt) (y : ValAt) = 
  BoolAt.Top |> ValAt.from_bool_at_set

let str_len (x : ValAt) = 
  x.strings |> Seq.fold (fun s x -> x.Length + s) Set.empty |> ValAt.from_num_at_set

let obj_can_delete (x : ValAt) (y : ValAt) = 
  //WARNING massa casino... TOP
//  match x.objs with
//  | None -> ValAt.empty
//  | Some o -> 
//    let pairs = y.strings |> Seq.map (fun x -> o, x)
//    let res = 
//      pairs |> Seq.fold
//        (fun s (x, y) ->
//          x.Value |> Map.fold (fun s v -> ) Set.empty
//        ) Set.empty
//  res
  BoolAt.Top |> ValAt.from_bool_at_set

let math_sin (x : ValAt) = 
  let res = if x.nums |> List.ofSeq = [Zero] then sset Zero else NumAt.Top
  res |> ValAt.from_num_at_set

let math_cos (x : ValAt) = NumAt.Top |> ValAt.from_num_at_set

let math_log (x : ValAt) = 
  let res = 
    if x.nums = Set.empty || x.nums = sset Minus then NumAt.Bottom
    elif x.nums = sset Zero then sset Minus
    else NumAt.Top
  res |> ValAt.from_num_at_set

let math_exp (x : ValAt) = 
  set [Zero; Plus] |> ValAt.from_num_at_set

let math_abs (x : ValAt)             = 
  x.nums |> Seq.fold (fun s x -> Set.add (match x with Minus -> Plus | _ -> x) s ) Set.empty |> ValAt.from_num_at_set

let math_pow (x : ValAt) (y : ValAt) = 
  //WARNING massa casino... TOP
  NumAt.Top |> ValAt.from_num_at_set

let regexp_match (x : ValAt) (y : ValAt) = 
  //WARNING ne capito, ne fatto... Uso TOP
  sset StringAt.Top |> ValAt.from_string_at_set

let str_split_regexp (x : ValAt) (y : ValAt) = 
  //WARNING ne capito, ne fatto... Uso TOP
  sset StringAt.Top |> ValAt.from_string_at_set
  
let str_split_str_exp (x : ValAt) (y : ValAt) = 
  //WARNING ne capito, ne fatto... Uso TOP
  sset StringAt.Top |> ValAt.from_string_at_set

//WARNING the next function docs can be found here:
//http://docs.racket-lang.org/reference/hashtables.html
let obj_iter_has_next (x : ValAt) (y : ValAt) = 
  //WARNING Serve per vedere se y ha un next nel record... TOP
  BoolAt.Top |> ValAt.from_bool_at_set

let obj_iter_key (x : ValAt) (y : ValAt) = 
  //get the key of the obj at index y... If Valid then all, otherwise Bottom
  if y.nums |> Set.contains Plus || y.nums.Contains Zero then
    let keys = match x.objs with None -> Set.empty | Some o -> o.GetKeys |> Set.ofList
    keys |> ValAt.from_string_at_set
  else
    ValAt.empty
  
let obj_iter_next (x : ValAt) (y : ValAt) = 
  //get the next element index of a record or false...
  if y.nums.Contains NumAt.Zero || y.nums.Contains NumAt.Plus || y.Undefined then
    (ValAt.from_num_at_set <| set [Plus; Zero]) + (ValAt.from_bool false)
  else
    ValAt.empty
    


let op_at (op : Op) args = 
  match op, args with
  | O.OAbstractEq, [x;y]  -> abs_eq x y
  | O.OAbstractEq, _      -> failwith "Wrong argument number"
  | O.OBAnd, [x;y]        -> num_band x y
  | O.OBAnd, _            -> failwith "Wrong argument number"
  | O.OBNot, [x]          -> num_not x 
  | O.OBNot, _            -> failwith "Wrong argument number"
  | O.OBOr, [x; y]        -> num_bor x y
  | O.OBOr, _             -> failwith "Wrong argument number"
  | O.OBXOr, [x; y]       -> num_bxor x y
  | O.OBXOr, _            -> failwith "Wrong argument number"
  | O.ODiv, [x; y]        -> num_div x y
  | O.ODiv, _             -> failwith "Wrong argument number"
  | O.OLShift, [x; y]     -> num_shl x y
  | O.OLShift, _          -> failwith "Wrong argument number"
  | O.OLt, [x; y]         -> num_less x y
  | O.OLt, _              -> failwith "Wrong argument number"
  | O.OMod, [x; y]        -> num_mod x y
  | O.OMod, _             -> failwith "Wrong argument number"
  | O.OMul, [x; y]        -> num_times x y
  | O.OMul, _             -> failwith "Wrong argument number"
  | O.ONumPlus, [x; y]    -> num_plus x y
  | O.ONumPlus, _         -> failwith "Wrong argument number"
  | O.ORShift, [x; y]     -> num_shr x y
  | O.ORShift, _          -> failwith "Wrong argument number"
  | O.OStrLt, [x; y]      -> str_less x y
  | O.OStrLt, _           -> failwith "Wrong argument number"
  | O.OStrPlus, [x; y]    -> str_plus x y
  | O.OStrPlus, _         -> failwith "Wrong argument number"
  | O.OStrictEq, [x; y]   -> strict_eq x y
  | O.OStrictEq, _        -> failwith "Wrong argument number"
  | O.OSub, [x; y]        -> num_sub x y
  | O.OSub, _             -> failwith "Wrong argument number"
  | O.OToInt32, [x]       -> to_int32 x
  | O.OToInt32, _         -> failwith "Wrong argument number"
  | O.OToInteger, [x]     -> to_integer x
  | O.OToInteger, _       -> failwith "Wrong argument number"
  | O.OToUInt32, [x]      -> to_uint32 x
  | O.OToUInt32, _        -> failwith "Wrong argument number"
  | O.OZfRShift, [x; y]   -> num_lshr x y
  | O.OZfRShift, _        -> failwith "Wrong argument number"
  | O.OHasOwnProp, [x; y] -> has_own_prop x y
  | O.OHasOwnProp, _      -> failwith "Wrong argument number"
  | O.OIsPrim, [x]        -> is_prim x
  | O.OIsPrim, _          -> failwith "Wrong argument number"
  | O.OMathAbs, [x]       -> math_abs x
  | O.OMathAbs, _         -> failwith "Wrong argument number"
  | O.OMathCos, [x]       -> math_cos x
  | O.OMathCos, _         -> failwith "Wrong argument number"
  | O.OMathExp, [x]       -> math_exp x
  | O.OMathExp, _         -> failwith "Wrong argument number"
  | O.OMathLog, [x]       -> math_log x
  | O.OMathLog, _         -> failwith "Wrong argument number"
  | O.OMathPow, [x; y]    -> math_pow x y
  | O.OMathPow, _         -> failwith "Wrong argument number"
  | O.OMathSin, [x]       -> math_sin x
  | O.OMathSin, _         -> failwith "Wrong argument number"
  | O.OObjCanDelete, [x; y] -> obj_can_delete x y
  | O.OObjCanDelete, _    -> failwith "Wrong argument number"
  | O.OPrimToBool, [x]    -> prim_to_bool x
  | O.OPrimToBool, _      -> failwith "Wrong argument number"
  | O.OPrimToNum, [x]     -> prim_to_num x
  | O.OPrimToNum, _       -> failwith "Wrong argument number"
  | O.OPrimToStr, [x]     -> prim_to_string x
  | O.OPrimToStr, _       -> failwith "Wrong argument number"
  | O.OPrint, [x]         -> print_string x
  | O.OPrint, _           -> failwith "Wrong argument number"
  | O.OStrContains, [x; y]-> str_contains x y
  | O.OStrContains, _     -> failwith "Wrong argument number"
  | O.OStrLen, [x]        -> str_len x
  | O.OStrLen, _          -> failwith "Wrong argument number"
  | O.OStrStartsWith, [x; y] -> str_startswith x y
  | O.OStrStartsWith, _   -> failwith "Wrong argument number"
  | O.OSurfaceTypeof, [x] -> surface_typeof x
  | O.OSurfaceTypeof, _   -> failwith "Wrong argument number"
  | O.OTypeof, [x]        -> typeof x
  | O.OTypeof, _          -> failwith "Wrong argument number"
  | O.ORegExpMatch, [x; y] -> regexp_match x y
  | O.ORegExpMatch, _     -> failwith "Wrong argument number"
  | O.OStrSplitRegExp, [x; y] -> str_split_regexp x y
  | O.OStrSplitRegExp, _  -> failwith "Wrong argument number"
  | O.OStrSplitStrExp, [x; y] -> str_split_str_exp x y
  | O.OStrSplitStrExp, _  -> failwith "Wrong argument number"
  | O.OObjIterHasNext, [x; y] -> obj_iter_has_next x y
  | O.OObjIterHasNext, _  -> failwith "Wrong argument number"
  | O.OObjIterKey, [x; y] -> obj_iter_key x y
  | O.OObjIterKey, _      -> failwith "Wrong argument number"
  | O.OObjIterNext, [x; y]-> obj_iter_next x y
  | O.OObjIterNext, _     -> failwith "Wrong argument number"
  | O.ORegExpQuote, _     -> failwith "Not implemented"

let abs_set_rec v1 v2 v3 = 
  let pairs = match v1.objs with None -> [] | Some o -> v2.strings |> Seq.map (fun s -> o, s) |> List.ofSeq
  let vals = pairs |> Seq.map (fun (o, f) -> o.SetField f v3) |> List.ofSeq
  vals |> fold_option (+) |> ValAt.from_object
let abs_del_rec (v1 : ValAt) (v2 : ValAt) resolver = 
  v1
let abs_get_rec (v1 : ValAt) (v2 : ValAt) resolver = 
  let labels = 
    [
      match v1.objs with
      | Some o -> 
        for f in v2.strings do
          yield o.GetField f
      | _ -> ()
    ]
  let res = labels |> Seq.fold (+) Set.empty |> Set.map (fun l -> resolver <| C l) |> Set.fold (+) (ValAt.from_undefined ())
//  if res.ChoicesCount > 0 then
//    do Utils.Log.alert "%s" res.pretty.pretty
  res
module LambdaJS.AbstractTypes

open Syntax
open Text.PrettyPrint
open Text.PrettySeqs
open Utils.Collections

let record_dim = 1

let set x = Set.ofList x
let sset x = set [x]
let bottom = Set.empty

type BoolAt = 
  | TrueAt
  | FalseAt 
  member self.pretty = 
    text <| sprintf "%A" self
  static member from_bool b  = 
    if b then TrueAt 
    else FalseAt
  static member s_from_bool b = 
    sset <| BoolAt.from_bool b
  static member STrueAt = sset TrueAt
  static member SFalseAt = sset FalseAt
  static member Top = set [TrueAt; FalseAt]
  static member Bottom : Set<BoolAt> = Set.empty
  static member (&&^) (x, y) = 
    match x, y with
    | TrueAt, TrueAt -> BoolAt.STrueAt
    | _ -> BoolAt.SFalseAt
  static member (||^) (x, y) = 
    match x, y with
    | FalseAt, FalseAt -> BoolAt.SFalseAt
    | _ -> BoolAt.STrueAt
  static member (!^) x = 
    match x with
    | TrueAt -> BoolAt.FalseAt
    | FalseAt -> BoolAt.TrueAt
  static member (=^) (x, y) = 
    let v = x = y in sset <| BoolAt.from_bool v
  static member (<>^) (x, y) = 
    let v = x <> y in sset <| BoolAt.from_bool v

and NumAt = 
  | Plus
  | Zero
  | Minus
  member self.pretty = 
    match self with
    | Plus -> text "(+)"
    | Zero -> text "(0)"
    | Minus -> text "(-)"
  static member from_num n = 
    match n with
    | _ when n = 0. -> Zero
    | _ when n > 0. -> Plus
    | _ when n < 0. -> Minus
  static member from_num_s n = 
    match n with
    | _ when n = 0. -> sset Zero
    | _ when n > 0. -> sset Plus
    | _ when n < 0. -> sset Minus
    | _             -> bottom //set [Zero; Plus; Minus]
  static member (+) (x, y) = 
    match x, y with
    | Zero, n
    | n, Zero -> sset n
    | Plus, Plus -> sset Plus
    | Minus, Minus -> sset Minus
    | Plus, Minus
    | Minus, Plus -> set [Plus; Minus; Zero]
  static member (*) (x, y) = 
    match x, y with
    | Zero, _
    | _, Zero -> sset Zero
    | Plus, Plus 
    | Minus, Minus -> sset Plus
    | Plus, Minus
    | Minus, Plus -> sset Minus
  static member (/) (x, y) = 
    match x, y with
    | _, Zero -> bottom
    | Plus, Plus 
    | Minus, Minus -> sset Plus
    | Plus, Minus
    | Minus, Plus -> sset Minus
    | Zero, _ -> sset Zero
  static member inv x      = 
    match x with
    | Zero -> Zero
    | Plus -> Minus
    | Minus -> Plus
  static member (-) (x, y) = 
    x + (NumAt.inv y)
  static member (%) (x, y) = 
    match x, y with
    | _, Zero -> bottom
    | Zero, _ -> sset Zero
    | Plus, _ -> set [Plus; Zero]
    | Minus, _ -> set [Minus; Zero]
  static member (=^) (x, y) = 
    match x, y with
    | Zero, Zero -> BoolAt.STrueAt
    | Minus, Minus
    | Plus, Plus -> BoolAt.Top
    | _, _ -> BoolAt.SFalseAt
  static member (<>^) (x, y) = 
    match x, y with
    | Zero, Zero -> BoolAt.SFalseAt
    | Minus, Minus
    | Plus, Plus -> BoolAt.Top
    | _, _ -> BoolAt.STrueAt
  static member (>^)  (x : NumAt, y) = 
    match x, y with
    | Plus, Plus 
    | Minus, Minus -> BoolAt.Top
    | Zero, Zero -> BoolAt.SFalseAt
    | Minus, _ -> BoolAt.SFalseAt
    | Zero, Plus -> BoolAt.SFalseAt
    | _ -> BoolAt.STrueAt
  static member (<^)  (x : NumAt, y) = 
    match x, y with
    | Plus, Plus 
    | Minus, Minus -> BoolAt.Top
    | Zero, Zero -> BoolAt.SFalseAt
    | Minus, _ -> BoolAt.STrueAt
    | Zero, Plus -> BoolAt.STrueAt
    | _ -> BoolAt.SFalseAt
  static member (<=^) (x : NumAt, y) = 
    match x, y with
    | Plus, Plus 
    | Minus, Minus -> BoolAt.Top
    | Zero, Zero -> BoolAt.STrueAt
    | Minus, _ -> BoolAt.STrueAt
    | Zero, Plus -> BoolAt.STrueAt
    | _ -> BoolAt.SFalseAt    
  static member (>=^) (x : NumAt, y) =  
    match x, y with
    | Plus, Plus 
    | Minus, Minus -> BoolAt.Top
    | Zero, Zero -> BoolAt.STrueAt
    | Minus, _ -> BoolAt.SFalseAt
    | Zero, Plus -> BoolAt.SFalseAt
    | _ -> BoolAt.STrueAt
  static member Top = set [Plus; Minus; Zero]
  static member Bottom : Set<NumAt> = Set.empty

(*and StringAt = S of string 
    with
      member self.pretty = 
        let (S s) = self
        quotes <| text (sprintf "%s*" s)
      member self.CLength = 
        let (S s) = self
        s.Length
      member self.Substring (startIndex, length) = 
        let (S s) = self
        let l = s.Length
        if startIndex > l then
          S ""
        elif startIndex <= l && length > l then
          S <| s.Substring (startIndex, l - startIndex)
        else
          S <| s.Substring(startIndex, length) 
      member self.Contains c = 
        let (S s) = self
        if s.Contains c then
          BoolAt.STrueAt
        else
          BoolAt.Top
      static member from_string s = S s
      static member (+) (x, y) = 
        x
      static member (=^) (S x, S y) = 
        let m_dim = min (x.Length) (y.Length)
        let x = x.Substring(0, m_dim)
        let y = y.Substring(0, m_dim)
        if x <> y then
          BoolAt.STrueAt
        else
          BoolAt.Top*)

and StringAt = 
  | Exact of string
  | Pref  of string
  override this.ToString() = let c : Doc = this.pretty in c.pretty
  member self.pretty      = 
      match self with
      | Exact s -> quotes <| text s
      | Pref s ->
        quotes <| text (sprintf "%s*" s)
  member self.Length     = 
    match self with
    | Exact s -> NumAt.from_num_s <| float s.Length
    | Pref s -> set [Plus; Zero]
  member self.Substring (startIndex, length) = 
    match self with
    | Exact x -> 
      let l = x.Length
      if startIndex > l then
        failwith "index out of bound since not a prefix"
      elif startIndex <= l && length > l then
        failwith "length greater than string length on a non prefix" //S <| s.Substring (startIndex, l - startIndex)
      else
        Exact <| x.Substring(startIndex, length) 
    | Pref x ->
      let l = x.Length
      if startIndex > l then
        StringAt.Top
      elif startIndex <= l && length > l then
        Pref <| x.Substring (startIndex, l - startIndex)
      else
        Exact <| x.Substring(startIndex, length) 
  member self.Contains c  = 
    //WARNING da controllare come fa Giulia
    match self, c with
    | Exact s, Exact c -> BoolAt.s_from_bool <| s.Contains c
    | Exact s, Pref c  -> if s.Contains c then BoolAt.Top else BoolAt.SFalseAt
    | Pref s, Exact c  -> 
      BoolAt.Top 
      //if c.StartsWith s then BoolAt.Top else BoolAt.SFalseAt
    | Pref s, Pref c   -> 
        BoolAt.Top
  static member from_e_string s = Exact s
  static member from_string_s s = sset <| Exact s
  //static member from_p_string s = Pref s
  static member Top = Pref ""
  static member (+) (x, y : StringAt)   = 
    match x with
    | Exact x 
    | Pref x -> Pref x
  static member (=^) (x, y)             = 
    match x, y with
    | Exact x, Exact y  -> 
      if x = y then
        BoolAt.STrueAt
      else
        BoolAt.SFalseAt
    | Exact x, Pref y   -> 
      if x.StartsWith y then
        BoolAt.Top
      else
        BoolAt.SFalseAt
    | Pref y, Exact x   -> 
      if y.StartsWith x then
        BoolAt.Top
      else
        BoolAt.SFalseAt
    | Pref x, Pref y    -> 
      if x.StartsWith y || y.StartsWith x then
        BoolAt.Top
      else
        BoolAt.SFalseAt
  static member (<>^) (x : StringAt, y) = 
      let r = x =^ y
      seq{ for c in r do yield !^c } |> Set.ofSeq
  static member (=?) (x : StringAt, y)  = 
    x =^ y |> Set.contains TrueAt
  static member (<^) (x : StringAt, y)  = 
    match x, y with
    | Exact x, Exact y -> x < y |> BoolAt.from_bool |> sset
    | _ -> 
      //WARNING metto sempre top perche' devo controllare le disuguaglianze su stringhe
      BoolAt.Top
  (*let m_dim = min (x.Length) (y.Length) 
      let x = x.Substring(0, m_dim)
      let y = y.Substring(0, m_dim)
      if x <> y then
        BoolAt.STrueAt
      else
        BoolAt.Top*)
      
and Ref = Syntax.Ref

and Lambda = Syntax.Lambda

and ObjAt = 
  | O of Map<StringAt, Set<Syntax.Label>>
  member self.Value   = 
    let (O v) = self in v
  member self.pretty  = 
    self.Value |> Text.PrettySeqs.pretty_map prettifier (pretty_seq t_int ";") ";"
  member self.GetKeys = 
    self.Value |> Seq.map (fun b -> b.Key) |> List.ofSeq
  member self.IsExact = 
    self.GetKeys |> Seq.forall (fun k -> match k with StringAt.Exact _ -> true | Pref _ -> false )
  member self.ExactPart = 
    self.Value |> Map.filter (fun k _ -> match k with StringAt.Exact _ -> true | _ -> false)
  member self.StarPart  =  
    match self.Value |> Map.tryFind StringAt.Top with None -> Set.empty | Some s -> s
  member self.GetField f = 
    // WARNING: da gestire le proto chain
    self.Value |> Map.filter (fun k _ -> k =? f) |> Seq.fold (fun s p -> s + p.Value ) Set.empty
  member self.SetField f (l : Label) = 
    let l = sset l
    match f with
    | StringAt.Exact _ -> 
      Mapx.add_expand (+) f l self.Value |> O
    | Pref _  -> 
      Mapx.add_expand (+) StringAt.Top l self.Value |> O
  member self.HasOwnProp f = 
    if self.Value |> Map.tryPick (fun k _ -> if k =? f then Some () else None) <> None then BoolAt.Top else BoolAt.SFalseAt
  member self.IsSubObjOf (o : ObjAt) = 
    Mapx.map_is_subset_of Set.isSubset self.ExactPart o.ExactPart &&
    Set.isSubset self.StarPart  o.StarPart
  member self.union (o : ObjAt) = 
    if self = o then
      self
    elif self <== o then
      o
    elif o <== self then
      self
    else
      let star_p = self.StarPart + o.StarPart
      let exact_p = Mapx.union (+) self.ExactPart o.ExactPart
      if star_p <> Set.empty then
        Mapx.bind StringAt.Top star_p exact_p |> O
      else 
        exact_p |> O
  static member (+)   (x : ObjAt, y : ObjAt) = 
    x.union y
  static member (<==) (x : ObjAt, y : ObjAt) = 
    x.IsSubObjOf y  
  static member (>==) (x : ObjAt, y : ObjAt) = 
    y.IsSubObjOf x
  static member from_binds (binds : (string * Syntax.Label) seq) = 
    let binds = binds |> Seq.map (fun (s, v) -> StringAt.from_e_string s, sset v)
    Seq.fold (fun s (k, v) -> Mapx.add_expand (+) k v s) Map.empty binds |> O
  static member Bottom = O <| Map.empty

and RObjAt = 
  | RO of Map<StringAt, ValAt>
  member self.Value = let (RO o) = self in o
  static member from_ObjAt (obj : ObjAt) (resolver : CElem -> ValAt) = 
    obj.Value |> Map.map (fun k v -> v |> Set.fold (fun s v -> s + resolver (Cache v)) ValAt.empty) |> RO
  
(*
  and ObjAt = 
    | Exact of Map<StringAt, Set<Syntax.Label>> * int
    | Top   of Map<StringAt, Set<Syntax.Label>> * Set<Syntax.Label>
    member self.pretty = 
      match self with
      | Exact (o, _) ->
        o |> Text.PrettySeqs.pretty_map prettifier (pretty_seq t_int ";") ";"
      | Top (o, clash) -> 
        text "Prefix" <+> Text.PrettySeqs.pretty_map prettifier (pretty_seq t_int ";") ";" (o |> Map.add StringAt.Top clash)
    member self.GetKeys = 
      match self with
      | Exact (binds, _) -> 
        binds |> Seq.map (fun b -> b.Key) |> List.ofSeq
      | Top (binds, rest) -> 
        StringAt.Top::(binds |> Seq.map (fun b -> b.Key) |> List.ofSeq)
    member private self.GetPlainMap = 
      match self with
      | Exact (binds, _) -> 
        binds
      | Top (binds, rest) -> 
        Map.add StringAt.Top rest binds
    member self.GetField f = 
      // WARNING: da gestire le proto chain
      let try_find_set f m = match Map.tryFind f m with None -> Set.empty | Some s -> s
      match self, f with
      | Exact (binds, _), StringAt.Exact s -> 
        if binds |> Map.containsKey f then
          binds.[f], false
        else 
          Set.empty, true
      | Exact (binds, _), StringAt.Pref p  -> 
        seq{
          for b in binds do
            if b.Key =? f then
              yield b.Value
        } |> Seq.fold (+) Set.empty, true
      | Top (binds, rest), StringAt.Exact s ->
        try_find_set f binds + rest, false
      | Top (binds, rest), StringAt.Pref s ->
        seq{
          for b in binds do
            if b.Key =? f then
              yield b.Value
        } |> Seq.fold (+) Set.empty |> (+) rest, true
    member self.SetField f (l : Label)   = 
      let add_expand (f : 'a) (value : 'b) (s : Map<'a, Set<'b>>) = 
        if s |> Map.containsKey f then
          Map.map (fun k v -> if k = f then v |> Set.add value else v) s
        else 
          Map.add f (sset value) s
      match self, f with
      | Exact (binds, times), StringAt.Exact _ -> 
  //      if times < record_dim then
        let res = binds |> add_expand f l |> fun binds -> Exact (binds, times + 1) |> sset
        res
  //      else 
  //        Top (binds, sset l) |> sset
      | Top (binds, rest), StringAt.Exact _ -> 
        let res = [
                    (binds |> add_expand f l, rest) |> Top;
                    (binds, Set.add l rest) |> Top
                  ] |> set
        res
      | Top (binds, rest), StringAt.Pref _ -> 
        let res = seq {
                        yield (binds, Set.add l rest) |> Top
                        for b in binds do
                          if f =? b.Key then
                            yield (binds |> add_expand b.Key l, rest) |> Top
                      } |> Set.ofSeq
        res
      | Exact (binds, _), Pref _ ->
        let res =  
          seq {
            yield (binds, sset l) |> Top
            for b in binds do
              if f =? b.Key then
                yield (binds |> add_expand b.Key l, Set.empty) |> Top
          } |> Set.ofSeq
        res
    member self.IsSuperObjOf (o : ObjAt) = 
      let map_is_superset_of (m1 : Map<_, Set<'b>>) (m2 : Map<_, Set<'b>>) = 
        let k1 = m1 |> Seq.map (fun k -> k.Key) |> Set.ofSeq
        let k2 = m2 |> Seq.map (fun k -> k.Key) |> Set.ofSeq
        if Set.isSuperset k1 k2 then
          m2 |> Map.forall (fun k v -> Set.isSuperset m1.[k] v)
        else
          false
      let b1 = self.GetPlainMap
      let b2 = o.GetPlainMap
      map_is_superset_of b1 b2
    member self.union (o : ObjAt) = 
      if self.IsSuperObjOf o then
        self
      elif o.IsSuperObjOf self then
        o
      else
        let join_map (m1 : Map<_, Set<_>>) (m2 : Map<_, Set<_>>) = 
          m1 |> Map.fold 
                  (
                    fun acc k v -> 
                      acc |> Map.add k (if acc.ContainsKey k then acc.[k] + v else v)
                  ) m2 
        match self, o with
        | Exact (b1, _), Exact (b2, _) -> 
          Exact <| (join_map b1 b2, 0)
        | _ -> 
          let m1 = self.GetPlainMap
          let m2 = o.GetPlainMap
          let res = join_map m1 m2
          let exact = res |> Map.remove StringAt.Top
          let rest  = res.[StringAt.Top]
          Top (exact, rest)
    static member (+) (x : ObjAt, y : ObjAt) = x.union y
    static member from_binds binds = 
      let rec add_bind acc (binds : (string * Syntax.Label) list) = 
        match binds with
        | [] -> acc
        | (id, elem)::xs -> 
          let id = StringAt.from_e_string id
          let n_acc = 
            if Map.containsKey id acc then
              let n_val = Map.find id acc |> Set.add elem
              Map.add id n_val acc
            else
              Map.add id (sset elem) acc
          in add_bind n_acc xs
      let cnt : Map<StringAt, Set<Syntax.Label>> = add_bind Map.empty binds
      in ObjAt.Exact (cnt, 0)*)

and ValAt = 
  { 
    conds     : Set<BoolAt>
    nums      : Set<NumAt>
    strings   : Set<StringAt>
    refs      : Set<Ref>
    lambdas   : Set<Lambda>
    objs      : ObjAt option
    //r_objs    : RObjAt option
    Null      : bool
    Undefined : bool
  }
  member self.pretty = 
    let parts = 
      [
        if not <| Set.isEmpty self.conds then 
          yield text "conds:" <+> braces (xhcat "," (List.ofSeq <| Seq.map prettifier self.conds))
        if not <| Set.isEmpty self.nums then 
          yield text "nums:" <+> braces (xhcat "," (List.ofSeq <| Seq.map prettifier self.nums))
        if not <| Set.isEmpty self.strings then 
          yield text "strings:" <+> braces (xhcat "," (List.ofSeq <| Seq.map prettifier self.strings))
        if not <| Set.isEmpty self.refs then 
          yield text "refs:" <+> braces (xhcat "," (List.ofSeq <| Seq.map t_int self.refs))
        if not <| Set.isEmpty self.lambdas then 
          yield text "lambdas:" <+> braces (xhcat "," (List.ofSeq <| Seq.map t_int self.lambdas))
        if self.objs <> None then 
          yield text "objs:" <+> self.objs.Value.pretty
        if self.Null then
          yield text "Null:" <+> t_bool self.Null
        if self.Undefined then
          yield text "Undefined:" <+> t_bool self.Undefined
      ]
    if parts |> List.length > 0 then
      xhcat ";" parts
    else
      text "{ }"
  member self.diagnostic = 
    let parts = 
      [
        if not <| Set.isEmpty self.conds then 
          yield text "conds:" <+> t_int self.conds.Count
        if not <| Set.isEmpty self.nums then 
          yield text "nums:" <+> t_int self.nums.Count
        if not <| Set.isEmpty self.strings then 
          yield text "strings:" <+> t_int self.strings.Count
        if not <| Set.isEmpty self.refs then 
          yield text "refs:" <+> t_int self.refs.Count
        if not <| Set.isEmpty self.lambdas then 
          yield text "lambdas:" <+> t_int self.lambdas.Count
        if self.objs <> None then 
          yield text "obj with field:" <+> t_int self.objs.Value.Value.Count
        if self.Null then
          yield text "Null:" <+> t_bool self.Null
        if self.Undefined then
          yield text "Undefined:" <+> t_bool self.Undefined
      ]
    if parts |> List.length > 0 then
      xhcat ";" parts
    else
      text "{ }"
  override self.ToString() = self.pretty.pretty //self.diagnostic.pretty 
  member self.ChoicesCount = 
    let is_null_count  = if self.Null      then 1 else 0
    let is_undef_count = if self.Undefined then 1 else 0
    let has_obj        = if self.objs <> None then 1 else 0
    Set.count self.conds    +
    Set.count self.nums     +
    Set.count self.nums     +
    Set.count self.strings  +
    Set.count self.refs     +
    Set.count self.lambdas  +
    has_obj                 +
    is_null_count           +
    is_undef_count
  member self.Contains (v : ValAt) = 
    let is_null_cont  = not v.Null       || (self.Null       && v.Null)
    let is_undef_cont = not v.Undefined  || (self.Undefined  && v.Undefined)  
    let is_obj_cont = 
      match self.objs, v.objs with
      | None, None -> true
      | Some _, None -> true
      | None, Some _ -> false
      | Some x, Some y -> y <== x
    v.conds.IsSubsetOf    self.conds    &&
    v.nums.IsSubsetOf     self.nums     &&
    v.nums.IsSubsetOf     self.nums     &&
    v.strings.IsSubsetOf  self.strings  &&
    v.refs.IsSubsetOf     self.refs     &&
    v.lambdas.IsSubsetOf  self.lambdas  &&
    is_obj_cont                         &&
    is_null_cont                        &&
    is_undef_cont
  member self.IsPrim = 
    [
      if self.conds.Count > 0 then
        yield TrueAt
      if self.nums.Count > 0 then
        yield TrueAt
      if self.strings.Count > 0 then
        yield TrueAt
      if self.Null then
        yield TrueAt
      if self.Undefined then
        yield TrueAt
      if self.refs.Count > 0 then
        yield FalseAt
      if self.lambdas.Count > 0 then
        yield FalseAt
      if self.objs <> None then
        yield FalseAt     
    ] |> Set.ofSeq
//  member self.ResolveObj (resolver : CElem -> ValAt) = 
//    match self.objs with
//    | None -> self
//    | Some o -> { self with r_objs = Some <|  RObjAt.from_ObjAt o resolver }
  static member empty = { conds = Set.empty; nums = Set.empty; strings = Set.empty; refs = Set.empty; lambdas = Set.empty; objs = None; (*r_objs = None;*) Null = false; Undefined = false }
  static member from_string s     = 
    { ValAt.empty with strings = StringAt.from_string_s s }
  static member from_string_at_set ss = 
    { ValAt.empty with strings = ss }
  static member from_bool b       = 
    { ValAt.empty with conds = BoolAt.s_from_bool b }
  static member from_bool_at_set bs  = 
    { ValAt.empty with conds = bs }
  static member from_num n        = 
    { ValAt.empty with nums = NumAt.from_num_s n }
  static member from_num_at_set ns   = 
    { ValAt.empty with nums = ns }
  static member from_ref r        = 
    { ValAt.empty with refs = sset r }
  static member from_lambda l     = 
    { ValAt.empty with lambdas = sset l }
  static member from_null ()      = 
    { ValAt.empty with Null = true }
  static member from_undefined () = 
    { ValAt.empty with Undefined = true }
  static member from_binds binds  = 
    { ValAt.empty with objs = Some <| ObjAt.from_binds binds}
  static member from_object obj   = 
    { ValAt.empty with objs = obj}
//  static member from_objects objs = 
//    { ValAt.empty with objs = set <| objs}
  static member (+) (x : ValAt, y : ValAt) : ValAt = 
    let res_obj = 
      match x.objs, y.objs with
      | None, None      -> None
      | Some x, None    -> Some x
      | None, Some y    -> Some y
      | Some x, Some y  -> Some (x + y)
    { 
      conds     = x.conds     +  y.conds
      nums      = x.nums      +  y.nums
      strings   = x.strings   +  y.strings
      refs      = x.refs      +  y.refs
      lambdas   = x.lambdas   +  y.lambdas
      objs      = res_obj
      Null      = x.Null      || y.Null
      Undefined = x.Undefined || y.Undefined
    }
  static member union x y = x + y
  static member (===^) (x : ValAt, y : ValAt) = 
    //WARNING using top... ...se intersect vuota false else top
    BoolAt.Top |> ValAt.from_bool_at_set
  static member (==^) (x : ValAt, y : ValAt)  = 
    //WARNING using top...
    BoolAt.Top |> ValAt.from_bool_at_set

//and CacheEl = Syntax.label

and CElem = 
  | Cache of Syntax.Label
  | Gamma of Syntax.id
  | Mu    of Permissions.PermissionBox * Ref
  member self.pretty = 
    match self with
    | Cache l -> text (sprintf "C(%d)" l)
    | Gamma v -> text (sprintf "R(%s)" v)
    | Mu (p, r) -> text "Mu" <|> parens (p.pretty <|> comma <|> t_gen r)
  override self.ToString() = self.pretty.pretty

and PElem = 
  | P of Syntax.Label
  member self.pretty = 
    let (P l) = self
    text (sprintf "P(%d)" l)

and RecordOp = 
  | GetFieldAt of CElem * CElem
  | SetFieldAt of CElem * CElem * CElem
  | DelFieldAt of CElem * CElem
  member self.pretty = 
    match self with
    | GetFieldAt (el1, el2) -> 
      text "GetFieldAt" <|> parens(xhcat ";" [el1.pretty; el2.pretty])
    | SetFieldAt (el1, el2, el3) -> 
      text "SetFieldAt" <|> parens(xhcat ";" [el1.pretty; el2.pretty; el3.pretty])
    | DelFieldAt (el1, el2) -> 
      text "DelFieldAt" <|> parens(xhcat ";" [el1.pretty; el2.pretty])
    
and Constraint = 
  | TIncl       of ValAt * CElem
  | Incl        of CElem * CElem
  | PIncl       of PElem * PElem
  | OpIncl      of Syntax.Op * CElem list * CElem
  | RecordIncl  of RecordOp * CElem
  | Impl        of Constraint * Constraint
  member self.pretty = 
    match self with
    | TIncl (t, e)  ->
      braces (t.pretty) <+> text "<=" <+> e.pretty
    | Incl (e1, e2) -> 
      e1.pretty <+> text "<=" <+> e2.pretty
    | PIncl (p1, p2) -> 
      p1.pretty <+> text "[=" <+> p2.pretty
    | Impl (c1, c2) -> 
      c1.pretty <+> text "=>" <+> c2.pretty
    | RecordIncl (op, c) ->
      parens <| (op.pretty <+> text "<=" <+> c.pretty)
    | OpIncl (op, es, e1) -> 
       text op.pretty <+> Text.PrettySeqs.pretty_list prettifier ";" es <+> text "<=" <+> e1.pretty
  override self.ToString () = self.pretty.pretty

//let pretty_set (s : Set<Constraint>) = 
//  let rows = s |> List.ofSeq |> List.map (fun c -> c.pretty)
//  text "conditions : " <+> braces (xvcat " /\\" rows)
  
let (<--) t1 e2 = TIncl (t1, e2)
let (<==) e1 e2 = Incl (e1, e2)
let (==>) c1 c2 = Impl (c1, c2)
let (|==) p1 p2 = PIncl (p1, p2)

let Mu = CElem.Mu
let C  = CElem.Cache
let R  = CElem.Gamma
let P  = PElem.P

let true_at = ValAt.from_bool true
let false_at = ValAt.from_bool false

let get_field_at = "get_field"
let set_field_at = "set_field"
let del_field_at = "del_field"
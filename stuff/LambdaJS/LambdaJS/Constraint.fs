module LambdaJS.ToBeRemoved
//
//open LambdaJS.Syntax
//open LambdaJS.AbstractTypes
//open Text.PrettyPrint
//
//type CElem = 
//  | Cache of Syntax.label
//  | Gamma of Syntax.id
//  | Mu    of Permissions.Permission * Ref
//  member self.pretty = 
//    match self with
//    | Cache l -> text (sprintf "C(%s)" l)
//    | Gamma v -> text (sprintf "R(%s)" v)
//    | Mu (p, r) -> text "Mu" <|> parens (p.pretty_doc <|> comma <|> t_gen r)
//
//and PElem = 
//  | P of Syntax.label
//  member self.pretty = 
//    let (P l) = self
//    text (sprintf "P(%s)" l)
//
//and Constraint = 
//  | TIncl     of ValAt * CElem
//  | Incl      of CElem * CElem
//  | PIncl     of PElem * PElem
//  | OpIncl    of Syntax.Op * CElem list * CElem
//  | Impl      of Constraint * Constraint
//  member self.pretty = 
//    match self with
//    | TIncl (t, e)  ->
//      braces (t.pretty) <+> text "<=" <+> e.pretty
//    | Incl (e1, e2) -> 
//      e1.pretty <+> text "<=" <+> e2.pretty
//    | PIncl (p1, p2) -> 
//      p1.pretty <+> text "<=" <+> p2.pretty
//    | Impl (c1, c2) -> 
//      c1.pretty <+> text "=>" <+> c2.pretty
//    | OpIncl (op, es, e1) -> 
//       text op.pretty <+> Text.PrettySeqs.pretty_list prettifier ";" es <+> text "<=" <+> e1.pretty
//  override self.ToString () = self.pretty.pretty
//
////let pretty_set (s : Set<Constraint>) = 
////  let rows = s |> List.ofSeq |> List.map (fun c -> c.pretty)
////  text "conditions : " <+> braces (xvcat " /\\" rows)
//  
//let (<--) t1 e2 = TIncl (t1, e2)
//let (<==) e1 e2 = Incl (e1, e2)
//let (==>) c1 c2 = Impl (c1, c2)
//
//

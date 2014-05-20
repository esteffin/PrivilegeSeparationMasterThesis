module LambdaJS.Syntax

open Utils.Prelude
open Utils.PrettyPrint
open Utils.ParsingUtils
open Text.PrettyPrint

type id = string
//type label = id

/// For alpha-renaming ids. First element is the ref of the actual name, the Second is the original name
type re_id = id * id

let (!.) (id, _) = id

type Ref    = int
type Lambda = int
type Label  = int

type Tag = 
  | Ref of Ref
  | Lambda of Lambda

type [<AbstractClass>] Node<'tag> (loc : location, ?tag : 'tag) = 
  member self.IsTagged with get () = is_some self.Tag
  member val Tag = tag with get, set
  member val Loc = loc
  abstract pretty : string 
  abstract pretty_localized : string
  default self.pretty_localized = 
    sprintf "[@%O] %s" self.Loc (soprintf self.Tag)
  override self.ToString() = self.pretty

type locatable<'a, 'tag> (value : 'a, loc : location, ?tag : 'tag) =
    inherit Node<'tag> (?tag = tag, loc = loc)
    member val value = value with get, set
    override self.pretty = sprintf "%O" self.value
    override self.pretty_localized = sprintf "%O %s" self.value base.pretty_localized

//type Op = locatable<OOp, obj>

and Op = 
  | ONumPlus
  | OStrPlus
  | OMul | ODiv | OMod | OSub
  | OLt  | OStrLt
  | OBAnd | OBOr | OBXOr | OBNot
  | OLShift | ORShift | OZfRShift
  | OStrictEq
  | OAbstractEq
  | OTypeof
  | OSurfaceTypeof
  | OPrimToNum
  | OPrimToStr
  | OPrimToBool
  | OIsPrim
  | OHasOwnProp
  | OToInteger | OToInt32 | OToUInt32
  | OPrint                                                            // ^for Rhino
  | OStrContains | OStrSplitRegExp | OStrSplitStrExp                  // ^for Regexes
  | OStrStartsWith                                                    // ^for forin
  | OStrLen
  | OObjIterHasNext | OObjIterNext | OObjIterKey                      // ^more forin
  | OObjCanDelete
  | OMathExp | OMathLog | OMathCos | OMathSin | OMathAbs | OMathPow
  | ORegExpMatch | ORegExpQuote
  with
    member self.pretty 
      with get () = 
        match self with
        | ONumPlus -> "+"
        | OStrPlus -> "string-+"
        | OMul -> "*"
        | ODiv -> "/"
        | OMod -> "%"
        | OSub -> "-"
        | OLt  -> "<"
        | OStrLt -> "string-<"
        | OStrictEq -> "==="
        | OAbstractEq -> "=="
        | OTypeof -> "typeof"
        | OSurfaceTypeof -> "surface-typeof"
        | OPrimToNum -> "prim->number"
        | OPrimToStr -> "prim->string"
        | OPrimToBool -> "prim->bool"
        | OIsPrim -> "prim?"
        | OToInteger -> "to-integer"
        | OToInt32 -> "to-int-32"
        | OToUInt32 -> "to-uint-32"
        | OBAnd -> "&"
        | OBOr -> "\\|"
        | OBXOr -> "^"
        | OBNot -> "~"
        | OLShift -> "<<"
        | ORShift -> ">>"
        | OZfRShift -> ">>>"
        | OHasOwnProp -> "has-own-prop?"
        | OPrint -> "print-string"
        | OStrContains -> "str-contains"
        | OObjIterHasNext -> "obj-iterate-has-next?"
        | OObjIterNext -> "obj-iterate-next"
        | OObjIterKey -> "obj-iterate-key"
        | OStrStartsWith -> "str-startswith"
        | OStrLen -> "str-length"
        | OStrSplitRegExp -> "str-split-regexp"
        | OStrSplitStrExp -> "str-split-strexp"
        | OObjCanDelete -> "obj-can-delete?"
        | OMathExp -> "math-exp"
        | OMathLog -> "math-log"
        | OMathCos -> "math-cos"
        | OMathSin -> "math-sin"
        | OMathAbs -> "math-abs"
        | OMathPow -> "math-pow"
        | ORegExpQuote -> "regexp-quote"
        | ORegExpMatch -> "regexp-match"
    override self.ToString() = self.pretty

type Expr (value : EExpr, loc : location, ?tag : Tag) = 
  inherit locatable<EExpr, Tag> (value = value, loc = loc, ?tag = tag)
  member val Label : Label = -1 with get, set
  member self.pretty_doc = brackets (self.value.pretty_doc <|> comma <+> (t_int self.Label))
  
and (*[<StructuralComparison; StructuralEquality>]*) EExpr = 
  | ENumber       of double
  | EString       of string
  | EBool         of bool
  | EUndefined
  | ENull
  | ELambda       of re_id list * Expr
  | EObject       of (string * Expr) list
  | EId           of re_id
  | EOp           of Op * Expr list
  | EApp          of Expr * Expr list
  | ELet          of (re_id * Expr) list * Expr
  | ESetRef       of Expr * Expr
  | ERef          of Expr
  | EDeref        of Expr
  | EGetField     of Expr * Expr
  | EUpdateField  of Expr * Expr * Expr
  | EDeleteField  of Expr * Expr
  | ESeq          of Expr * Expr
  | EIf           of Expr * Expr * Expr
  | EWhile        of Expr * Expr
  | ELabel        of re_id * Expr
  | EBreak        of re_id * Expr
  | EThrow        of Expr
  | ECatch        of Expr * Expr
  | EFinally      of Expr * Expr
  // |They use HOAS when possible so that they don't mess up bindings.  When
  // pretty-printing, they unravel these to use conventional bindings.
  //| ELet1         of Expr * (id -> Expr)
  //| ELet2         of Expr * Expr * (id -> id -> Expr)
  | EEval   // ^an expression that calls eval, or a related function.  If
            // EEval becomes the active expression, our model immediately aborts.
  with
    member self.pretty_doc 
      with get () = 
        let map = List.map
        match self with
        | ENumber n                 -> 
          text (sprintf "%f" n)
        | EString s                 -> 
          text (quot s)
        | EBool b when b            -> 
          text "#t"
        | EBool b                   -> 
          text "#f"
        | EUndefined                -> 
          text "undefined"
        | ENull                     -> 
          text "null"
        | ELambda (ids, e)          ->
          let d = e.pretty_doc 
          parens <| (text "lambda" <+> (parens (xhcat "" <| map (text << (!.)) ids)) .+. d)
        | EObject pairs             ->
          let prop (x, e : Expr) = 
            parens (text (quot x) <+> e.pretty_doc)
          let props = map prop pairs
          parens (text "object" <+> vcat props)
        | EId id                    -> 
          text !.id
        | EOp (op, exs)             -> 
          parens (text op.pretty <+> hsep (map (fun (ex : Expr) -> ex.pretty_doc) exs))
        | EApp (e1, acts)           ->
          let es = map (fun (e : Expr) -> e.pretty_doc) (e1::acts)
          parens <| hsep es
        | ELet (binds, e)           -> 
          let f (id, e' : Expr) = 
            parens <| (text !.id <+> e'.pretty_doc) 
          let binds = map f binds
          parens <| (text "let" .+. parens (vcat binds) .+. e.pretty_doc)
        | ESetRef (e1, e2)          -> 
          parens <| (text "set!" .+. e1.pretty_doc .+. e2.pretty_doc)
        | ERef e                    ->
          parens <| (text "alloc" <+> e.pretty_doc)
        | EDeref e                  ->
          parens <| (text "deref" <+> e.pretty_doc)
        | EGetField (e1, e2)        ->
          parens <| (text "get-field" .+. e1.pretty_doc .+. e2.pretty_doc)
        | EUpdateField (e1, e2, e3) -> 
          parens <| (text "update-field" <+> e1.pretty_doc .+. e2.pretty_doc .+. e3.pretty_doc)
        | EDeleteField (e1, e2)     -> 
          parens <| (text "delete-field" <+> e1.pretty_doc .+. e2.pretty_doc)
        | ESeq (e1, e2)             -> 
          parens <| (text "begin" .+. e1.pretty_doc .+. e2.pretty_doc)
        | EIf (c, thn, els)         ->
          parens <| (text "if" <+> c.pretty_doc .+. thn.pretty_doc .+. els.pretty_doc)
        | EWhile (c, body)          -> 
          parens <| (text "while" .+. c.pretty_doc .+. body.pretty_doc)
        | ELabel (lbl, e)           ->
          parens <| (text "label" <+> text !.lbl .+. e.pretty_doc)
        | EBreak (lbl, e)           ->
          parens <| (text "break" <+> text !.lbl .+. e.pretty_doc)
        | EThrow e                  -> 
          parens <| (text "throw" <+> e.pretty_doc)
        | ECatch (e1, e2)           ->
          parens <| (text "try-catch" .+. e1.pretty_doc .+. e2.pretty_doc)
        | EFinally (e1, e2)         ->
          parens <| (text "try-finally" .+. e1.pretty_doc .+. e2.pretty_doc)
        //| ELet1 (bind1, eFn Expr * (id -> Expr)
        //| ELet2         of Expr * Expr * (id -> id -> Expr)
        | EEval                     -> 
          text "eval-semantic-bomb"
    member self.pretty 
      with get () = 
        let parens = Utils.PrettyPrint.parens
        match self with
        | ENumber n                 -> 
          sprintf "%f" n
        | EString s                 -> 
          s
        | EBool b when b            -> 
          "True"
        | EBool b                   -> 
          "False"
        | EUndefined                -> 
          "undefined"
        | ENull                     -> 
          "null"
        | ELambda (ids, e)          -> 
          parens <| sprintf "lambda %s -> ..." (parens (sprintfs ids)) //e
        | EObject pairs             ->
          let prop (x, e) = 
            parens (sprintf "%s : %O" x e)
          parens <| sprintf "object : ..." //(vsprintfs (List.map prop pairs)))
        | EId id                    -> 
          !.id
        | EOp (op, exs)             -> 
          parens (sprintf "%O %s" op (sprintfs exs))
        | EApp (e1, acts)           ->
          parens <| sprintf "call: %O args..." e1 //(sprintfs (e1::acts))
        | ELet (binds, e)           -> 
          let f (x, e') = 
            parens (sprintf "%s = ..." !.x (*e'*))
          let binds = List.map f binds
          parens (sprintf "let (%s) . %O" (vsprintfs binds) e)
        | ESetRef (e1, e2)          -> 
          parens <| sprintf "set! %O %O" e1 e2
        | ERef e                    ->
          parens <| sprintf "alloc %O" e
        | EDeref e                  ->
          parens <| sprintf "deref %O" e
        | EGetField (e1, e2)        ->
          parens <| sprintf "get-field %O %O" e1 e2
        | EUpdateField (e1, e2, e3) -> 
          parens <| sprintf "update-field %O %O %O" e1 e2 e3
        | EDeleteField (e1, e2)     -> 
          parens <| sprintf "delete-field %O %O" e1 e2
        | ESeq (e1, e2)             -> 
          parens <| sprintf "begin %O %O" e1 e2
        | EIf (c, thn, els)         ->
          parens <| sprintf "if %O then %O else %O" c thn els
        | EWhile (c, body)          -> 
          parens <| sprintf "while %O {%O}" c body
        | ELabel (lbl, e)           ->
          parens <| sprintf "label %s: %O" !.lbl e
        | EBreak (lbl, e)           ->
          parens <| sprintf "break %s: %O" !.lbl e
        | EThrow e                  -> 
          parens <| sprintf "throw: %O" e
        | ECatch (e1, e2)           ->
          parens <| sprintf "try-catch %O %O" e1 e2
        | EFinally (e1, e2)         ->
          parens <| sprintf "try-finally %O %O" e1 e2
        //| ELet1 (bind1, eFn Expr * (id -> Expr)
        //| ELet2         of Expr * Expr * (id -> id -> Expr)
        | EEval                     -> 
          "eval-semantic-bomb"
    override self.ToString() = self.pretty

type Program = P of Expr 
  with 
    member self.pretty = let (P e) = self in e.pretty
    member self.pretty_doc = let (P e) = self in e.pretty_doc
    override self.ToString () = self.pretty

module Pattern =
  let (| Number | _ |) (expr : Expr)      = 
    match expr.value with
    | ENumber n -> Some <| Number n
    | _ -> None
  let (| String | _ |) (expr : Expr)      = 
    match expr.value with
    | EString n -> Some <| String n
    | _ -> None
  let (| Bool | _ |) (expr : Expr)        = 
    match expr.value with
    | EBool n -> Some <| Bool n
    | _ -> None
  let (| Undefined | _ |) (expr : Expr)   = 
    match expr.value with
    | EUndefined -> Some <| Undefined
    | _ -> None
  let (| Null | _ |) (expr : Expr)        = 
    match expr.value with
    | ENull -> Some Null
    | _ -> None
  let (| Lambda | _ |) (expr : Expr)      = 
    match expr.value with
    | ELambda (ids, e) -> Some <| Lambda (List.map (!.) ids, e)
    | _ -> None
  let (| MLambda | _ |) (expr : Expr)     = 
    match expr.value with
    | ELambda _ -> 
      match expr.Tag with
      | Some (Tag.Lambda name) -> Some <| MLambda name
      | _ -> failwith <| sprintf "Error because not correctly labeled @ %s." expr.pretty_localized
    | _ -> None
  let (| Object | _ |) (expr : Expr)      = 
    match expr.value with
    | EObject fields -> Some <| Object fields
    | _ -> None
  let (| Id | _ |) (expr : Expr)          = 
    match expr.value with
    | EId id -> Some <| Id !.id
    | _ -> None
  let (| Op | _ |) (expr : Expr)          = 
    match expr.value with
    | EOp (op, exps) -> Some <| Op (op, exps)
    | _ -> None
  let (| UnOp | _ |) (expr : Expr)        = 
    match expr.value with
    | EOp (op, [exp]) -> Some <| UnOp (op, exp)
    | _ -> None
  let (| BinOp | _ |) (expr : Expr)       = 
    match expr.value with
    | EOp (op, e1::e2::[]) -> Some <| BinOp (op, e1, e2)
    | _ -> None
  let (| App | _ |) (expr : Expr)         = 
    match expr.value with
    | EApp (callee, actuals) -> Some <| App (callee, actuals)
    | _ -> None
  let (| Let | _ |) (expr : Expr)         = 
    match expr.value with
    | ELet (binds, e) -> 
      let binds = List.map (fun (id, e) -> (!.id, e)) binds
      Some <| Let (binds, e)
    | _ -> None
  let (| SetRef | _ |) (expr : Expr)      = 
    match expr.value with
    | ESetRef (e1, e2) -> Some <| SetRef (e1, e2)
    | _ -> None
  let (| Ref | _ |) (expr : Expr)         = 
    match expr.value with
    | ERef e -> Some <| Ref e
    | _ -> None
  let (| Deref | _ |) (expr : Expr)       = 
    match expr.value with
    | EDeref e -> Some <| Deref e
    | _ -> None
  let (| GetField | _ |) (expr : Expr)    = 
    match expr.value with
    | EGetField (e1, e2) -> Some <| GetField (e1, e2)
    | _ -> None
  let (| UpdateField | _ |) (expr : Expr) = 
    match expr.value with
    | EUpdateField (e1, e2, e3) -> Some <| UpdateField (e1, e2, e3)
    | _ -> None
  let (| DeleteField | _ |) (expr : Expr) = 
    match expr.value with
    | EDeleteField (e1, e2) -> Some <| DeleteField (e1, e2)
    | _ -> None
  let (| Seq | _ |) (expr : Expr)         = 
    match expr.value with
    | ESeq (e1, e2) -> Some <| Seq (e1, e2)
    | _ -> None
  let (| If | _ |) (expr : Expr)          = 
    match expr.value with
    | EIf (e1, e2, e3) -> Some <| If (e1, e2, e3)
    | _ -> None
  let (| While | _ |) (expr : Expr)       = 
    match expr.value with
    | EWhile (e1, e2) -> Some <| While (e1, e2)
    | _ -> None
  let (| Label | _ |) (expr : Expr)       = 
    match expr.value with
    | ELabel (lbl, e) -> Some <| Label (!.lbl, e)
    | _ -> None
  let (| Break | _ |) (expr : Expr)       = 
    match expr.value with
    | EBreak (lbl, e) -> Some <| Break (!.lbl, e)
    | _ -> None
  let (| Throw | _ |) (expr : Expr)       = 
    match expr.value with
    | EThrow e -> Some <| Throw e
    | _ -> None
  let (| Catch | _ |) (expr : Expr)       = 
    match expr.value with
    | ECatch (e1, e2) -> Some <| Catch (e1, e2)
    | _ -> None
  let (| Finally | _ |) (expr : Expr)     = 
    match expr.value with
    | EFinally (e1, e2) -> Some <| Finally (e1, e2)
    | _ -> None
  let (| Eval | _ |) (expr : Expr)        = 
    match expr.value with
    | EEval -> Some Eval
    | _ -> None
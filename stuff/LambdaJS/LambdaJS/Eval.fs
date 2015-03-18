module LambdaJS.Eval

open LambdaJS.Syntax
open Utils.Env
open Utils.Monad

let f_ref = ref

type env = t<re_id, Val>

and num = int

and str = string

and ref = int

and obj = Map<string, Val>

and lambda = Syntax.Expr * env

and Val = 
  | Num of num
  | Str of str
  | Obj of obj
  | Ref of ref
  | Lambda of lambda

type state =
    {
        env   : env
        heap  : t<ref, Val>
    }

type SMonad<'a> = M<'a, state>

let M = builder<state>()

let MList = Utils.Monad.List(M)

let ref_cnt = f_ref 0

let set_ref v = 
  M{
    let! state = get_state
    let state = { state with heap = bind !ref_cnt v state.heap }
    do incr ref_cnt
    do! set_state state
  }

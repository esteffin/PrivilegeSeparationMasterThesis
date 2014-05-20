module LambdaJS.Parsing
open Utils.Prelude
open LambdaJS.Parser
open Microsoft.FSharp.Text.Lexing
open System.IO
open Lexer
open PreParsing

let range (lexbuf : LexBuffer<_>) : string = 
  sprintf "s_line: %d; s_col: %d -> e_line: %d; e_col: %d." lexbuf.StartPos.Line lexbuf.StartPos.Column lexbuf.EndPos.Line lexbuf.EndPos.Column

let parse (input : string) = 
  let inputChannel = new StreamReader(input)
  let lexbuf = LexBuffer<char>.FromTextReader inputChannel
  try 
    let p = Parser.start Lexer.tokenize lexbuf
    p
  with 
    | PreParsing.ParseErrorContextException ctx ->
        let message = ctx :?> Parsing.ParseErrorContext<Parser.token> //ctx :?> Parsing.ParseErrorContext<_>
        match message.CurrentToken with
          |None -> failwith (sprintf "unknown at line: %d col: %d" lexbuf.StartPos.Line lexbuf.StartPos.Column)
          |Some tk ->
            failwith <| sprintf "error parsing %s %s"  (token_to_string tk) (range lexbuf)
    | _ -> failwith (sprintf "unknown at line: %d col: %d" lexbuf.StartPos.Line lexbuf.StartPos.Column)
       
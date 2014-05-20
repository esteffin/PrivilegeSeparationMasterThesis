module LambdaJS.PreParsing

open Utils.Prelude
open Utils.ParsingUtils
open Microsoft.FSharp.Text.Lexing

exception ParseErrorContextException of obj

let parse_error_rich = Some (fun ctx -> raise (ParseErrorContextException ctx))

let throw_located_error loc err fmt = throw_formatted (fun msg -> failwith (sprintf "Error %s @ %O" msg loc)) fmt

let newline (lexbuf : LexBuffer<_>) = lexbuf.EndPos <- lexbuf.EndPos.NextLine

let lexeme = LexBuffer<_>.LexemeString

let get_location (lexbuf : LexBuffer<_>) = 
  new location( lexbuf.StartPos.Line, 
                lexbuf.StartPos.Column, 
                lexbuf.EndPos.Line, 
                lexbuf.EndPos.Column, 
                lexbuf.StartPos.FileName)
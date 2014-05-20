module Main.main
open Utils.Prelude
open LambdaJS.Parsing
open LambdaJS.Traverse
open System.IO
open Utils.Processes
open Text.PrettyPrint
module M = Utils.Monad.Maybe

let set_culture () = 
  do System.Threading.Thread.CurrentThread.CurrentCulture   <- new System.Globalization.CultureInfo("en-US")
  do System.Threading.Thread.CurrentThread.CurrentUICulture <- new System.Globalization.CultureInfo("en-US")

let full = true //false //
let just_analysis = false //true //

let get_error f = 
  let f = System.IO.File.ReadAllText f
  if f.StartsWith("\"<stdin>\" (") then
    Some f
  else
    None

let fprintf s = 
  System.IO.File.WriteAllText("out.js", s)

[<EntryPoint>]
let main args = 
  let desugar_only = Array.tryFind ((=) "--desugar-only") args
  do set_culture()
  do System.Environment.CurrentDirectory <- "..\\..\\"
  do Utils.Log.Redirect.duplicate_out "log.txt"
  let input_file = "Source.js"
  if just_analysis then
    do printfn "Not desugared."
  else
    let args = if full then sprintf "-full %s" input_file else input_file
    do printfn "Desugaring %s" args
    do start_process_wait @"C:\Users\Henry\BatchAliases\LJS.bat" args
  let desugared_file = if just_analysis && false then "Manual.fs" else sprintf "desugared.%s" input_file
  match desugar_only, get_error desugared_file with
  | None, None -> 
    do printfn "Processing %s" desugared_file
    let p = parse desugared_file
    let p = LambdaJS.ARename.rename p
    let cache = add_labels_lambda p
    let (LambdaJS.Syntax.P  e) = p 
    do fprintf e.pretty_doc.pretty
    let constraint_state = LambdaJS.ConstraintGenerator.generate_constraints p cache
    do LambdaJS.ConstraintGenerator.show_stats constraint_state
//    let (LambdaJS.Syntax.P e) = p
//    do fprintf e.pretty_doc.pretty
    do LambdaJS.ConstraintSolver.resolve_constraint constraint_state
    //do printfn "%s" (Text.PrettySeqs.pretty_seq prettifier ";" constraint_state.lst).pretty
    do wait_key_pressed ()
  | _, Some error -> 
    do printfn "Error desugaring: %s" desugared_file
    do printfn "%s" error
    do wait_key_pressed ()
  | Some _, _ -> 
    ()    
  1

       
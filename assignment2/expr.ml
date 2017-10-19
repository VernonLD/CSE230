(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)

type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Sub      of expr * expr           (* I add the operate of subtration, which take two expr then return half of the difference between them *)
  | Neg      of expr * expr * expr   (* I add the operate of negative which take three expr, 
                                        if the first one is bigger than the second then return the original value of the third expr, 
                                        otherewise return the negative value of third expr *)
  | Thresh   of expr * expr * expr * expr	


let rec exprToString e = 
  match e with
  | VarX                 -> "x"
  | VarY                 -> "y"
  | Sine     e'          -> "sin(pi*" ^ exprToString e' ^ ")"
  | Cosine   e'          -> "cos(pi*" ^ exprToString e' ^ ")"
  | Average (a, b)       -> "(" ^ "(" ^ exprToString a ^ "+" ^ exprToString b ^ ")" ^ "/2" ^ ")"
  | Times   (a, b)       -> exprToString a ^ "*" ^ exprToString b
  | Sub     (a,b)        -> exprToString a ^ "+" ^ exprToString b
  | Neg     (a,b,c)      -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ "-" ^ exprToString c ^ ")"
  | Thresh  (a, b, c, d) -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ exprToString d ^ ")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildSub(e1,e2)                = Sub(e1,e2)
let buildNeg(e1,e2,e3)             = Neg(e1,e2,e3) 
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)


let pi = 4.0 *. atan 1.0

let rec eval (e,x,y) = 
  match e with
  | VarX -> x
  | VarY -> y
  | Sine     e'          -> sin (pi *. eval(e',x,y)) 
  | Cosine   e'          -> cos (pi *. eval(e',x,y))
  | Average (a, b)       -> (eval(a,x,y)+.eval(b,x,y))/.2.0
  | Times   (a, b)       -> eval(a,x,y) *. eval(b,x,y)
  | Sub     (a, b)       -> (eval(a,x,y) -. eval(b,x,y))/.2.0
  | Neg     (a, b, c)    -> if eval(a,x,y) < eval(b,x,y) = true then eval(c,x,y) else -. eval(c,x,y)
  | Thresh  (a, b, c, d) -> if eval(a,x,y) < eval(b,x,y) = true then 0.01 *. eval(c,x,y) else 0.01 *. eval(d,x,y)

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)

(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

let rec assoc (d,k,l) = 
  match (d,k,l) with
   | (d,k,[]) -> d
   | (d,k,(k',v)::tl) -> if k = k' then v else assoc(d,k,tl)

(* fill in the code wherever it says : failwith "to be written" *)

(* function mem_m take two input h l with type of a' and a' list, if h exist 
  in l return l else return add h to the head of the list l.
  This function help us to determine the value of seen'.*)
let mem_m h l = 
  if List.mem h l = true then l else h::l

let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = mem_m h seen in
        let rest' = t in 
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
  match f b with
  | (b', false) -> b'
  | (b', true)  -> wwhile(f, b') 


(* fill in the code wherever it says : failwith "to be written" *)
(* function pre_f take parameter of a function f and a varibale x with 
  type of a', if the result of f x equal to x, return a tuple 
  with false boolean value , else return a tuple with true boolean value. 
  This function help us to modify parameter f in order to make it could be processed by funtion wwhile*)
let pre_f  = fun f -> fun x -> 
    match f x with
    | x' when x = x'-> (x',false)
    | y' -> (y',true)


let fixpoint (f,b) = wwhile ((pre_f f),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)

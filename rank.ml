(* a data type representing expressions.  Fill in: *)
type formula = 
Const of float 
| Sum of formula * formula 
| Difference of formula * formula 
| Product of formula * formula 
| Division of formula * formula
| Maximum of formula * formula
| Minimum of formula * formula 
| Aggregate 
| Rank
let make_add f1 f2 = Sum(f1, f2)
let make_sub f1 f2 = Difference(f1, f2)
let make_mul f1 f2 = Product(f1, f2)
let make_div f1 f2 = Division(f1, f2)
let make_max f1 f2 = Maximum(f1, f2)
let make_min f1 f2 = Minimum(f1, f2)
let make_float f = Const f
let make_rank () = Rank
let make_aggr () = Aggregate

let rec compute (f : formula) (rank : float) (agg : float) = match f with 
| Const f -> f
| Sum(f1,f2) -> (compute f1 rank agg) +. (compute f2 rank agg)
| Product(f1,f2) -> (compute f1 rank agg) *. (compute f2 rank agg)
| Difference(f1, f2)-> (compute f1 rank agg) -. (compute f2 rank agg)
| Division(f1, f2) -> (compute f1 rank agg) /. (compute f2 rank agg)
| Maximum(f1, f2) -> if (compute f1 rank agg) > (compute f2 rank agg) then (compute f1 rank agg) else (compute f2 rank agg)
| Minimum(f1, f2) -> if (compute f1 rank agg) < (compute f2 rank agg) then (compute f1 rank agg) else (compute f2 rank agg)
| Rank -> rank
| Aggregate -> agg 


let rank (c : 'a) (default : float) (ls : 'a list) = 
  let rec rank_helper c1 default1 lst acc = match lst with 
    | [] -> default1
    | h::t -> if h = c1 then acc else (rank_helper c1 default1 t (acc +. 1.))
  in rank_helper c default ls 1.

let rec score (c : 'a) (agg : float) (f : formula) (n : float) (ballots : 'a list list) = match ballots with 
| [] -> agg
| h::t ->  score c (compute f (rank c n h ) agg) f n t 
  
let score_all (cs : 'a list) (init : float) (f : formula) (ballots : 'a list list) : ('a * float) list = 
  let rec score_helper cs init f n bs =
   match cs with 
  |[] -> []
  |h::t -> (h , score h init f n bs) :: score_helper t init f n bs in score_helper cs init f (float_of_int(List.length cs)) ballots

module type STATE_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end;;

module type PROP_TYPE =
  sig
    type t
  end;;

module Kripke (State : STATE_TYPE) (Prop : PROP_TYPE) =
    struct
      type state = State.t
      module StateSet = Set.Make (State)
      module StatePair = struct
        type t = (state * state)
        let compare = Pervasives.compare
      end;;
      module StatePairs = Set.Make (StatePair)
      type prop = Prop.t
      type pi = state -> prop -> bool
      type k = StatePairs.t list
      type kripke = StateSet.t * pi * (StatePairs.t) list

      let make states pi k =
        (List.fold_right StateSet.add states StateSet.empty,
         pi,
         List.map (fun ki -> List.fold_right StatePairs.add ki StatePairs.empty) k)

      type phi =
        | Prim of prop
        | And of phi * phi
        | Not of phi
        | Or of phi * phi
        | Iff of phi * phi
        | Implies of phi * phi
        | Knows of int * phi
        | Everyone of int list * phi

      let rec satisfies ((states, pi, k) as kripke) state phi = match phi with
        | Prim prop -> (pi state prop)
        | And (p1, p2) ->
          (satisfies kripke state p1) && (satisfies kripke state p2)
        | Not p -> not (satisfies kripke state p)
        | Or (p1, p2) ->
          satisfies kripke state (Not (And (Not p1, Not p2)))
        | Implies (p1, p2) ->
          satisfies kripke state (Or (Not p1, p2))
        | Iff (p1, p2) ->
          satisfies kripke state (And (Implies (p1, p2), Implies (p2, p1)))
        | Knows (i, p) ->
          let reachables = 
            StatePairs.filter (fun (s, t) -> s = state) (List.nth k i) in
          StatePairs.for_all (fun (s, t) -> satisfies kripke t p) reachables
        | Everyone (is, p) ->
          List.for_all (fun i -> satisfies kripke state (Knows (i, p))) is

  end;;


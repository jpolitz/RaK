module F = Format
module K = Kripke

module Example1 = struct
  type state = S | T | U
  module StateOrd = struct
    type t = state
    let compare = Pervasives.compare
  end;;
  type prop = P
  module Prop = struct
    type t = prop
  end;;
  module SK = K.Kripke(StateOrd) (Prop)
  open SK

  let fig21 =
    let states = [S; T; U] in
    let k = [
      [(S, S); (S, T); (T, S); (T, T); (U, U)];
      [(S, S); (S, U); (T, T); (U, S); (U, U)]
    ] in
    let pi st _ = match st with
      | S
      | U -> true
      | T -> false in
    make states pi k

  let tests = [
    ("Primitive", fun () ->
      satisfies fig21 S (Prim P));
    ("Knows", fun () ->
      satisfies fig21 S (Knows (1, Prim P)));
    ("NotPrim", fun () ->
      not (satisfies fig21 T (Prim P)));
    ("Page20", fun () ->
      satisfies fig21 S
       (And (Not (Knows (0, Prim P)),
             And (Knows (1, Prim P),
                  And (Knows (0, Or (Knows (1, Prim P),
                                     Knows (1, Not (Prim P)))),
                       Not (Knows (1, Not (Knows (0, Prim P)))))))))
  ]

end;;

module MuddyChildren = struct
  type state = NNN | NNY | NYN | NYY | YNN | YNY | YYN | YYY
  module StateOrd = struct
    type t = state
    let compare = Pervasives.compare
  end;;
  type prop = M0 | M1 | M2 | SomeM
  module Prop = struct
    type t = prop
  end;;
  module SK = K.Kripke(StateOrd) (Prop)
  open SK

  let fig23 =
    let states = [NNN; NNY; NYN; NYY; YNN; YNY; YYN; YYY] in
    let reflexives = List.map (fun s -> (s, s)) states in
    let k = [
      [(NNN, YNN); (NYN, YYN); (NYY, YYY); (NNY, YNY);
       (YNN, NNN); (YYN, NYN); (YYY, NYY); (YNY, NNY)]@reflexives;
      [(NNN, NYN); (NNY, NYY); (YNY, YYY); (YNN, YYN);
       (NYN, NNN); (NYY, NNY); (YYY, YNY); (YYN, YNN)]@reflexives;
      [(NNN, NNY); (NYN, NYY); (YYN, YYY); (YNN, YNY);
       (NNY, NNN); (NYY, NYN); (YYY, YYN); (YNN, YNY)]@reflexives
    ] in
    let pi s p = match s, p with
      | NNN, M0 | NNY, M0 | NYN, M0 | NYY, M0 -> false
      | _, M0 -> true
      | NNN, M1 | NNY, M1 | YNN, M1 | YNY, M1 -> false
      | _, M1 -> true
      | NNN, M2 | NYN, M2 | YNN, M2 | YYN, M2 -> false
      | _, M2 -> true
      | _, SomeM -> false in
    make states pi k

  let fig24 =
    let states = [NNN; NNY; NYN; NYY; YNN; YNY; YYN; YYY] in
    let reflexives = List.map (fun s -> (s, s)) states in
    let k = [
      [(NYN, YYN); (NYY, YYY); (NNY, YNY);
       (YYN, NYN); (YYY, NYY); (YNY, NNY)]@reflexives;
      [(NNY, NYY); (YNY, YYY); (YNN, YYN);
       (NYY, NNY); (YYY, YNY); (YYN, YNN)]@reflexives;
      [(NYN, NYY); (YYN, YYY); (YNN, YNY);
       (NYY, NYN); (YYY, YYN); (YNN, YNY)]@reflexives
    ] in
    let pi s p = match s, p with
      | NNN, M0 | NNY, M0 | NYN, M0 | NYY, M0 -> false
      | _, M0 -> true
      | NNN, M1 | NNY, M1 | YNN, M1 | YNY, M1 -> false
      | _, M1 -> true
      | NNN, M2 | NYN, M2 | YNN, M2 | YYN, M2 -> false
      | _, M2 -> true
      | _, SomeM -> true in
    make states pi k
 
  let tests =[
    ("Child 0 has a muddy forehead in YNN",
     fun () ->
       (satisfies fig23 YNN (Prim M0)));
    ("Child 0 doesn't have a muddy forehead in NNN",
     fun () ->
       (satisfies fig23 NNN (Not (Prim M0))));
    ("Child 0 doesn't know she has a muddy forehead in YNN.",
     fun () ->
       (satisfies fig23 YNN (Not (Knows (0, Prim M0)))));
    ("Child 0 doesn't know she doesn't have a muddy forehead in NNN.",
     fun () ->
       (satisfies fig23 NNN (Not (Knows (0, Not (Prim M0))))));
    ("Child 1 knows that Child 0 has a muddy forehead in YNN",
     fun () ->
       (satisfies fig23 YNN (Knows (1, Prim M0))));

    ("Everyone knows that it can't be that no one is muddy after the father speaks",
     fun () ->
       let fact = Or (Prim M0, Or (Prim M1, Prim M2)) in
       (satisfies fig24 YNN (Everyone ([0;1;2], fact))));
    ("Everyone doesn't know that Child 0 is muddy after the father speaks",
     fun () ->
       let fact = Prim M0 in
       (satisfies fig24 YNN (Everyone ([0;1;2], fact))));
    ("But Child 1 and 2 do",
     fun () ->
       let fact = Prim M0 in
       (satisfies fig24 YNN (Everyone ([1;2], fact))))
  ]

end;;

module Main = struct
  let main () =
    List.iter (fun (desc, f) ->
      F.printf "%s" desc;
      if f() then F.printf " passed\n" else F.printf " failed\n")
      (List.concat [Example1.tests; MuddyChildren.tests])
end;;

Main.main ()


module F = Format
module K = Kripke

module Main = struct
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


  let main () =
    List.iter (fun (desc, f) ->
      F.printf "%s" desc;
      if f() then F.printf " passed\n" else F.printf " failed\n") tests
end;;
Main.main ()


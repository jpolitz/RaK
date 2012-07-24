module F = Format
module K = Kripke
open K.StringKripke

module Main = struct

  let fig21 =
    let states = ["s"; "t"; "u"] in
    let k = [
      [("s", "s"); ("s", "t"); ("t", "s"); ("t", "t"); ("u", "u")];
      [("s", "s"); ("s", "u"); ("t", "t"); ("u", "s"); ("u", "u")]
    ] in
    let pi st _ = match st with
      | "s"
      | "u" -> true
      | "t" -> false
      | _ -> failwith "'splosions" in
    make states pi k

  let tests = [
    (fun () -> F.printf "Should be true: %b\n"
      (satisfies fig21 "s" (Prim "p")));
    (fun () -> F.printf "Should be true: %b\n"
      (satisfies fig21 "s" (Knows (1, Prim "p"))));
    (fun () -> F.printf "Should be false: %b\n"
      (satisfies fig21 "t" (Prim "p")));
    (fun () -> F.printf "Should be true: %b\n"
      (satisfies fig21 "s"
        (And (Not (Knows (0, Prim "p")),
              And (Knows (1, Prim "p"),
                   And (Knows (0, Or (Knows (1, Prim "p"),
                                      Knows (1, Not (Prim "p")))),
                        Not (Knows (1, Not (Knows (0, Prim "p"))))))))))
  ]


  let main () =
    List.iter (fun f -> f()) tests
end;;
Main.main ()


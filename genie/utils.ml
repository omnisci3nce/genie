let rec repeat n f =
  match n with
  | 0 -> ()
  | a ->
      f ();
      repeat (a - 1) f

let typeable_key (key : Raylib.Key.t) : string option =
  let capitalise = Raylib.is_key_down Raylib.Key.Left_shift in
  let letter =
    match key with
    | A -> Some "a"
    | B -> Some "b"
    | C -> Some "c"
    | D -> Some "d"
    | E -> Some "e"
    | F -> Some "f"
    | G -> Some "g"
    | H -> Some "h"
    | I -> Some "i"
    | J -> Some "j"
    | K -> Some "k"
    | L -> Some "l"
    | M -> Some "m"
    | N -> Some "n"
    | O -> Some "o"
    | P -> Some "p"
    | Q -> Some "q"
    | R -> Some "r"
    | S -> Some "s"
    | T -> Some "t"
    | U -> Some "u"
    | V -> Some "v"
    | W -> Some "w"
    | X -> Some "x"
    | Y -> Some "y"
    | Z -> Some "z"
    | Space -> Some " "
    | _ -> None
  in
  Option.map (fun s -> if capitalise then String.uppercase_ascii s else s) letter

let box_keys =
  let open Raylib.Key in
  [ A; B; C; D; E; F; G; H; I; J; K; L; M; N; O; P; Q; R; S; T; U; V; W; X; Y; Z; Space ]

let get_typed_key () = box_keys |> List.filter Raylib.is_key_pressed |> List.find_map typeable_key

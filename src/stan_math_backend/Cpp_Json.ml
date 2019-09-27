open Core_kernel
open Middle

let rec sizedtype_to_json (st : mtype_loc_ad with_expr sizedtype) :
    Yojson.Basic.t =
  let emit_cpp_expr e = Fmt.strf "<< %a >>" Expression_gen.pp_expr e in
  match st with
  | SInt -> `Assoc [("name", `String "int")]
  | SReal -> `Assoc [("name", `String "real")]
  | SVector d | SRowVector d ->
      `Assoc [("name", `String "vector"); ("length", `String (emit_cpp_expr d))]
  | SMatrix (d1, d2) ->
      `Assoc
        [ ("name", `String "matrix")
        ; ("rows", `String (emit_cpp_expr d1))
        ; ("cols", `String (emit_cpp_expr d2)) ]
  | SArray (st, d) ->
      `Assoc
        [ ("name", `String "array")
        ; ("length", `String (emit_cpp_expr d))
        ; ("element_type", sizedtype_to_json st) ]

let var_type_json (name, st) : Yojson.Basic.t =
  `Assoc
    [ ("name", `String name)
    ; ("type", sizedtype_to_json st)]

let%expect_test "outvar to json pretty" =
  let var x = {expr= Var x; emeta= internal_meta} in
  (* the following is equivalent to:
     parameters {
       vector[N] var_one[K];
     }
  *)
  ("var_one", SArray (SVector (var "N"), var "K"))
  |> var_type_json |> Yojson.Basic.pretty_to_string |> print_endline ;
  [%expect
    {|
  {
    "name": "var_one",
    "type": {
      "name": "array",
      "length": "<< K >>",
      "element_type": { "name": "vector", "length": "<< N >>" }
    }
  } |}]

let replace_cpp_expr s =
  s
  |> Str.global_replace (Str.regexp "\"") "\\\""
  |> Str.global_replace (Str.regexp "\\\\\"<<") "\" <<"
  |> Str.global_replace (Str.regexp ">>\\\\\"") "<< \""

let wrap_in_quotes s = "\"" ^ s ^ "\""

let out_var_interpolated_json_str var_type_map =
  var_type_map |> List.map ~f:var_type_json
  |> (fun l -> `List l)
  |> Yojson.Basic.to_string |> replace_cpp_expr |> wrap_in_quotes

let%expect_test "outvar to json" =
  let var x = {expr= Var x; emeta= internal_meta} in
  [("var_one", SArray (SVector (var "N"), var "K"))]
  |> out_var_interpolated_json_str |> print_endline ;
  [%expect
    {|
    "[{\"name\":\"var_one\",\"type\":{\"name\":\"array\",\"length\":" << K << ",\"element_type\":{\"name\":\"vector\",\"length\":" << N << "}}}]" |}]

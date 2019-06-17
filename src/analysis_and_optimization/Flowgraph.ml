open Core_kernel
open Middle
open State.Cps

module type S = sig 
        
    type t
    
    (** A `Label` is a module with the type `t` overwhich the flowgraph
        is constructed along with 
    *)
    module Label : Label.S

    (** A `LabelSet` is a set with elements of type `Label.t` 
    *)
    module LabelSet : Set.S with module Elt := Label

    (** An label map is a map from the type of `Label.t`
    *)
    module LabelMap : Map.S with module Key := Label


    type flowgraph = 
        { initials : LabelSet.t 
        ; successors : LabelSet.t LabelMap.t 
        }

    val build : t -> flowgraph

end

module Reverse(X: S) : S 
    with type t := X.t     
    and module Label := X.Label    
    and module LabelSet := X.LabelSet
    and module LabelMap := X.LabelMap
    and type flowgraph := X.flowgraph = struct

    let build t : X.flowgraph = 
        let rev = X.build t in
        let initials = rev.successors
            |> Map.filter ~f:Set.is_empty
            |> Set.of_map_keys
        in 
        let init = 
            Map.map rev.successors  
                ~f:(fun _ -> X.LabelSet.empty )
        in
        let f ~key ~data accu =
            X.LabelSet.fold data 
                ~init:accu 
                ~f:(fun accu old_succ -> 
                        X.LabelMap.set accu 
                            ~key:old_succ
                            ~data:(X.LabelSet.add  
                                    (X.LabelMap.find_exn accu old_succ)  
                                    key 
                                    )
                    ) 
        in 

        let successors = 
                X.LabelMap.fold 
                rev.successors
                ~init
                ~f
        in         
        { initials 
        ; successors
        }
        
end



(* -- Flowgraph over `('e,'m) Middle.stmt_with` ----------------------------- *)


(**
    Flowgraph over `stmt_loc`
*)
module Statement_reverse : S = struct 

    type t = Middle.stmt_loc

    module Label = Label.Make(Int)

    module LabelSet = Set.Make_using_comparator(Label)

    module LabelMap = Map.Make_using_comparator(Label)

    type flowgraph = 
        { initials : LabelSet.t 
        ; successors : LabelSet.t LabelMap.t 
        }

    (** Traverse a `stmt_loc` adding unique integer labels and constructing 
        a map from label to `int stmt_label`
    *)
    let label_statements (stmt_loc : stmt_loc) =
        let apply_label label location = 
            {  location ; label } 
        in
        let f location =
            State.(
            get >>= fun (label,map) -> 
            let stmt = map_stmt_with ~e:(fun x -> x) ~f:(apply_label label) stmt_loc in
            let map' = LabelMap.add_exn map ~key:label ~data:stmt in
            put (label + 1,map') >>= fun _ -> 
            return @@ {location; label}
        )
        in
        traverse_stmt_with ~e:State.return ~f stmt_loc


    let build stmt_loc = 
        let _ = 
            label_statements stmt_loc
            |> State.run_state ~init:(0,LabelMap.empty)
        in 
        { initials = LabelSet.empty 
        ; successors = LabelMap.empty
        }
end 


module Statement = Reverse(Statement_reverse)


(* let mk_statement_map  stmt = 
    let rec aux 
let reverse_of_statement stmt =  *)

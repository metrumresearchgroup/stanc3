(* open Core_kernel *)

(* module type S = sig 
    type label
    type property 
    (* module M : Map.S with type 
    val  *)
end  *)

(** The central definition of a monotone dataflow analysis framework.
    Given a compatible flowgraph, lattice and transfer function, we can
    run the mfp (maximal fixed point) algorithm, which computes a maximal
    fixed point (MFP) for the set of equations/inequalities of properties at the
    entry and exit of each node in the flow graph, as defined by the triple.
    Note that this gives a safe approximation to the MOP (meet over all paths)
    solution that we would really be interested in, but which is often incomputable.
    In case of a distributive lattice of properties, the MFP and MOP solutions coincide.
    *)

(* module Make
    (F : Flowgraph.S)
    (L : Lattice.Lattice.S) 
    (T : Transfer_function.S with type labels = F.Node.t and type properties = L.t)
    : S with type label = F.Node.t 
    and type property = L.t 
     = struct

    type label = F.Node.t

    type property = L.t

end *)

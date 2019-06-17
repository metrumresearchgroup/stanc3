open Core_kernel

module Initial = struct

  module type S = sig
    module Label : Label.S
    module LabelSet : Set.S with module Elt := Label
    val initial : LabelSet.t
  end

  module Make_initial_empty (X : Label.S) : S with module Label = X = struct
    module Label = X
    module LabelSet = Set.Make_using_comparator (Label)
    let initial = LabelSet.empty
  end

  module Make_reaching_defn (Vars : S) (Lbls : Label.S) : S = struct
    module Label = struct
      module T = struct
        type t = Vars.Label.t * Lbls.t option
        [@@deriving compare, hash, sexp_of, of_sexp]
      end

      include T
      include Comparator.Make (T)
    end

    module LabelSet = Set.Make_using_comparator (Label)

    let initial = LabelSet.map ~f:(fun x -> (x, None)) Vars.initial
  end
end

module Total = struct
  module type S = sig
    module Label : Label.S
    module LabelSet : Set.S with module Elt := Label
    val total : LabelSet.t
  end
end

module InitialTotal = struct
  module type S = sig
    include Initial.S
    include Total.S with module Label := Label and module LabelSet := LabelSet
  end

  module Make_initial_empty (X : Total.S) :
    S with module Label := X.Label and module LabelSet := X.LabelSet = struct
    include X
    let initial = LabelSet.empty
  end
end

module LatticeNoBottom = struct
  module type S = sig
    type t

    val leq : t -> t -> bool
    val initial : t
    val lub : t -> t -> t
  end

  (**  The lattice without bottom of sets of some values, with the 
        inclusion order, set union and the empty set *)
  module Make_powerset (X : Initial.S) : S with type t = X.LabelSet.t = struct
    type t = X.LabelSet.t

    let initial = X.initial
    let leq s1 s2 = X.LabelSet.is_subset s1 ~of_:s2
    let lub s1 s2 = X.LabelSet.union s1 s2
  end



  module Make_dual_powerset (X : Initial.S) : S with type t = X.LabelSet.t =
  struct
    type t = X.LabelSet.t

    let initial = X.initial
    let leq s1 s2 = X.LabelSet.is_subset s2 ~of_:s1
    let lub s1 s2 = X.LabelSet.inter s1 s2
  end

  (** The lattice (without bottom) of partial functions, ordered under
        inverse graph inclusion, with intersection 
    *)
  module Make_dual_partial_function (Domain : Total.S) (Codomain : Label.S) :
    S
    with type t =
                ( Domain.Label.t
                , Codomain.t
                , Domain.Label.comparator_witness )
                Map.t = struct
    module PartialMap = Map.Make_using_comparator (Domain.Label)

    type t = Codomain.t PartialMap.t

    let initial = PartialMap.empty

    let leq s1 s2 =
      Domain.LabelSet.for_all Domain.total ~f:(fun k ->
          match (PartialMap.find s1 k, PartialMap.find s2 k) with
          | Some x, Some y -> x = y
          | Some _, None | None, None -> true
          | None, Some _ -> false )

    let lub s1 s2 =
      let f ~key ~data =
        Map.find s2 key
        |> Option.value_map ~default:false ~f:(fun v -> v = data)
      in
      PartialMap.filteri ~f s1
  end
end

module Lattice = struct
  (** A Lattice is a Lattice without bottom, with bottom :) 
    *)
  module type S = sig
    include LatticeNoBottom.S

    val bottom : t
  end

  (**  Add a fresh bottom element to a lattice (possibly without bottom) *)
  module Make_from_lattice_no_bottom (X : LatticeNoBottom.S) :
    S with type t = X.t option = struct
    type t = X.t option

    let initial = Some X.initial
    let bottom = None

    let leq s1_opt s2_opt =
      match (s1_opt, s2_opt) with
      | Some s1, Some s2 -> X.leq s1 s2
      | Some _, None -> false
      | None, Some _ | None, None -> true

    let lub s1_opt s2_opt =
      match (s1_opt, s2_opt) with
      | Some s1, Some s2 -> Some (X.lub s1 s2)
      | Some _, None -> s1_opt
      | None, Some _ | None, None -> s2_opt
  end

  (** The lattice of partial functions, where we add a fresh bottom element,
        to represent an inconsistent combination of functions 
    *)
  module Make_dual_partial_function (Domain : Total.S) (Codomain : Label.S) :
    S
    with type t =
                ( Domain.Label.t
                , Codomain.t
                , Domain.Label.comparator_witness )
                Map.t
                option = struct
    include Make_from_lattice_no_bottom
              (LatticeNoBottom.Make_dual_partial_function (Domain) (Codomain))
  end

  module Make_powerset (X : Initial.S) : S with type t = X.LabelSet.t = struct
    include LatticeNoBottom.Make_powerset (X)

    let bottom = X.LabelSet.empty
  end

  (** The specific powerset lattice we use for reaching definitions analysis 
        XXX : I've refactored so we don't really need this specialized functor
        though we may want it for convenience?
    *)
  module Make_reaching_definitions (Vars : Initial.S) (Lbls : Label.S) : S =
  struct
    include Make_powerset (Initial.Make_reaching_defn (Vars) (Lbls))
  end

  module Make_dual_powerset (X : InitialTotal.S) :
    S with type t = X.LabelSet.t = struct
    include LatticeNoBottom.Make_dual_powerset (X)

    let bottom = X.total
  end
end

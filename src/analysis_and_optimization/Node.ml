open Core_kernel

module type Basic = sig
  type t [@@deriving compare, sexp_of, of_sexp, hash]
end

module type S = sig
  type t [@@deriving compare, sexp_of, of_sexp, hash]

  include Comparator.S with type t := t
end

module Make (X : Basic) : S with type t = X.t = struct
  module T = struct
    type t = X.t

    let compare = X.compare
    let sexp_of_t = X.sexp_of_t
    let t_of_sexp = X.t_of_sexp
    let hash = X.hash
    let hash_fold_t = X.hash_fold_t
  end

  include T
  include Comparator.Make (T)
end

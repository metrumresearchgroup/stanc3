
module type Basic = sig 
    include Functor.S
    include Foldable.Basic with type 'a t := 'a t
end


module type S = sig
    include Functor.S 
    include Foldable.S with type 'a t := 'a t
end
  

module Make(X: Basic) : S with type 'a t := 'a X.t = struct
    let map = X.map
    include Foldable.Make(X)
end


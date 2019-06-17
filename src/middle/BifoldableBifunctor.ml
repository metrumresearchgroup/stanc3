

module type Basic = sig 
    include Bifunctor.Basic
    include Bifoldable.Basic with type ('a,'b) t := ('a,'b) t 
end


module type S = sig 
    include Bifunctor.S 
    include Bifoldable.S with type ('a,'b) t := ('a,'b) t
end 
  

module Make(X: Basic) : S with type ('a,'b) t := ('a,'b) X.t = struct
    include Bifunctor.Make(X)
    include Bifoldable.Make(X)
end
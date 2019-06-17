open Core_kernel

module type S = sig
    module F : FoldableFunctor.S
    module M : Monad.S
    val traverse : (f: 'a -> 'b M.t) -> 'a F.t -> ('b F.t) M.t  
end
  
  
module type S2 = sig
    module F : FoldableFunctor.S
    module M : Monad.S2
    val traverse : (f: 'a -> ('b,'e) M.t) -> 'a F.t -> ('b F.t,'e) M.t  
end
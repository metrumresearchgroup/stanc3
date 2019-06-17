open Core_kernel

module type S = sig
    module F : BifoldableBifunctor.S
    module M : Monad.S
    val bitraverse :  (e: 'a -> 'c M.t) -> (f: 'b -> 'd M.t) -> ('a,'b) F.t -> ('c,'d) F.t M.t  
end
  
  
module type S2 = sig
    module F : BifoldableBifunctor.S
    module M : Monad.S2
    val bitraverse :  (e: 'a -> ('c,'e) M.t) -> (f: 'b -> ('d,'e) M.t) -> ('a,'b) F.t -> (('c,'d) F.t,'e) M.t  
end
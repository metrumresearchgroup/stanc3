module type Basic = sig
    type ('a,'b) t
    val bimap : f:('a -> 'c) -> g:('b -> 'd) -> ('a,'b) t -> ('c,'d) t 
end  

module type S = sig 
    include Basic 
    val map_first : f:('a -> 'b) -> ('a,'c) t -> ('b,'c) t
    val map_second : f:('b -> 'c) -> ('a,'b) t -> ('a,'c) t
end 

module Make(X: Basic) : S with type ('a,'b) t := ('a,'b) X.t = struct
    let bimap = X.bimap

    let map_first ~f x = bimap ~f ~g:(fun x -> x) x

    let map_second ~f x = bimap ~f:(fun x -> x) ~g:f x
end 
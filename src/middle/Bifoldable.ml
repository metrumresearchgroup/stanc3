open Core_kernel

module type Basic = sig 
    type ('a,'b) t
    val bifold_left : f:('c -> 'a -> 'c) -> g:('c -> 'b -> 'c) -> init:'c -> ('a,'b) t -> 'c
    val bifold_right : 
        [ `Custom of f:('a -> 'c -> 'c) -> g:('b -> 'c -> 'c) -> init:'c -> ('a,'b) t -> 'c
        | `Define_using_bifold_left
        ]
end 

module type S  = sig 
    type ('a,'b) t
    val bifold_left : f:('c -> 'a -> 'c) -> g:('c -> 'b -> 'c) -> init:'c -> ('a,'b) t -> 'c
    val bifold_right : f:('a -> 'c -> 'c) -> g:('b -> 'c -> 'c) -> init:'c -> ('a,'b) t -> 'c
    
    val fold_left_first : f:('c -> 'a -> 'c) -> init:'c -> ('a,'b) t -> 'c
    val fold_left_second : f:('c -> 'b -> 'c) -> init:'c -> ('a,'b) t -> 'c
    val fold_right_first : f:('a -> 'c -> 'c) -> init:'c -> ('a,'b) t -> 'c
    val fold_right_second : f:('b -> 'c -> 'c) -> init:'c -> ('a,'b) t -> 'c

    val biany : pfirst:('a -> bool) -> psecond:('b -> bool) -> ('a,'b) t -> bool
    val biall : pfirst:('a -> bool) -> psecond:('b -> bool) -> ('a,'b) t -> bool
    
end 

module Make(X: Basic) : S with type ('a,'b) t := ('a,'b) X.t = struct
    let bifold_left = X.bifold_left

    let bifold_right_from_bifold_left ~f ~g ~init x =
        let f' k x z = k @@ f x z  
        and g' k x z = k @@ g x z  in
        bifold_left ~f:f' ~g:g' ~init:(fun x -> x) x init

    let bifold_right = 
        match X.bifold_right with 
        | `Custom f -> f 
        | `Define_using_bifold_left -> bifold_right_from_bifold_left


    let fold_left_first ~f ~init x = bifold_left ~f ~g:(fun accu _ -> accu) ~init x
    let fold_left_second ~f ~init x = bifold_left ~f:(fun accu _ -> accu) ~g:f ~init x

    let fold_right_first ~f ~init x = bifold_right ~f ~g:(fun _ accu -> accu) ~init x
    let fold_right_second ~f ~init x = bifold_right ~f:(fun _ accu -> accu) ~g:f ~init x

    let biany ~pfirst ~psecond x = 
        bifold_right ~init:false x
            ~f:(fun x accu -> accu || pfirst x) 
            ~g:(fun x accu -> accu || psecond x)

    let biall ~pfirst ~psecond x = 
        bifold_right ~init:true x
            ~f:(fun x accu -> accu && pfirst x) 
            ~g:(fun x accu -> accu && psecond x)

end 

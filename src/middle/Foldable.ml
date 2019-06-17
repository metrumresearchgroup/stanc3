open Core_kernel
module type Basic = sig 
    type 'a t
    val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b    
    val fold_right : 
        [ `Custom of f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
        | `Define_using_fold_left 
        ]
end

module type S = sig
    type 'a t
    val fold_right : f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
    val fold_left : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b 
    val to_list : 'a t -> 'a list
    val any : ('a -> bool) -> 'a t -> bool
    val all : ('a -> bool) -> 'a t -> bool
    val find : ('a -> bool) -> 'a t -> 'a option
end 

module Make(X: Basic) : S with type 'a t := 'a X.t = struct
    

    let fold_left = X.fold_left

    let fold_right_from_fold_left ~f ~init x = 
        let f' k x z = k @@ f x z in
        fold_left ~f:f' ~init:(fun x -> x) x init
      
    let fold_right = 
        match X.fold_right with 
        | `Custom f -> f 
        | `Define_using_fold_left -> fold_right_from_fold_left

    let to_list x =
        fold_right ~f:(fun x xs -> x::xs) ~init:[] x

    let any pred x = 
        fold_right ~f:(fun x accu -> accu || pred x) ~init:false x

    let all pred x = 
        fold_right ~f:(fun x accu -> accu && pred x) ~init:true x

    let find pred x = 
        fold_right ~f:(fun x accu -> if pred x then Some x else None |> Option.first_some accu) ~init:None x
    
end 


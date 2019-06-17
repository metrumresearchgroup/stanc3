(** The Middle Intermediate Representation, which program transformations
    operate on *)

open Core_kernel
(* open State.Cps *)

(** Source code locations *)
type location =
  { filename: string
  ; line_num: int
  ; col_num: int
  ; included_from: location option }
[@@deriving sexp]

(** Delimited locations *)
type location_span = {begin_loc: location; end_loc: location} [@@deriving sexp]



module LitType = struct
  type t = Int | Real | Str
  [@@deriving compare,hash, sexp]
end

module UnsizedType  = struct
  type t =
    | UInt
    | UReal
    | UVector
    | URowVector
    | UMatrix
    | UArray of t
    | UFun of (autodifftype * t) list * returntype
    | UMathLibraryFunction
  [@@deriving compare,hash,sexp]

  (** Flags for data only arguments to functions *)
  and autodifftype = DataOnly | AutoDiffable

  and returntype = Void | ReturnType of t
end


module SizedType = struct
    (** Sized types, for variable declarations *)
    type 'e t =
      | SInt
      | SReal
      | SVector of 'e
      | SRowVector of 'e
      | SMatrix of 'e * 'e
      | SArray of 'e t * 'e
    [@@deriving compare, fold, hash,map, sexp]

  module T = struct
    type nonrec 'a t = 'a t
    let map = map
    let fold_left ~f ~init x = fold f init x

    let fold_right = 
      let rec aux ~f ~init = function 
        | SInt -> init 
        | SReal -> init 
        | SVector e -> f e init
        | SRowVector e -> f e init
        | SMatrix(e1,e2) -> f e1 (f e2 init)
        | SArray(sizedtype,e) -> aux ~f ~init:(f e init) sizedtype

      in `Custom aux

  end 

  
  module FF = FoldableFunctor.Make(T)
  include FF

end

module PossiblySizedType = struct
  module T = struct
    type 'e t = 
      | Sized of 'e SizedType.t
      | Unsized of UnsizedType.t
    [@@deriving hash, map, fold, sexp]
  
    let fold_left ~f ~init x = fold f init x

    let fold_right =
      let aux ~f ~init = function
        | Sized sizedtype -> SizedType.fold_right ~f ~init sizedtype
        | Unsized _ -> init  in
      `Custom aux
    
  end

  include T 
  module FF = FoldableFunctor.Make(T)
  include FF
    
  
end



module IndexedExpr  = struct
  module T = struct
    type 'e t = 
      | All
      | Single of 'e
      | Upfrom of 'e
      | Downfrom of 'e
      | Between of 'e * 'e
      | MultiIndex of 'e
    [@@deriving hash, map, fold, sexp]

    let fold_left ~f ~init x = fold f init x

    
    let fold_right = 
      let aux ~f ~init = function
        | All -> init 
        | Single e -> f e init
        | Upfrom e -> f e init
        | Downfrom e -> f e init
        | Between(e1,e2) -> f e1 (f e2 init)
        | MultiIndex e -> f e init  
      in `Custom aux
      
  end
  include T 
  module FF = FoldableFunctor.Make(T)
  include FF

  (* module Traverse(M : Monad.S ) : Traversable.S with module F := FF = struct
    let traverse ~f = function
    | Single e -> M.map (f e) ~f:(fun e' -> Single e')
    | Upfrom e -> M.map (f e) ~f:(fun e' -> Upfrom e')
    | Downfrom e -> M.map (f e) ~f:(fun e' -> Downfrom e')
    | MultiIndex e -> M.map (f e) ~f:(fun e' -> Upfrom e')
    | Between (lower_e, upper_e) ->
      M.(
        f lower_e
        >>= fun lower_e' ->
        f upper_e |> map ~f:(fun upper_e' -> Between (lower_e', upper_e')))
    | All -> M.return All
  end 

  module Traverse2(M : Monad.S2) : Traversable.S2 with module F := FF = struct
    let traverse ~f = function
    | Single e -> M.map (f e) ~f:(fun e' -> Single e')
    | Upfrom e -> M.map (f e) ~f:(fun e' -> Upfrom e')
    | Downfrom e -> M.map (f e) ~f:(fun e' -> Downfrom e')
    | MultiIndex e -> M.map (f e) ~f:(fun e' -> Upfrom e')
    | Between (lower_e, upper_e) ->
      M.(
        f lower_e
        >>= fun lower_e' ->
        f upper_e |> map ~f:(fun upper_e' -> Between (lower_e', upper_e')))
    | All -> M.return All
  end 

  module TraverseState = Traverse2(State) *)

end

module FunKind = struct
  type t = StanLib | CompilerInternal | UserDefined
  [@@deriving compare, hash, sexp]
end

module Operator = struct
  type t =
    | Plus
    | PPlus
    | Minus
    | PMinus
    | Times
    | Divide
    | Modulo
    | LDivide
    | EltTimes
    | EltDivide
    | Pow
    | Or
    | And
    | Equals
    | NEquals
    | Less
    | Leq
    | Greater
    | Geq
    | PNot
    | Transpose
  [@@deriving compare,sexp,hash] 

  let to_string x = 
    Sexp.to_string (sexp_of_t x) ^ "__"
end

module Expr = struct

  module Pattern  = struct
    (** Expression pattern functor *)
    type 'e t = 
      | Var of string
      | Lit of LitType.t * string
      | FunApp of FunKind.t * string * 'e list
      | TernaryIf of 'e * 'e * 'e
      | EAnd of 'e * 'e
      | EOr of 'e * 'e
      | Indexed of 'e * 'e IndexedExpr.t list 
    [@@deriving hash, map, sexp, fold]

    module T = struct
      type nonrec 'a t = 'a t 
      let map = map 

      let fold_left ~f ~init x = fold f init x
      
      let fold_right = 
        let aux ~f ~init = function
          | Var _ | Lit _ -> init
          | FunApp(_,_,xs) ->  List.fold_right ~f xs ~init
          | TernaryIf(pred_e,true_e,false_e) ->  f pred_e (f true_e (f false_e init))
          | EAnd(e1,e2) | EOr(e1,e2) -> f e1 (f e2 init)
          | Indexed(e,idxs) -> 
              List.fold_right ~f:(fun x accu -> IndexedExpr.fold_right ~f ~init:accu x) idxs ~init
              |> f e
        in 
        `Custom aux
      
      
    end

    module FF = FoldableFunctor.Make(T)
    include FF
    
    let lit_string n = Lit(LitType.Str,n)
    let lit_real n = Lit(LitType.Real,string_of_float n)
    let lit_int n = Lit(LitType.Int, string_of_int n)
    let var name = Var name
    let if_then_else pred then_ else_ = TernaryIf(pred,then_,else_)
    let and_ a b = EAnd(a,b)
    let or_ a b = EOr(a,b)
    let udf name args = FunApp(FunKind.UserDefined,name,args)
    let compiler_fn name args = FunApp(FunKind.CompilerInternal, name, args)
    let stan_fn name args = FunApp(FunKind.StanLib,name,args)
    let plus a b = stan_fn Operator.(to_string Plus) [a;b]
    let pplus a b = stan_fn Operator.(to_string PPlus) [a;b]
    let minus a b = stan_fn Operator.(to_string Minus) [a;b]
    let pminus a b = stan_fn Operator.(to_string PMinus) [a;b]
    let times a b = stan_fn Operator.(to_string Times) [a;b]
    let divide a b = stan_fn Operator.(to_string Divide) [a;b]
    let modulo a b = stan_fn Operator.(to_string Modulo) [a;b]
    let ldivide a b = stan_fn Operator.(to_string LDivide) [a;b]
    let elt_times a b = stan_fn Operator.(to_string EltTimes) [a;b]
    let elt_divide a b = stan_fn Operator.(to_string EltDivide) [a;b]
    let pow a b = stan_fn Operator.(to_string Pow) [a;b]
    let equals a b = stan_fn Operator.(to_string Equals) [a;b]
    let nequals a b = stan_fn Operator.(to_string NEquals) [a;b]
    let less a b = stan_fn Operator.(to_string Less) [a;b]
    let leq a b = stan_fn Operator.(to_string Leq) [a;b]
    let greater a b = stan_fn Operator.(to_string Greater) [a;b]
    let geq a b = stan_fn Operator.(to_string Geq) [a;b]
    let pnot a= stan_fn Operator.(to_string PNot) [a]
    let transpose a = stan_fn Operator.(to_string Transpose) [a]
  end

  module T = struct
    (** Expressions *)
    type 'meta t = 
      { expr : 'meta t Pattern.t 
      ; emeta : 'meta 
      }
    [@@deriving hash, sexp]
    
    let rec map f { expr ; emeta } = 
      { expr = Pattern.map (map f) expr 
      ; emeta = f emeta 
      }

    let rec fold_left ~f ~(init: 'b) { expr ; emeta} = 
      let accu = f init emeta in
      Pattern.fold_left ~f:(fun init e -> fold_left ~f ~init e) ~init:accu expr
    
    let fold_right = 
      let rec aux ~f  ~(init: 'b) {expr ; emeta} : 'b = 
        let accu = Pattern.fold_right ~f:(fun e init -> aux ~f ~init e) ~init expr in
        f emeta accu
      in 
      `Custom aux
  end 

  include T
  module FF = FoldableFunctor.Make(T)
  include FF

  let rec fold_right_pattern ~f ~init {expr;_} = 
    f expr @@ Pattern.fold_right 
      ~f:(fun x accu -> fold_right_pattern ~f ~init:accu x) 
      ~init: 
      expr
            
  let rec fold_left_pattern ~f ~init {expr;_} = 
    Pattern.fold_left 
      ~f:(fun accu x -> fold_left_pattern ~f ~init:accu x) 
      ~init:(f init expr) 
      expr

  let any_pattern ~pred ?init:(accu=false) expr =
    let f x accu = accu || pred x in
    fold_right_pattern ~f ~init:accu expr

  let all_pattern ~pred ?init:(accu=true) expr =
    let f x accu = accu && pred x in
    fold_right_pattern ~f ~init:accu expr

  
  
  
  module Untyped = struct 
    type meta = unit
    type nonrec t = meta t
  end 

  module Typed = struct 
    type meta =
      { mtype: UnsizedType.t
      ; mloc: location_span sexp_opaque [@compare.ignore]
      ; madlevel: UnsizedType.autodifftype }
    [@@deriving sexp, hash]

    (** Typed expressions with location info  *)
    type nonrec t = meta t

    let type_of x = x.emeta.mtype
    let ad_level x = x.emeta.madlevel
    let location  x = x.emeta.mloc

    let sexp_of_t = sexp_of_t sexp_of_meta
    let t_of_sexp = t_of_sexp meta_of_sexp
  end 

(* Construct values *)
  let with_meta m p = { expr = p ; emeta = m}
    
  let lit_string ~meta n = with_meta meta @@ Pattern.lit_string n
  let lit_string_ = lit_string ~meta:()
  
  let lit_real ~meta n = with_meta meta @@ Pattern.lit_real n
  let lit_real_ = lit_real ~meta:()
  
  let lit_int ~meta n = with_meta meta @@ Pattern.lit_int n
  let lit_int_ = lit_int ~meta:()
      
  let var ~meta name = with_meta meta @@ Pattern.var name
  let var_ = var ~meta:()

  
  let if_then_else ~meta pred then_ else_ = with_meta meta @@ Pattern.if_then_else pred then_ else_
  let if_then_else_ = if_then_else ~meta:()
    
  let and_ ~meta a b = with_meta meta @@ Pattern.and_ a b 
  let and__ = and_ ~meta:()
  
  let or_ ~meta a b = with_meta meta @@ Pattern.or_ a b
  let or__ = or_ ~meta:()
    
  let udf ~meta name args = with_meta meta @@ Pattern.udf name args
  let udf_ = udf ~meta:()
  
  let compiler_fn ~meta name args = with_meta meta @@ Pattern.compiler_fn name args
  let compiler_fn_  = compiler_fn ~meta:()
  
  let stan_fn ~meta name args = with_meta meta @@ Pattern.stan_fn name args
  let stan_fn_  = compiler_fn ~meta:()
   
  let plus ~meta a b = with_meta meta @@ Pattern.plus a b
  let plus_ = plus ~meta:()
  
  let pplus ~meta a b = with_meta meta @@ Pattern.pplus a b
  let pplus_ = pplus ~meta:()
  
  let minus ~meta a b = with_meta meta @@ Pattern.minus a b
  let minus_ = minus ~meta:()
    
  let pminus ~meta a b = with_meta meta @@ Pattern.pminus a b
  let pminus_ = pminus ~meta:()
  
  let times ~meta a b = with_meta meta @@ Pattern.times a b
  let times_ = times ~meta:()
  
  let divide ~meta a b = with_meta meta @@ Pattern.divide a b
  let divide_ = divide ~meta:()
  
  let modulo ~meta a b = with_meta meta @@ Pattern.modulo a b
  let modulo_ = modulo ~meta:()
  
  let ldivide ~meta a b = with_meta meta @@ Pattern.ldivide a b
  let ldivide_ = ldivide ~meta:()
  
  let elt_times ~meta a b = with_meta meta @@ Pattern.elt_times a b
  let elt_times_ = elt_times ~meta:()
  
  let elt_divide ~meta a b = with_meta meta @@ Pattern.elt_divide a b
  let elt_divide_ = elt_divide ~meta:()
  
  let pow ~meta a b = with_meta meta @@ Pattern.pow a b
  let pow_ = pow ~meta:()
  
  let equals ~meta a b = with_meta meta @@ Pattern.equals a b
  let equals_ = equals ~meta:()
  
  let nequals ~meta a b = with_meta meta @@ Pattern.nequals a b
  let nequals_ = nequals ~meta:()
  
  let less ~meta a b = with_meta meta @@ Pattern.less a b
  let less_ = less ~meta:()
  
  let leq ~meta a b = with_meta meta @@ Pattern.leq a b
  let leq_ = leq ~meta:()
  
  let greater ~meta a b = with_meta meta @@ Pattern.greater a b
  let greater_ = greater ~meta:()
  
  let geq ~meta a b = with_meta meta @@ Pattern.geq a b
  let geq_ = geq ~meta:()
  
  let pnot ~meta a  = with_meta meta @@ Pattern.pnot a 
  let pnot_ = pnot ~meta:()
  
  let transpose ~meta a  = with_meta meta @@ Pattern.transpose a 
  let transpose_ = transpose ~meta:()

end

module Stmt = struct


  module Pattern = struct

    module T = struct
      type ('e, 's) t =
        | Assignment of (string * 'e IndexedExpr.t list) * 'e
        | TargetPE of 'e
        | NRFunApp of FunKind.t * string * 'e list
        | Break
        | Continue
        | Return of 'e option
        | Skip
        | IfElse of 'e * 's * 's option
        | While of 'e * 's  
        | For of {loopvar: string; lower: 'e; upper: 'e; body: 's}
        | Block of 's list
        | SList of 's list
        | Decl of
          { decl_adtype: UnsizedType.autodifftype
          ; decl_id: string
          ; decl_type: 'e PossiblySizedType.t 
          }
      [@@deriving sexp,map,fold,hash]
    
      let bimap ~f ~g x = map f g x

      let bifold_left ~f ~g ~init x = fold f g init x
      
      let bifold_right = 
        let bifold_right_aux ~f ~g ~init = function
          | Assignment((_,idxs),rval) -> 
              List.fold_right idxs ~init:(f rval init) 
                ~f:(fun idx accu -> IndexedExpr.fold_right ~f ~init:accu idx) 
          | TargetPE expr -> f expr init
          | NRFunApp(_,_,exprs) -> List.fold_right ~f exprs ~init 
          | Return (Some expr) -> f expr init 
          | Break | Continue | Skip | Return None -> init      
          | IfElse(pred_e,true_s,Some(false_s)) -> f pred_e (g true_s (g false_s init))
          | IfElse(pred_e,true_s,None) -> f pred_e (g true_s  init)
          
          | While(pred_e,s) -> f pred_e (g s init)
          | For {lower; upper; body; _} ->  f lower (f upper (g body init))
          | Block xs | SList xs  -> List.fold_right ~f:g xs ~init
          | Decl{decl_type;_} -> PossiblySizedType.fold_right ~f ~init  decl_type
        in 
        `Custom bifold_right_aux

    end 

    include T 
    module BB = BifoldableBifunctor.Make(T)
    include BB

    let assign name ?idxs:(ix=[]) e = Assignment((name,ix),e)    
    let targetPE e = TargetPE(e)    
    let nr_udf name args = NRFunApp(FunKind.UserDefined,name,args)
    let nr_compiler_fn name args = NRFunApp(FunKind.CompilerInternal, name, args)
    let nr_stan_fn name args = NRFunApp(FunKind.StanLib,name,args)    
    let break = Break
    let continue = Continue
    let skip = Skip
    let return value = Return(Some value)
    let return_void = Return(None)    
    let if_then_else pred then_ else_ = IfElse(pred,then_,Some else_)
    let if_then pred then_ = IfElse(pred,then_,None)    
    let while_ test body = While(test,body)
    let for_ loopvar lower upper body = For {loopvar; lower; upper; body}
    let block xs = Block(xs)
    let slist xs = SList(xs)    
    let declare ~adtype name ~ty  = Decl {decl_adtype = adtype; decl_id = name; decl_type = ty}
    let declare_unsized ~adtype name ~ty = declare ~adtype ~ty:(PossiblySizedType.Unsized ty) name
    let declare_sized ~adtype name ~ty = declare ~adtype ~ty:(PossiblySizedType.Sized ty) name
  end 
  

  module T = struct

    type ('exprmeta,'stmtmeta) t = 
      { stmt : ('exprmeta Expr.t , ('exprmeta,'stmtmeta) t) Pattern.t 
      ; smeta : 'stmtmeta
      }
    [@@deriving hash, sexp]

    let rec bimap ~f ~g {stmt; smeta} =
      { stmt = Pattern.bimap ~f:(Expr.map f) ~g:(bimap ~f ~g) stmt
      ; smeta = g smeta 
      }

    let rec bifold_left ~f ~g ~init {stmt;smeta} = 
      Pattern.bifold_left 
        ~f:(fun accu -> Expr.fold_left ~f ~init:accu)
        ~g:(fun accu -> bifold_left ~f ~g ~init:accu)
        ~init:(g init smeta)
        stmt

    let bifold_right = 
      let rec aux ~f ~g ~init {stmt;smeta} = 
        let accu = 
          Pattern.bifold_right 
            ~f:(fun x accu -> Expr.fold_right ~f ~init:accu x)
            ~g:(fun x accu -> aux ~f ~g ~init:accu x)
          ~init 
          stmt
        in 
        g smeta accu
        in
        `Custom aux
  end

  include T 
  module BB = BifoldableBifunctor.Make(T)
  include BB

  let pattern {stmt;_} = stmt 
  let meta {smeta;_} = smeta


let rec bimap ~f ~g { pattern ; meta } = 
    { pattern = Pattern.bimap (Expr.map f) (bimap ~f ~g) pattern 
    ; meta = g meta 
    }
    
  let map_first ~f x = bimap ~f:(fun x -> x) ~g:f x
    
  let map_second ~f x = bimap ~f ~g:(fun x -> x) x  
  
  let rec bifold_left ~f ~g ~init {pattern;meta} = 
    Pattern.bifold_left
      ~f:(fun accu x -> Expr.fold_left ~f ~init:accu x)
      ~g:(fun accu x -> bifold_left ~f ~g ~init:accu x)
      ~init:(g init meta)
      pattern
      
  let fold_left_first ~f ~init = bifold_left  ~f ~g:(fun accu _ -> accu) ~init
  
  let fold_left_second ~f ~init = bifold_left ~f:(fun accu _ -> accu) ~g:f ~init      
      
  let rec bifold_right ~f ~g ~init {pattern;meta} = 
    g meta @@ Pattern.bifold_right
      ~f:(fun x accu -> Expr.fold_right ~f ~init:accu x)
      ~g:(fun x accu -> bifold_right ~f ~g ~init:accu x)
      ~init
      pattern
  
  
  let fold_right_first ~f ~init = bifold_right   ~f ~g:(fun accu _ -> accu) ~init 
  
  let fold_right_second ~f ~init = bifold_right   ~f:(fun accu _ -> accu) ~g:f ~init  
  
  let rec bifold_right_pattern ~f ~g ~init {pattern;_} = 
    g pattern @@ Pattern.bifold_right 
      ~f:(fun x accu -> Expr.fold_right_pattern ~f ~init:accu x)
      ~g:(fun x accu -> bifold_right_pattern ~f ~g ~init:accu x) 
      ~init
      pattern

  let fold_right_pattern_first ~f ~init = 
    bifold_right_pattern ~f ~g:(fun _ accu -> accu) ~init
    
  let fold_right_pattern_second ~f ~init = 
    bifold_right_pattern ~f:(fun _ accu -> accu) ~g:f ~init
  
  
  let rec bifold_left_pattern ~f ~g ~init {pattern;_} = 
    Pattern.bifold_left 
      ~f:(fun accu x -> Expr.fold_left_pattern ~f ~init:accu x)
      ~g:(fun accu x -> bifold_left_pattern ~f ~g ~init:accu x) 
      ~init:(g init pattern ) 
      pattern
      
  let fold_left_pattern_first ~f ~init = 
    bifold_left_pattern ~f ~g:(fun accu _ -> accu) ~init
    
  let fold_left_pattern_second ~f ~init = 
    bifold_left_pattern ~f:(fun accu _ -> accu) ~g:f ~init
  

  (* These can be derived from a `bifold_*_pattern` definition *)
  let biany_pattern ~pred_first ~pred_second ?init:(accu=false) x =  
    bifold_right_pattern 
      ~f:(fun pattern accu -> accu || pred_first pattern)
      ~g:(fun pattern accu -> accu || pred_second pattern)
      ~init:accu 
      x
      
  let biall_pattern ~pred_first ~pred_second ?init:(accu=true) x =  
    bifold_right_pattern 
      ~f:(fun pattern accu -> accu && pred_first pattern)
      ~g:(fun pattern accu -> accu && pred_second pattern)
      ~init:accu 
      x
  


   (* Construct statements *) 
  let with_meta meta pattern = { pattern;meta }
    
  let assign ~meta name ?idxs e = with_meta meta @@ Pattern.assign name ?idxs e
  let assign_ name ?idxs e = assign ~meta:()  name ?idxs e
  
  let targetPE ~meta e = with_meta meta @@ Pattern.targetPE e
  let targetPE_ e = targetPE ~meta:() e
  
  let nr_udf ~meta name args = with_meta meta @@ Pattern.nr_udf name args
  let nr_udf_  name args = nr_udf ~meta:() name args
  
  let nr_compiler_fn ~meta name args =  with_meta meta @@ Pattern.nr_compiler_fn name args
  let nr_compiler_fn_  name args = nr_compiler_fn ~meta:() name args
  
  let nr_stan_fn ~meta name args =  with_meta meta @@ Pattern.nr_stan_fn name args
  let nr_stan_fn_  name args = nr_stan_fn ~meta:() name args
  
  let break ~meta =  with_meta meta @@ Pattern.break
  let break_ = break ~meta:()
  
  let continue ~meta = with_meta meta @@ Pattern.continue
  let continue_ = continue ~meta:()
  
  let skip ~meta = with_meta meta @@ Pattern.skip
  let skip_ = skip ~meta:()
  
  let return ~meta value = with_meta meta @@ Pattern.return value
  let return_ value = return ~meta:() value
  
  let return_void ~meta = with_meta meta @@ Pattern.return_void
  let return_void_ = return_void ~meta:()
  
  let if_then_else ~meta pred then_ else_ = with_meta meta @@ Pattern.if_then_else pred then_ else_
  let if_then_else_ pred then_ else_ = if_then_else ~meta:() pred then_ else_
  
  let if_then ~meta pred then_ = with_meta meta @@ Pattern.if_then pred then_
  let if_then_ pred then_ = if_then ~meta:() pred then_
  
  let while_ ~meta ~test ~body = with_meta meta @@ Pattern.while_ test body
  let while__ ~test ~body = while_ ~meta:() ~test ~body
  
  let for_ ~meta loopvar lower upper body = with_meta meta @@ Pattern.for_ loopvar lower upper body
  let for__ loopvar lower upper body = for_ ~meta:() loopvar lower upper body
  
  let block ~meta xs = with_meta meta @@ Pattern.block xs
  let block_ xs = block ~meta:() xs
  
  let slist ~meta xs = with_meta meta @@ Pattern.slist xs
  let slist_ xs = slist ~meta:() xs 
  
  let declare ~meta ~adtype name ~ty  = with_meta meta @@ Pattern.declare ~adtype name ~ty
  let declare_ ~adtype name ~ty = declare ~meta:() ~adtype name ~ty
  
  let declare_unsized ~meta ~adtype name ~ty = with_meta meta @@ Pattern.declare_unsized ~adtype name ~ty
  let declare_unsized_ ~adtype name ~ty = declare_unsized ~meta:() ~adtype name ~ty
  
  let declare_sized ~meta ~adtype name ~ty = with_meta meta @@ Pattern.declare_sized ~adtype name ~ty
  let declare_sized_ ~adtype name ~ty = declare_sized ~meta:() ~adtype name ~ty



  module Untyped = struct 
    type meta = unit 
    type nonrec t = (Expr.Untyped.meta,meta) t 
  end 

  module Typed = struct 
    type meta = location_span sexp_opaque[@compare.ignore] [@@deriving sexp]
    (** Statements with typed expressions and location info *)
    type nonrec t = (Expr.Typed.meta, meta) t

    let sexp_of_t = sexp_of_t Expr.Typed.sexp_of_t sexp_of_meta
    let t_of_sexp = t_of_sexp Expr.Typed.t_of_sexp meta_of_sexp
  end

  module Labelled = struct
    type 'lbl meta = 
        {location: location_span sexp_opaque [@compare.ignore]
        ; label: 'lbl
        }
    [@@deriving sexp]

    (** Statements with typed expressions, location info and labels *)
    type nonrec 'lbl t = (Expr.Typed.meta, 'lbl meta) t

    let sexp_of_t sexp_of_lbl = sexp_of_t Expr.Typed.sexp_of_t (sexp_of_meta sexp_of_lbl)
    let t_of_sexp lbl_of_sexp = t_of_sexp Expr.Typed.t_of_sexp (meta_of_sexp lbl_of_sexp)
  end 



end


module FunDef = struct
    type fun_arg_decl = (UnsizedType.autodifftype * string * UnsizedType.t) list
    [@@deriving hash,sexp]

    module T = struct
      type 's t =
        { fdrt: UnsizedType.t option
        ; fdname: string
        ; fdargs: fun_arg_decl
        ; fdbody: 's
        ; fdloc: location_span sexp_opaque [@compare.ignore] 
        }
      [@@deriving sexp, map, fold]  

      let fold_left ~f ~init x = fold f init x 

      let fold_right = `Define_using_fold_left
    end 

    include T 
    module FF = FoldableFunctor.Make(T)
    include FF
end 



module Program = struct 

  type io_block =
    | Data
    | Parameters
    | TransformedParameters
    | GeneratedQuantities
  [@@deriving sexp, hash]

  type 'e io_var = string * ('e SizedType.t * io_block) [@@deriving sexp, map]

  
  type ('e, 's) t =
  { functions_block: 's FunDef.t list
  ; input_vars: 'e io_var list
  ; prepare_data: 's list (* data & transformed data decls and statements *)
  ; log_prob: 's list (*assumes data & params are in scope and ready*)
  ; generate_quantities: 's list (* assumes data & params ready & in scope*)
  ; transform_inits: 's list
  ; output_vars: 'e io_var list
  ; prog_name: string
  ; prog_path: string }
  [@@deriving sexp, map]


  let name {prog_name;_} = prog_name
  let prepare_data {prepare_data;_} = prepare_data
  let output_vars {output_vars;_} = output_vars

  let transform_inits {transform_inits;_} = transform_inits

  module Untyped = struct 
    type nonrec t = (Expr.Untyped.t , Stmt.Untyped.t) t
  end 

  module Typed = struct 
    type nonrec t = (Expr.Typed.t , Stmt.Typed.t) t
    
  end 
end 

type internal_fn =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  | FnReadParam
  | FnWriteParam
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
[@@deriving sexp]









(* 
(** Arithmetic and logical operators *)
type operator =
  | Plus
  | PPlus
  | Minus
  | PMinus
  | Times
  | Divide
  | Modulo
  | LDivide
  | EltTimes
  | EltDivide
  | Pow
  | Or
  | And
  | Equals
  | NEquals
  | Less
  | Leq
  | Greater
  | Geq
  | PNot
  | Transpose
[@@deriving sexp, hash, compare]







type litType = Int | Real | Str [@@deriving sexp, hash, compare]

(**  *)
type fun_kind = StanLib | CompilerInternal | UserDefined
[@@deriving compare, sexp, hash]

type 'e index =
  | All
  | Single of 'e
  (*
  | MatrixSingle of 'e
 *)
  | Upfrom of 'e
  | Downfrom of 'e
  | Between of 'e * 'e
  | MultiIndex of 'e
[@@deriving sexp, hash, map, fold]

and 'e expr =
  | Var of string
  | Lit of litType * string
  | FunApp of fun_kind * string * 'e list
  | TernaryIf of 'e * 'e * 'e
  | EAnd of 'e * 'e
  | EOr of 'e * 'e
  | Indexed of 'e * 'e index list
[@@deriving sexp, hash, map, compare]

type fun_arg_decl = (autodifftype * string * unsizedtype) list
[@@deriving sexp, hash, map]

type 's fun_def =
  { fdrt: unsizedtype option
  ; fdname: string
  ; fdargs: fun_arg_decl
  ; fdbody: 's
  ; fdloc: location_span sexp_opaque [@compare.ignore] }
[@@deriving sexp, hash, map]

type 'e lvalue = string * 'e index list [@@deriving sexp, hash, map, fold]

type ('e, 's) statement =
  | Assignment of 'e lvalue * 'e
  | TargetPE of 'e
  | NRFunApp of fun_kind * string * 'e list
  | Break
  | Continue
  | Return of 'e option
  | Skip
  | IfElse of 'e * 's * 's option
  | While of 'e * 's
  (* XXX Collapse with For? *)
  | For of {loopvar: string; lower: 'e; upper: 'e; body: 's}
  (* A Block for now corresponds tightly with a C++ block:
     variables declared within it have local scope and are garbage collected
     when the block ends.*)
  | Block of 's list
  (* SList has no semantics, just programming convenience *)
  | SList of 's list
  | Decl of
      { decl_adtype: autodifftype
      ; decl_id: string
      ; decl_type: 'e possiblysizedtype }
[@@deriving sexp, hash, map, fold]










(* == Traversals ===============================================================
  A `traversal` is like a `map` except that it also allows for effects.

  In the following functions the effect is always from `State` since this is
  what we need in dataflow analysis.
*)
let traverse_index ~f = function
  | Single e -> State.map (f e) ~f:(fun e' -> Single e')
  | Upfrom e -> State.map (f e) ~f:(fun e' -> Upfrom e')
  | Downfrom e -> State.map (f e) ~f:(fun e' -> Downfrom e')
  | MultiIndex e -> State.map (f e) ~f:(fun e' -> Upfrom e')
  | Between (lower_e, upper_e) ->
      State.(
        f lower_e
        >>= fun lower_e' ->
        f upper_e |> map ~f:(fun upper_e' -> Between (lower_e', upper_e')))
  | All -> State.return All

let traverse_expr ~f = function
  | FunApp (fn_kind, name, exprs) ->
      State.(
        List.map ~f exprs |> all
        |> map ~f:(fun exprs' -> FunApp (fn_kind, name, exprs')))
  | EAnd (le, re) ->
      State.(f le >>= fun le' -> f re |> map ~f:(fun re' -> EAnd (le', re')))
  | EOr (le, re) ->
      State.(f le >>= fun le' -> f re |> map ~f:(fun re' -> EOr (le', re')))
  | Indexed (e, idxs) ->
      State.(
        f e
        >>= fun e' ->
        List.map ~f:(traverse_index ~f) idxs
        |> all
        |> map ~f:(fun idxs' -> Indexed (e', idxs')))
  | TernaryIf (pred, te, fe) ->
      State.(
        f pred
        >>= fun pred' ->
        f te
        >>= fun te' -> f fe |> map ~f:(fun fe' -> TernaryIf (pred', te', fe')))
  | Var name -> State.return @@ Var name
  | Lit (ty, name) -> State.return @@ Lit (ty, name)



let traverse_lvalue ~f (name, idxs) =
  idxs
  |> List.map ~f:(traverse_index ~f)
  |> State.all
  |> State.map ~f:(fun idxs' -> (name, idxs'))

let rec traverse_sizedtype ~f = function
  | SVector e -> f e |> State.map ~f:(fun e' -> SVector e')
  | SRowVector e -> f e |> State.map ~f:(fun e' -> SRowVector e')
  | SArray (sized_ty, e) ->
      State.(
        traverse_sizedtype ~f sized_ty
        >>= fun sized_ty' -> f e |> map ~f:(fun e' -> SArray (sized_ty', e')))
  | SReal -> State.return SReal
  | SInt -> State.return SInt
  | SMatrix (e1, e2) ->
      State.(
        f e1 >>= fun e1' -> f e2 |> map ~f:(fun e2' -> SMatrix (e1', e2')))

let traverse_possiblysizedtype ~f = function
  | Sized sizedtype -> State.map ~f:(fun ty -> Sized ty) @@ f sizedtype
  | Unsized unsizedtype -> State.return @@ Unsized unsizedtype

(** Statement traveral with effects fixed at `State` *)
let traverse_statement ~e ~f = function
  | IfElse (pred, s_true, Some s_false) ->
      State.(
        e pred
        >>= fun pred' ->
        f s_true
        >>= fun s_true' ->
        f s_false
        |> map ~f:(fun s_false' -> IfElse (pred', s_true', Some s_false')))
  | IfElse (pred, s_true, None) ->
      State.(
        e pred
        >>= fun pred' ->
        f s_true |> map ~f:(fun s_true' -> IfElse (pred', s_true', None)))
  | While (pred, body) ->
      State.(
        e pred
        >>= fun pred' -> f body |> map ~f:(fun body' -> While (pred', body')))
  | For {loopvar; lower; upper; body} ->
      State.(
        e lower
        >>= fun lower' ->
        e upper
        >>= fun upper' ->
        f body
        |> map ~f:(fun body' ->
               For {loopvar; lower= lower'; upper= upper'; body= body'} ))
  | Block xs -> State.(List.map ~f xs |> all |> map ~f:(fun xs' -> Block xs'))
  | SList xs -> State.(List.map ~f xs |> all |> map ~f:(fun xs' -> SList xs'))
  | Assignment (lvalue, expr) ->
      State.(
        traverse_lvalue ~f:e lvalue
        >>= fun lvalue' ->
        e expr |> map ~f:(fun expr' -> Assignment (lvalue', expr')))
  | TargetPE expr -> e expr |> State.map ~f:(fun expr' -> TargetPE expr')
  | NRFunApp (fun_kind, name, exprs) ->
      exprs |> List.map ~f:e |> State.all
      |> State.map ~f:(fun exprs' -> NRFunApp (fun_kind, name, exprs'))
  | Decl decl ->
      decl.decl_type
      |> traverse_possiblysizedtype ~f:(traverse_sizedtype ~f:e)
      |> State.map ~f:(fun decl_type' -> Decl {decl with decl_type= decl_type'})
  | Return (Some expr) ->
      e expr |> State.map ~f:(fun expr' -> Return (Some expr'))
  | Return None -> State.return @@ Return None
  | Break -> State.return Break
  | Continue -> State.return Continue
  | Skip -> State.return Skip

(* == Fixed types =========================================================== *)

(* -- Expressions ----------------------------------------------------------- *)

(** Fixed-point of expressions with meta-data *)
type 'm with_expr = {expr: 'm with_expr expr; emeta: 'm}
[@@deriving compare, sexp, hash]

(** Map a `with_expr` changing the type of meta-data *)
let rec map_expr_with ~f {expr; emeta} =
  {expr= map_expr (map_expr_with ~f) expr; emeta= f emeta}

let rec fold_expr_with ~f ~init {expr ; emeta } =
  fold_expr (fun accu -> fold_expr_with ~f ~init:accu) (f init emeta) expr


(** Traverse a `with_expr` mapping the type of meta-data with effects *)
let rec traverse_expr_with ~f {expr; emeta} =
  State.(
    f emeta
    >>= fun emeta' ->
    traverse_expr ~f:(traverse_expr_with ~f) expr
    >>= fun expr' -> return @@ {expr= expr'; emeta= emeta'})

(* -- Statements ------------------------------------------------------------ *)

(** Fixed-point of statements with meta-data *)
type ('e, 'm) stmt_with =
  {stmt: ('e with_expr, ('e, 'm) stmt_with) statement; smeta: 'm}
[@@deriving sexp]

(** Map a `stmt_with` changing the type of meta-data for the statments and 
    expressions 
*)
let rec map_stmt_with ~e ~f {stmt; smeta} =
  { stmt= map_statement (map_expr_with ~f:e) (map_stmt_with ~e ~f) stmt
  ; smeta= f smeta }

let rec fold_stmt_with ~e ~f ~init {stmt;smeta} =
  fold_statement      
    (fun accu -> fold_expr_with ~f:e ~init:accu)
    (fun accu -> fold_stmt_with ~e ~f ~init:accu) 
    (f init smeta)
    stmt



    

let rec traverse_stmt_with ~e ~f {stmt; smeta} =
  State.(
    f smeta
    >>= fun smeta' ->
    traverse_statement stmt ~e:(traverse_expr_with ~f:e)
      ~f:(traverse_stmt_with ~e ~f)
    |> map ~f:(fun stmt' -> {stmt= stmt'; smeta= smeta'}))




type ('e, 'm) stmt_with_num = {stmtn: ('e with_expr, int) statement; smetan: 'm}
[@@deriving sexp, hash]

(* == Fixed types specialised with meta-data ================================ *)

type expr_no_meta = unit with_expr
type stmt_no_meta = (expr_no_meta, unit) stmt_with

(** Type information added during semantic check 
*)
type mtype_loc_ad =
  { mtype: unsizedtype
  ; mloc: location_span sexp_opaque [@compare.ignore]
  ; madlevel: autodifftype }
[@@deriving compare, sexp, hash]

(** Typed expressions with location info 
*)
type expr_typed_located = mtype_loc_ad with_expr
[@@deriving sexp, compare, hash]

(** Statements with typed expressions and location info 
*)
type stmt_loc =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with
[@@deriving sexp]

(** Statements with typed expressions, location info and labels *)
type 'lbl stmt_labelled = (mtype_loc_ad, 'lbl stmt_label) stmt_with
[@@deriving sexp]

and 'lbl stmt_label =
  {location: location_span sexp_opaque [@compare.ignore]; label: 'lbl}
[@@deriving sexp]


let label_statements (stmt_loc : stmt_loc) : int stmt_labelled =
    let f location = State.(
        get >>= fun label -> 
        put (label + 1) >>= fun _ -> 
        return @@ {location; label}
    )
    in
    traverse_stmt_with ~e:State.return ~f stmt_loc
    |> State.run_state ~init:0
    |> fst



type stmt_loc_num =
  (mtype_loc_ad, (location_span sexp_opaque[@compare.ignore])) stmt_with_num
[@@deriving sexp]

type typed_prog = (mtype_loc_ad with_expr, stmt_loc) prog [@@deriving sexp]

type internal_fn =
  | FnLength
  | FnMakeArray
  | FnMakeRowVec
  | FnNegInf
  | FnReadData
  | FnReadParam
  | FnWriteParam
  | FnConstrain
  | FnUnconstrain
  | FnCheck
  | FnPrint
  | FnReject
[@@deriving sexp]

(**  A custom comparator which ignores locations on expressions *)
module ExprComparator = struct
  type t = expr_typed_located [@@deriving sexp, compare]
end

(**  A module for sets of expressions which ignore their locations *)
module ExprSet = Set.Make (ExprComparator)

(**  A module for maps of expressions which ignore their locations *)
module ExprMap = Map.Make (ExprComparator) *)

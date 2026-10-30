open Syntax
open Context
open Typecheck

type derivation =
  | DTrue of context
  | DFalse of context
  | DVar of context * string * ty
  | DAbs of context * string * ty * derivation * ty
  | DApp of context * derivation * derivation * ty
  | DIf of context * derivation * derivation * derivation * ty

let rec derive (ctx : context) (t : term) : ((ty * derivation), type_error) result =
  match t with
  (* T-True *)
  | True -> Ok (TyBool, DTrue ctx)
  (* T-False *)
  | False -> Ok (TyBool, DFalse ctx)
  (* T-Var *)
  | Var x ->
    (match lookup x ctx with
     | Some ty -> Ok (ty, DVar (ctx, x, ty))
     | None -> Error (UnboundVariable x))
  (* T-Abs *)
  | Abs (x, ty, body) ->
    let ctx' = extend x ty ctx in
    (match derive ctx' body with
     | Ok (body_ty, d_body) ->
       let arrow = TyArrow (ty, body_ty) in
       Ok (arrow, DAbs (ctx, x, ty, d_body, arrow))
     | Error e -> Error e)
  (* T-App *)
  | App (t1, t2) ->
    (match derive ctx t1 with
     | Error e -> Error e
     | Ok (ty1, d1) ->
       (match ty1 with
        | TyArrow (arg_ty, res_ty) ->
          (match derive ctx t2 with
           | Error e -> Error e
           | Ok (ty2, d2) ->
             if ty2 = arg_ty then Ok (res_ty, DApp (ctx, d1, d2, res_ty))
             else Error (ArgumentMismatch (arg_ty, ty2)))
        | _ -> Error (ExpectedFunction ty1)))
  (* T-If *)
  | If (c, t1, t2) ->
    (match derive ctx c with
     | Error e -> Error e
     | Ok (cty, dc) ->
       if cty <> TyBool then Error (ExpectedBool cty)
       else
         match derive ctx t1, derive ctx t2 with
         | Ok (ty1, d1), Ok (ty2, d2) ->
           if ty1 = ty2 then Ok (ty1, DIf (ctx, dc, d1, d2, ty1))
           else Error (BranchMismatch (ty1, ty2))
         | Error e, _ | _, Error e -> Error e)

let rec string_of_derivation (d : derivation) : string =
  let rule = function
    | DTrue _ -> "T-True"
    | DFalse _ -> "T-False"
    | DVar _ -> "T-Var"
    | DAbs _ -> "T-Abs"
    | DApp _ -> "T-App"
    | DIf _ -> "T-If"
  in
  match d with
  | DTrue ctx ->
    rule d ^ "\n" ^ string_of_context ctx ^ " |- true : Bool"
  | DFalse ctx ->
    rule d ^ "\n" ^ string_of_context ctx ^ " |- false : Bool"
  | DVar (ctx, x, ty) ->
    rule d ^ "\n" ^ string_of_context ctx ^ " |- " ^ x ^ " : " ^ string_of_ty ty
  | DAbs (ctx, x, ty, d_body, arrow) ->
    string_of_derivation d_body ^ "\n" ^ rule d ^ "\n" ^
    string_of_context ctx ^ " |- lambda " ^ x ^ ":" ^ string_of_ty ty ^ ". ... : " ^ string_of_ty arrow
  | DApp (ctx, d1, d2, res) ->
    string_of_derivation d1 ^ "\n\n" ^ string_of_derivation d2 ^ "\n" ^ rule d ^ "\n" ^
    string_of_context ctx ^ " |- (... ...) : " ^ string_of_ty res
  | DIf (ctx, dc, d1, d2, ty) ->
    string_of_derivation dc ^ "\n\n" ^ string_of_derivation d1 ^ "\n\n" ^
    string_of_derivation d2 ^ "\n" ^ rule d ^ "\n" ^
    string_of_context ctx ^ " |- if ... then ... else ... : " ^ string_of_ty ty

let string_of_derivation_precise (term : term) (d : derivation) : string =
  let rec go (t : term) (d : derivation) : string =
    match t, d with
    | True, DTrue ctx ->
      "T-True\n" ^ string_of_context ctx ^ " |- true : Bool"
    | False, DFalse ctx ->
      "T-False\n" ^ string_of_context ctx ^ " |- false : Bool"
    | Var x, DVar (ctx, _, ty) ->
      "T-Var\n" ^ string_of_context ctx ^ " |- " ^ x ^ " : " ^ string_of_ty ty
    | Abs (x, ty, body), DAbs (ctx, _, _, d_body, arrow) ->
      go body d_body ^ "\nT-Abs\n" ^
      string_of_context ctx ^ " |- lambda " ^ x ^ ":" ^ string_of_ty ty ^ ". " ^
      string_of_term body ^ " : " ^ string_of_ty arrow
    | App (t1, t2), DApp (ctx, d1, d2, res) ->
      go t1 d1 ^ "\n\n" ^ go t2 d2 ^ "\nT-App\n" ^
      string_of_context ctx ^ " |- " ^ string_of_term t ^ " : " ^ string_of_ty res
    | If (c, t1, t2), DIf (ctx, dc, d1, d2, ty) ->
      go c dc ^ "\n\n" ^ go t1 d1 ^ "\n\n" ^ go t2 d2 ^ "\nT-If\n" ^
      string_of_context ctx ^ " |- " ^ string_of_term t ^ " : " ^ string_of_ty ty
    | _ -> string_of_derivation d
  in
  go term d
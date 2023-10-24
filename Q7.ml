let rec get_type (gamma : env_type) (e : expr) : expr_type option =
  match e with
  | Var v -> StringMap.find_opt v gamma
  | IntLit _ -> Some IntType
  | Plus (e1, e2) | Minus (e1, e2) | Leq (e1, e2) ->
    (match (get_type gamma e1, get_type gamma e2) with
    | (Some IntType, Some IntType) -> Some IntType
    | _ -> None)
  | BoolLit _ -> Some BoolType
  | And (e1, e2) | Or (e1, e2) ->
    (match (get_type gamma e1, get_type gamma e2) with
    | (Some BoolType, Some BoolType) -> Some BoolType
    | _ -> None)
  | Not e1 ->
    (match get_type gamma e1 with
    | Some BoolType -> Some BoolType
    | _ -> None)
  | IfThenElse (e1, e2, e3) ->
    (match (get_type gamma e1, get_type gamma e2, get_type gamma e3) with
    | (Some BoolType, Some t2, Some t3) when t2 = t3 -> Some t2
    | _ -> None)

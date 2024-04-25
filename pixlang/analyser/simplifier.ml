(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points (entre autres) qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (pixels, coordonnées et couleurs) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

let simplifier (program : Ast.program) = program







(* Codez ici le simplificateur de termes.

    Tout comme pour le langage du cours, l’idée consiste à remplacer les termes constants par le résultat de leur calcul.

    Faites une sous-fonctions récursive pour les expressions et les statements.
    Ces fonction font un pattern matching sur leur argument et traitent chaque cas séparément. Elles renvoient un argument de même type que celui reçu.
    Par exemple : simplify_expression : Ast.expression -> Ast.expression

    Les cas minimaux attendus sont les cas sur les entiers, les flottants, les booléens, ainsi que les if dont le test est constant, et les for qui ne s’exécutent jamais.

    Deux points (entre autres) qui peuvent vous permettre d’aller plus loin :
      - les expressions ne peuvent pas faire d’effet de bord ici, ce qui permet de simplifier des expressions pas nécessairement constantes.
      - Les types composés (pixels, coordonnées et couleurs) peuvent également être simplifiés (e.g., (1,2) + (2,x) peut être simplifié en (3,2+x)).

    Vous détaillerez dans le rapport les différents cas que vous simplifiez dans votre simplificateur.
*)

open Ast

(* Simplifie une expression *)
let rec simplify_expr expr =
 match expr with
 | Binary_operator (op, e1, e2, annot) ->
 let e1_simplified = simplify_expr e1 in
 let e2_simplified = simplify_expr e2 in
 begin match (op, e1_simplified, e2_simplified) with
 | (Plus, Const_int (x, _), Const_int (y, _)) -> Const_int (x + y, annot)
 | (Minus, Const_int (x, _), Const_int (y, _)) -> Const_int (x - y, annot)
 | (Times, Const_int (x, _), Const_int (y, _)) -> Const_int (x * y, annot)
 | (Div, Const_int (x, _), Const_int (y, _)) -> 
 if y != 0 then Const_int (x / y, annot) 
 else Binary_operator (Div, e1_simplified, e2_simplified, annot)
 | (Plus, Coord (x1, y1, _), Coord (x2, y2, _)) -> 
 Coord (simplify_expr x1, simplify_expr y1, annot)
 | (Plus, Color (r1, g1, b1, _), Color (r2, g2, b2, _)) -> 
 Color (simplify_expr r1, simplify_expr g1, simplify_expr b1, annot)
 | (And, Const_bool (x, _), Const_bool (y, _)) -> Const_bool (x && y, annot)
 | (Or, Const_bool (x, _), Const_bool (y, _)) -> Const_bool (x && y, annot)
 | (Plus, Const_int (x, _), Const_real (y, _)) -> Const_real (float_of_int x +. y, annot)
 | (Plus, Const_real (x, _), Const_int (y, _)) -> Const_real (x +. float_of_int y, annot)
 | _ -> Binary_operator (op, e1_simplified, e2_simplified, annot)
 end
 | Unary_operator (Real_of_int, e, annot) ->
 begin match simplify_expr e with
 | Const_int (x, _) -> Const_real (float_of_int x, annot)
 | simplified -> Unary_operator (Real_of_int, simplified, annot)
 end
 | _ -> expr 
 
(* Simplifie une instruction *)
let rec simplify_instruction instruction =
 match instruction with
 | Block (stmts, annot) ->
 let simplified_stmts = List.map simplify_instruction stmts in
 Block (simplified_stmts, annot)
 | IfThenElse (test, then_stmt, else_stmt, annot) ->
 let test_simplified = simplify_expr test in
 if let Const_bool (b, _) = test_simplified in b
 then simplify_instruction then_stmt
 else simplify_instruction else_stmt
 | For (var, start, end', step, body, annot) ->
 let start_simplified = simplify_expr start in
 let end'_simplified = simplify_expr end' in
 let step_simplified = simplify_expr step in
 if let Const_int (s, _), Const_int (e, _) = start_simplified, end'_simplified in s > e
 then Block ([], annot)
 else For (var, start_simplified, end'_simplified, step_simplified, simplify_instruction body, annot)
 | Foreach (var, list_expr, body, annot) ->
 let list_simplified = simplify_expr list_expr in
 if let List (items, _) = list_simplified in items = []
 then Block ([], annot)
 else Foreach (var, list_simplified, simplify_instruction body, annot)
 | _ -> instruction 

(* Entry point for simplification; remove if unused *)
let simplifier (Program (args, body)) =
 let body_simplified = simplify_instruction body in
 Program (args, body_simplified)

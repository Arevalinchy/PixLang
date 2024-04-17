open Util
open Ast

(* Codez ici la passe de renommage de termes.

   Cette passe permet de simplifier grandement la gestion des passes suivantes, et de l’interprétation, en assurant qu’un nom de variable n’est jamais réutilisé dans la même portée de déclaration.

   Pour cela, on va modifier le programme en, pour chaque nom, gardant un nombre correspondant son nombre de redéfinition, et en renommant l’occurence de chaque nom par un identifiant unique (son nom, suivi de son nombre d’occurence, avec un séparateur interdit dans le langage (pour empêcher les redéfinitions)).

   Comme seule la portée des variables est importante, dans deux blocs disjoints, il est possible de réutiliser un même nom.

   Pour obtenir ce résultat, il sera nécessaire de copier les environnement avant d’évaluer des sous-blocs (puisqu’en sortant d’un bloc, il est possible de continuer à utiliser un nom défini plus haut).

   Attention, l’interpréteur ne fonctionnera pas correctement en cas de redéfinition si vous n’effectuez pas correctement cette passe.*)

let renaming (program : program) =
  let name_counter = Environment.new_environment () in
  Environment.add name_counter "x" 0;
  program
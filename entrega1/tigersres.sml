structure tigersres =
struct

open tigerabs
open tigertab
open tigertips
(*
datatype EnvEntry =
	VIntro	(* int readonly *)
	| Var of {ty: Tipo}
	| Func of {level: unit, label: tigertemp.label,
		formals: Tipo list, result: Tipo, extern: bool}


*)
datatype EnvEntry =
        VIntro of {access: tigertrans.access, level: int}   (* int readonly *)
        | Var of {ty: Tipo, access: tigertrans.access, level: int}
        | Func of {level: tigertrans.level, label: tigertemp.label,formals: Tipo list, result: Tipo, extern: bool}                   

(*val mainLevel = () *)
end

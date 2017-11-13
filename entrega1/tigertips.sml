structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil
	| TInt
	| TString
	| TArray of Tipo ref * unique
	| TRecord of (string * Tipo ref * int) list * unique
        | TFunc of Tipo list * Tipo
        | TTipo of string 
	
(*	| TTipo of string 
        | TArray of Tipo ref  * unique
Se agrega TFunc y se modifica TTipo y TArray*)

end

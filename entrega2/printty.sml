open tigertips
fun printTy (TNil) = "Nil"
  | printTy (TInt) = "Int"
  | printTy (TString) = "String"
  | printTy (TUnit) = "Unit"
  | printTy (TArray (a,b)) = "Array of "^ printTy(!a)
  | printTy (TRecord (a,b)) = "Record of "^ print'(a)
  | printTy (TTipo str) = str
and print' [] = ""
  | print' ((str,tr,i)::xs) = printTy(!tr)^" "^print'(xs)

structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertab
open printty

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
    tabNueva(),
    [("int", TInt), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
    tabNueva(),
    [("print", Func{level=mainLevel, label="print",
        formals=[TString], result=TUnit, extern=true}),
    ("flush", Func{level=mainLevel, label="flush",
        formals=[], result=TUnit, extern=true}),
    ("getchar", Func{level=mainLevel, label="getstr",
        formals=[], result=TString, extern=true}),
    ("ord", Func{level=mainLevel, label="ord",
        formals=[TString], result=TInt, extern=true}),
    ("chr", Func{level=mainLevel, label="chr",
        formals=[TInt], result=TString, extern=true}),
    ("size", Func{level=mainLevel, label="size",
        formals=[TString], result=TInt, extern=true}),
    ("substring", Func{level=mainLevel, label="substring",
        formals=[TString, TInt, TInt], result=TString, extern=true}),
    ("concat", Func{level=mainLevel, label="concat",
        formals=[TString, TString], result=TString, extern=true}),
    ("not", Func{level=mainLevel, label="not",
        formals=[TInt], result=TInt, extern=true}),
    ("exit", Func{level=mainLevel, label="exit",
        formals=[TInt], result=TUnit, extern=true})
    ])

fun tipoReal (TTipo s, (env : tenv)) : Tipo = 
    (case tabBusca(s , env) of 
         NONE => raise Fail "tipoReal Ttipo"
       | SOME t => t)
  | tipoReal (t, _) = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b =
        (* let *)
        (*     val a = case !r of *)
        (*         SOME t => t *)
        (*         | NONE => raise Fail "No debería pasar! (1)" *)
        (* in *)
        (*     tiposIguales a b *)
        (* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) =
        (* let *)
        (*     val b = case !r of *)
        (*         SOME t => t *)
        (*         | NONE => raise Fail "No debería pasar! (2)" *)
        (* in *)
        (*     tiposIguales a b *)
        (* end *)raise Fail "No debería pasar! (2)"
  | tiposIguales a b = (a=b)
(*  | tiposIguales _ = false*)
fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")

fun checkTipos t1 t2 pos = 	if tiposIguales t1 t2 
					then () 
					else error("Error de tipos, se espera "
							^ printTy (t1) 
							^ " y me diste "
							^ printTy (t2),pos)
			

fun transExp(venv, tenv) =
    let fun trexp(VarExp v) = trvar(v)
        | trexp(UnitExp _) = {exp=(), ty=TUnit}
        | trexp(NilExp _)= {exp=(), ty=TNil}
        | trexp(IntExp(i, _)) = {exp=(), ty=TInt}
        | trexp(StringExp(s, _)) = {exp=(), ty=TString}
        | trexp(CallExp({func, args}, nl)) =
            let
                val f  = case tabBusca (func, venv) of
                           SOME e => e
                           | NONE => error("Error. No existe la funcion \""^func^"\"", nl)
                val ltaf = (fn (Func {formals, ...}) => formals | _ => raise Fail ("No es Func\n")) f
                val trf = (fn (Func {result, ...}) => result | _ => raise Fail ("No es Func\n")) f      (*Pipe para evitar warning*)
                val lta = map (fn t => (trexp  t)) args
                val lta1 = map (fn {ty, ...} => ty) lta 
                val _ = if (length lta1) = (length ltaf) then () else error("Error cantidad de argumentos en \""^func^"\"", nl)
                val t = let 
                            fun tipolis (x::xs) (y::ys) = (tiposIguales x y) andalso tipolis xs ys (*short circuit andalso*)
                            | tipolis _ _ = true (* agregar error de tipo cuando hagamos prityprint de tipos*)
                        in
                            tipolis lta1 ltaf
                        end
	        val _ = if t then () else error("Error de tipo en  \""^func^"\"", nl)
            in
                {exp=(), ty=trf} (*COMPLETADO*)
            end
        | trexp(OpExp({left, oper=EqOp, right}, nl)) =
            let
                val {exp=_, ty=tyl} = trexp left
                val {exp=_, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
                    else error("Tipos no comparables", nl)
            end
        | trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
            let
                val {exp=_, ty=tyl} = trexp left
                val {exp=_, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
                    else error("Tipos no comparables", nl)
            end
        | trexp(OpExp({left, oper, right}, nl)) = 
            let
                val {exp=_, ty=tyl} = trexp left
                val {exp=_, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr then
                    case oper of
                        PlusOp => if tipoReal(tyl, tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | MinusOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | TimesOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | DivideOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | LtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | LeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | GtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | GeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
                        | _ => raise Fail "No debería pasar! (3)"
                else error("Error de tipos", nl)
            end
        | trexp(RecordExp({fields, typ}, nl)) =
            let
                (* Traducir cada expresión de fields *)
                val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

                (* Buscar el tipo *)
                val (tyr, cs) = case tabBusca(typ, tenv) of
                    SOME t => (case tipoReal(t,tenv) of
                        TRecord (cs, u) => (TRecord (cs, u), cs)
                        | _ => error(typ^" no es de tipo record", nl))
                    | NONE => error("Tipo inexistente ("^typ^")", nl)
                
                (* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
                fun verificar [] [] = ()
                  | verificar (c::cs) [] = error("Faltan campos", nl)
                  | verificar [] (c::cs) = error("Sobran campos", nl)
                  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
                        if s<>sy then error("Error de campo", nl)
                        else if tiposIguales ty (!t) then verificar cs ds
                             else error("Error de tipo del campo "^s, nl)
                val _ = verificar cs tfields
            in
                {exp=(), ty=tyr}
            end
        | trexp(SeqExp(s, nl)) =
            let    val lexti = map trexp s
                val exprs = map (fn{exp, ty} => exp) lexti
                val {exp, ty=tipo} = hd(rev lexti)
            in    { exp=(), ty=tipo } end
        | trexp(AssignExp({var, exp}, nl)) =
           let
	      	val tyvar = #ty (trvar (var,nl))
                val tyexp = #ty (trexp exp)
                val _     = checkTipos tyvar tyexp nl
           in
                 {exp=(), ty=tyvar}  end (*READY*)
        | trexp(IfExp({test, then', else'=SOME else'}, nl)) =
            let val {exp=testexp, ty=tytest} = trexp test
                val {exp=thenexp, ty=tythen} = trexp then'
                val {exp=elseexp, ty=tyelse} = trexp else'
            in
                if tipoReal(tytest,tenv)=TInt andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
                else error("Error de tipos en if" ,nl)
            end
        | trexp(IfExp({test, then', else'=NONE}, nl)) =
            let val {exp=exptest,ty=tytest} = trexp test
                val {exp=expthen,ty=tythen} = trexp then'
            in
                if tipoReal(tytest,tenv)=TInt andalso tythen=TUnit then {exp=(), ty=TUnit}
                else error("Error de tipos en if", nl)
            end
        | trexp(WhileExp({test, body}, nl)) =
            let
                val ttest = trexp test
                val tbody = trexp body
            in
                if tipoReal(#ty ttest, tenv) = TInt andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
                else if tipoReal(#ty ttest, tenv) <> TInt then error("Error de tipo en la condición", nl)
                else error("El cuerpo de un while no puede devolver un valor", nl)
            end
        | trexp(ForExp({var, escape, lo, hi, body}, nl)) =
                        let (*val {explo,tylo} = trexp lo 
                            val _ = if esInt tylo then () else error (...)
                            val {exphi,tyhi} = trexp hi
                            val _ = if esInt tyhi then () else error(...)
                            val venv' = tabinsert venv var(VarEntry {ty= TIntRo})
                            val {empbody,tybody} = transExp (tenv,venv',body)
                            val _ = if tybody = if tybody=TUnit then () else error (..)*)
            in {exp=(), ty=TUnit} end (*COMPLETAR*)
        | trexp(LetExp({decs, body}, _)) =
            let
                val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
                val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
            in 
                {exp=(), ty=tybody}
            end
        | trexp(BreakExp nl) =
            {exp=(), ty=TUnit} (*COMPLETAR*)
        | trexp(ArrayExp({typ, size, init}, nl)) =
            {exp=(), ty=TUnit} (*COMPLETAR*)
        and trvar(SimpleVar s, nl) =
              let 
                        val v = case tabBusca(s, venv) of
                                    SOME (Var e) => e
                                    | SOME _ => error ("Espera una variable y le das una funcion",nl)
                                    | NONE => error("No existe la variable \""^s^"\"", nl)
                        val tyv = #ty(v)
              in (*Ver ty de devolucion*)
            {exp=(), ty=tyv} 
	      end (*READY*)
        | trvar(FieldVar(v, s), nl) =
                let
                        val {exp,ty=tipor} = trvar (v,nl)
                        fun tiporecord (TRecord ([],un)) = error ("No existe la variable\""^s^"\" en el record \n",nl)
                          | tiporecord (TRecord ((str,typref,_)::xs , un)) = if str = s then !typref else tiporecord (TRecord (xs,un))   
                          | tiporecord _ = error ("Error de tipo, no es un Record",nl)
                        val typrec = tiporecord tipor  
                in {exp=(), ty=typrec} 
		end(*READY*)
        | trvar(SubscriptVar(v, e), nl) = 
                let
                        val {ty,...} = trexp e
                        val {exp,ty=tipov} = trvar (v,nl)
                        fun tipoarreglo (TArray (typref,_)) = !typref  
                          | tipoarreglo _  = error ("Se espera un tipo arreglo \n",nl)
                        val typarr = (tipoarreglo tipov)
                        val _ = if tiposIguales ty TInt then () else error ("Se espera un Int recibi un \n",nl) (*CUANDO HAGAMOS PPRINT \""^ty^"\" \n",nl) *)
                in
            		{exp=(), ty=typarr}
		end (*READY*)
        and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) =
                        let val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
                            val tyv = if tyinit = TNil then error ("La expresion es de tipo NIL, no se puede asignar a una variable",pos) else tyinit
			                val _ = if tyinit = TUnit then error ("La expresion devuevlve Unit, no se puede asignar a una variable",pos) else ()
                            val venv' = tabRInserta (name,(Var {ty=tyv}),venv)
                         in (venv', tenv, []) end (*READY*)
        | trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) = 
	        let
		             val {exp=expinit,ty=tyinit} = transExp (venv,tenv) init
                    val t' = case tabBusca (s,tenv) of
		                		  NONE => error ("El tipo \""^s^"\" no esta declarado",pos)
				                | SOME ss => ss
                    val tyv = if tyinit = TNil then error ("La expresion es de tipo NIL, no se puede asignar a una variable",pos) else tyinit
		            val _ = if tyinit = TUnit then error ("La expresion devuevlve Unit, no se puede asignar a una variable",pos) else ()
		            val _ = if tiposIguales tyv t' then () else error ("Se esperaba el tipo t' y me diste tyinit ",pos)(*hacer prettyprint*)
		            val venv' = tabRInserta (name,(Var {ty=tyv}),venv)
           in (venv',tenv, []) end (*READY *)
        | trdec (venv,tenv) (FunctionDec (xs)) =
          let 
                val venv' = auxVenv (venv,tenv) xs
                val _ = trdecfun (venv',tenv) xs
          in (venv',tenv,[]) end

        | trdec (venv,tenv) (TypeDec ts) = (venv, tenv, []) (*COMPLETAR*)
      and auxVenv (venv,tenv) [] = venv (*La lista tiene utilidad en la generacion de cod inter*) (*primera pasada,actualiza venv*)
        | auxVenv (venv,tenv) (({name, params, result, ...},pos)::fs) = 
          let
              val typparam = List.map (fn x => (transTy tenv pos) (#typ x)) params
              val result' = case result of
                                 SOME r => transTy tenv pos (NameTy r)
                               | NONE   => TUnit
              val venv' = tabRInserta (name ,(Func{formals=typparam,result=result',level=(), extern = true,label = tigertemp.newlabel()}),venv)
          in auxVenv (venv',tenv) fs end
      and auxBody venv tenv ({name,params,result,body},pos) = 
          let
              val venv''   = foldl (auxBodyFold tenv pos) venv params
              val {exp,ty} = transExp (venv'',tenv) body
              val result' = auxResult result tenv pos
          in 
              checkTipos result' ty pos
          end
      and auxBodyFold tenv pos (x,y) = tabRInserta (#name x, (Var {ty = (transTy tenv pos (#typ x))}),y) 
                  
      and trdecfun  (venv,tenv)  []   = ()
        | trdecfun  (venv,tenv) (x::xs) = 
          let 
             val _ = auxBody venv tenv x
          in trdecfun (venv,tenv)  xs end        
      and transTy tenv pos (NameTy s)    = 
          let 
              val ti = case tabBusca (s,tenv) of
                            SOME ss => ss
                           |NONE => error ("El tipo \""^s^"\" no esta definido",pos)
          in ti  end
        | transTy tenv pos _ = error ("No puede definir tipos dentro de una funcion",pos)
      and auxResult (SOME s) tenv pos = transTy tenv pos (NameTy s)
         |auxResult NONE a b  = TUnit
        in trexp end
fun transProg ex =
    let    val main =
                LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
                                result=NONE, body=ex}, 0)]],
                        body=UnitExp 0}, 0)
        val _ = (* transExp(tab_vars, tab_tipos) main*) transExp(tab_vars, tab_tipos) ex
    in  print "bien!\n" end
end


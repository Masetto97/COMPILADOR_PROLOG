
%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%														%%%%%% ANALIZADOR LEXICO%%%%%%%%%
%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Le pasamos el nombre del archivo entre comillas simples, y creamos ponemos un nombre a la lista de Token sacados del archivo %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conversor_archivo(File, Tokens) :-
    open(File, read, F),read_string(F, "","",S, String), close(F), string_chars(String,T), 
	tokens_cls(T,SL), rm_tokens_rep(SL,SL2),atomic_list_concat(SL2,StringL), split_string(StringL, "\n\n", "\t", Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regla para saber el total de tokens en un archivo, N es una variable que almacena el tamaño%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_tokens(File,Token,N):- conversor_archivo(File, Tokens),length(Tokens,N).

listita(File,Token,L):- conversor_archivo(File, Tokens),sort(Tokens,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reglas para limpiar los tokens, para ello recorremos una lista de tokens (T) y devolvemos la lista limpia en SL %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Caso Base %%

tokens_cls([],[]).

%% Operadores Arimeticos %%

tokens_cls([+|RESTO_CADENA],['\n',+,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([-|RESTO_CADENA],['\n',-,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([*|RESTO_CADENA],['\n',*,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([/|RESTO_CADENA],['\n',/,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).

%% Operadores Logicos %%

tokens_cls([>,=|RESTO_CADENA],['\n','>=','\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([<,=|RESTO_CADENA],['\n','<=','\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([<|RESTO_CADENA],['\n',<,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([>|RESTO_CADENA],['\n',>,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([=|RESTO_CADENA],['\n',=,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([!,=|RESTO_CADENA],['\n','!=','\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).

%% Fin, unión de sentencias y espacios en blanco %%

tokens_cls([;|RESTO_CADENA],['\n',;,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([:,=|RESTO_CADENA],['\n',:=,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([:|RESTO_CADENA],['\n',:,'\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([.|RESTO_CADENA],['\n',.|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).
tokens_cls([' '|RESTO_CADENA],['\n'|NUEVA_COLA_LISTA]):-tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA).

%% Resto de casos %%

tokens_cls([X|RESTO_CADENA],[X|NUEVA_COLA_LISTA]):- tokens_cls(RESTO_CADENA,NUEVA_COLA_LISTA),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reglas para saber si hay dos elementos iguales en la lista una vez limpiada %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Caso Base %%

rm_tokens_rep([],[]).

%% Saltos de lineas %%

rm_tokens_rep(['\n','\n'|RESTO_CADENA],['\n'|NUEVA_COLA_LISTA]):-rm_tokens_rep(RESTO_CADENA,NUEVA_COLA_LISTA).

%% Resto de casos %%

rm_tokens_rep([X|RESTO_CADENA],[X|NUEVA_COLA_LISTA]):-rm_tokens_rep(RESTO_CADENA,NUEVA_COLA_LISTA),!.

%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%														%%%% ANALIZADOR SINTACTICO%%%%%%%
%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%% Reglas del analizador_sintacticoR %%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comprobamos que el Programa en Pascal está bien estrucutrado desde su declaración (Program + nombre+;), cuerpo del programa (Bloque) y su final (.) %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pl_program(D,E) --> ["program"], identifier(X), [";"], bloque(D,E), ["."].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Empezamos a analizar el cuerpo del programa %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bloque(D,E) --> declaracion(D), statement(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Declaracion de las variables %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declaracion([V,X|R]) --> ["var"], variable(V,X), resto_variables(R).
resto_variables([V,X|R]) -->  [";"], variable(V,X), resto_variables(R).
resto_variables([]) --> [";"]. 

%% Enteros %%

variable(V,X) --> identifier(V), [":"], ["Integer"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Word"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Shortint"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Byte"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Longint"], {X is 0}. 

%% Decimales %%

variable(V,X) --> identifier(V), [":"], ["Real"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Single"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Double"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Extended"], {X is 0}. 
variable(V,X) --> identifier(V), [":"], ["Comp"], {X is 0}. 

%% Caracteres %%

variable(V,X) --> identifier(V), [":"], ["Char"], {X is 0}. 

%% Cadena de caracteres %%

variable(V,X) --> identifier(V), [":"], ["String"], {X is 0}. 

%% Booleanos %%

variable(V,X) --> identifier(V), [":"], ["Boolean"], {X is 0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Inicio Cuerpo Programa %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

statement((S;Ss)) --> ["begin"], statement(S), rest_statement(Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% Cuerpo Programa %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Asignación de valores %%

statement(read(X)) --> ["read"], identifier(X), [";"].
statement(assign(X,V)) --> identifier(X), [":="], expression(V), [";"].

%% Estructuras de control %%

statement(if(T,S1,S2)) --> ["if"], test(T), ["then"], statement(S1), ["else"], statement(S2).

%% Bucles %%

statement(while(T,S)) --> ["while"], test(T), ["do"], statement(S).

%% escritura de valores %%

statement(write(X)) --> ["write"], expression(X), [";"].

%% Fin del cuerpo %%

rest_statement(end) --> ["end"].
rest_statement((S;Ss)) --> statement(S), rest_statement(Ss).

%% OPERACIONES ARITMETICAS Y LOGICAS %%

expression(X) --> pl_constant(X).
expression(expr(Op, X, Y)) --> pl_constant(X), op_aritmetica(Op), expression(Y).

%% ARITMETICAS %%

op_aritmetica('+') --> ["+"].
op_aritmetica('*') --> ["*"].
op_aritmetica('-') --> ["-"].
op_aritmetica('/') --> ["/"].

%% CONSTANTES %%

pl_constant(X) --> identifier(X).
identifier(X) --> [X].
test(compare(Op,X,Y)) --> expression(X), op_logicas(Op), expression(Y).

%% OPERACIONES LOGICAS %% 

op_logicas('=') --> ["="].
op_logicas('!=') --> ["!="].
op_logicas('>') --> [">"].
op_logicas('<') --> ["<"].
op_logicas('>=') --> [">="].
op_logicas('<=') --> ["<="].

%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%														%%%% SEMANTICA OPERACIONAL %%%%%%
%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% ESTADO OPERACIONAL %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Caso Base %%

operar((S;Ss), V, NNV) :- operar(S, V, NV), operar(Ss, NV, NNV).

%% Asignar valor %%

operar(read(X),V, NV) :-  read(F), asignarvalor(X,F,V, NV).
operar(assign(X,Y), V, NV) :- calcularExpr(Y, V, S), asignarvalor(X, S, V, NV).

%% Comproba valor %%

operar(if(T,S1,S2),V, NV) :- logicas(T, V),!, operar(S1, V, NV).
operar(if(T,S1,S2),V, NV) :- operar(S2, V, NV).

%% Bucle While %%

operar(while(T,S),V, NNV) :- logicas(T, V),!, operar(S, V, NV), operar(while(T,S),NV,NNV).
operar(while(T,S),NV,NV).

%% asignar valor %%

operar(write(X),V, V) :- asignar(X, V).
operar(end, V, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% SEMANTICA %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Asignar valor %%

asignarvalor(S, Val, [X,Y|R], V):- S==X , V=[S,Val|R].
asignarvalor(S, Val, [X,Y|R], V):- S\=X, asignarvalor(S, Val, R, E), V=[X,Y|E].
asignarvalor(S, Val, [], V):- fail.

%% Calculo Expresiones %%

calcularExpr(expr(X, R, Z), V, S) :- atom_number(R, R1), atom_number(Z, Z1), operar(X, R1, Z1, S). 
calcularExpr(expr(X, R, Z), V, S) :- atom_number(R, R1), encontrarValor(Z, V, Z1), operar(X, R1, Z1, S).
calcularExpr(expr(X, R, Z), V, S) :- encontrarValor(R, V, R1), atom_number(Z, Z1), operar(X, R1, Z1, S).
calcularExpr(expr(X, R, Z), V, S) :- encontrarValor(R, V, R1), encontrarValor(Z, V, Z1), operar(X, R1, Z1, S).
calcularExpr(Y, V, S) :- atom_number(Y, S). 

%% Operaciones ARITMETICAS%%

operar(X, R1, Z1, S) :- X='+', S is R1+Z1.
operar(X, R1, Z1, S) :- X='*', S is R1*Z1.
operar(X, R1, Z1, S) :- X='-', S is R1-Z1.
operar(X, R1, Z1, S) :- X='/', S is R1/ Z1.

%% asignar valor %%

asignar(X,V) :- encontrarValor(X, V, R1), write(X), write("="), write(R1), write("\n").
asignar(X,V) :- atom_number(X, R1), write_ln(R1).

%% Operaciones LOGICAS%% 

logicas(compare(X,Y,Z), V) :- X = '=', encontrarValor(Y, V, Yi), encontrarValor(Z, V, Zi), Yi=Zi.
logicas(compare(X,Y,Z), V) :- X = '>', encontrarValor(Y, V, Yi), encontrarValor(Z, V, Zi), Yi>Zi.
logicas(compare(X,Y,Z), V) :- X = '<', encontrarValor(Y, V, Yi), encontrarValor(Z, V, Zi), Yi<Zi.
logicas(compare(X,Y,Z), V) :- X = '<=', encontrarValor(Y, V, Yi), encontrarValor(Z, V, Zi), Yi=<Zi.
logicas(compare(X,Y,Z), V) :- X = '>=', encontrarValor(Y, V, Yi), encontrarValor(Z, V, Zi), Yi>=Zi.

%% Busqueda valores para operar %%

encontrarValor(Y, [X,S|R], Z) :- Y=X, Z=S.
encontrarValor(Y, [X,S|R], Z) :- Y\=X, encontrarValor(Y, R, Z).
encontrarValor(Y, [], Z) :- fail.

%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%														%%%% LLAMADAS PRINCIPALES %%%%%
%														%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


analizador_sintactico(Fuente, Estructura, Variables) :- pl_program(Estructura, Variables, Fuente,[]), !.
analizador_lexico(Ar,Ltok):- conversor_archivo(Ar, Ltok).
compilar(Ar):- analizador_lexico(Ar,L),analizador_sintactico(L,V, S), operar(S, V, Fin), write("Estado Actual: "), write_ln(Fin), !.














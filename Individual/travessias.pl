%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica 

%---------------------------------  base de conhecimento ---------


:-include('pontos.pl').
%%pontos_recolha(latitude, longitude, local, tipo, quantidade, [lista]).

:-include('arcos_final.pl').
%%arco(RUA1, RUA2, DIST).


inicial('R do Alecrim').
%final('Av 24 de Julho').
final('Pc Duque da Terceira').
%final('R da Boavista').
%final('R Ferragial').

% -------------------Auxiliares -------------

%verifica se é membro da lista
membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).


%Extensao do meta-predicado nao
nao( Questao ) :- Questao, !, fail.
nao( Questao ).

%Calcula o comprimento de uma lista
comprimento(S,N) :- length(S,N).

%Predicado que da um print no terminal
imprime([]).
imprime([X|T]) :- write(X), nl, imprime(T).

%verifica se um elemento pertence a uma  lista
temElem([],_).
temElem(L,[H|T]):- membro(H,L).
temElem(L,[H|T]):- temElem(L,T);memberchk(H,L).  

%Obtem um arco apartir de um id
adjacente(Origem,Destino,Dist) :- arco(Origem,Destino,Dist).
adjacente(Origem,Destino,Dist) :- arco(Destino,Origem,Dist).

%verifica se uma lista esta vazia
estaVazia(L,V) :- comprimento(L,V),nao(V>0).

%encontra o menor elemento 
minimo(L, (A,B)) :-
    seleciona((A,B), L, R),
    \+ ( membro((A1,B1), R), B1 < B ).

%encontra o maior elemento 
maximo(L, (A,B)) :-
    seleciona((A,B), L, R),
    \+ ( membro((A1,B1), R), B1 > B ).

%reverte uma lista
reverseL(Ds,Es) :- reverseL(Ds, [], Es).
reverseL([],Ds,Ds).
reverseL([D|Ds],Es,Fs) :- reverseL(Ds, [D|Es], Fs).

%verifica qual o tipo de lixo de um ponto recolha 
getLixo(Local, Tipo) :- pontos_recolha(_,_,Local,Tipo,_, _).

%Estima o custo até ao nodo final
estimativa(Nodo,Est):-
    distance(Nodo, Est).

%Calcula a distancia até ao nodo final
distance(Origem,Dis):-
    pontos_recolha(Lat1,Lon1,Origem,_,_,_),
    final(Destino),
    pontos_recolha(Lat2,Lon2,Destino,_,_,_),
    P is 0.017453292519943295,
    A is (0.5 - cos((Lat2 - Lat1) * P) / 2 + cos(Lat1 * P) * cos(Lat2 * P) * (1 - cos((Lon2 - Lon1) * P)) / 2),
    Dis is (12742 * asin(sqrt(A))).


seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

%--------------------------------------





%------------------------------Procura não informada ------------------

%% --------------------------Depth first nº arcos---------------------------

resolveDP([Nodo|Caminho]):-
    inicial(Nodo),
    primeiroprofundidade(Nodo,[Nodo],Caminho).

primeiroprofundidade(Nodo,_, []):-
    final(Nodo).

primeiroprofundidade(Nodo, Historico, [NodoProx|Caminho]):-
    adjacente(Nodo, NodoProx,_),
    nao(membro(NodoProx, Historico)),
    primeiroprofundidade(NodoProx, [NodoProx|Historico], Caminho).


ppTodasSolucoes(L):- findall((S,C), (resolveDP(S), length(S,C)),L).
% ppTodasSolucoes(L):- findall((S,C), (resolveDP(S), length(S,C)),L)
%                    ,length(L,Tam)
%                    ,write(Tam).



menorNArcosDP(Nodo,Cam,NA):- findall((Ca,N), (resolveDP(Ca), length(Ca,N)),L),minimo(L,(Cam,NA)).
maiorNARCOSDP(Nodo,Cam,NA):- findall((Ca,N), (resolveDP(Ca), length(Ca,N)),L),maximo(L,(Cam,NA)).

%%-----------------------Depth First custo/distancia -----------------------


resolveDPC([Nodo|Caminho], Custo):-
    inicial(Nodo),
    depthFirstC(Nodo,[Nodo], Caminho, Custo).

depthFirstC(Nodo, _, [], 0):-
    final(Nodo).

depthFirstC(Nodo, Historico, [NodoProx|Caminho], Custo):-
    adjacente(Nodo, NodoProx, CustoArco),
    nao(membro(NodoProx, Historico)),
    depthFirstC(NodoProx, [NodoProx|Historico], Caminho, CustoAux),
    Custo is CustoArco + CustoAux.


melhor(Nodo,Cam,Custo):- findall((Ca,Cus), (resolveDPC(Ca,Cus)), L),minimo(L,(Cam,Custo)).
pior(Nodo,Cam,Custo):- findall((Ca,Cus), (resolveDPC(Ca,Cus)), L),maximo(L,(Cam,Custo)).


%----------------------------Depth First Produtividade ------------------------

resolveDPProdutividade([Nodo|Caminho], Custo):-
    inicial(Nodo),
    depthFirstP(Nodo,[Nodo], Caminho, Custo).

depthFirstP(Nodo, _, [], 0):-
    final(Nodo).

depthFirstP(Nodo, Historico, [NodoProx|Caminho], Custo):-
    adjacente(Nodo, NodoProx,_),
    calculaP(NodoProx, CustoArco),
    nao(membro(NodoProx, Historico)),
    depthFirstP(NodoProx, [NodoProx|Historico], Caminho, CustoAux),
    Custo is CustoArco + CustoAux.

totalLixo([],0).
totalLixo([X|T],Tx):- totalLixo(T,Ty), Tx is X + Ty.

calculaP(Nodo,Total) :- findall(C,pontos_recolha(_,_,Nodo,_,C,_),R),totalLixo(R,Total).

maisLixo(Nodo,Cam,Custo):- findall((Ca,Cus), (resolveDPProdutividade(Ca,Cus)), L),maximo(L,(Cam,Custo)).


%-----------------------Depth First Tipos ----------------------------

menorNArcosDPT(Tipo,Cam,NA):- findall((Ca,N), (resolveDPTipos(Ca,Tipo), length(Ca,N)),L),minimo(L,(Cam,NA)).
maiorNARCOSDPT(Tipo,Cam,NA):- findall((Ca,N), (resolveDPTipos(Ca,Tipo), length(Ca,N)),L),maximo(L,(Cam,NA)).



resolveDPTipos([Nodo|Caminho], Tipo):-
    inicial(Nodo),
    depthFirstT(Nodo,[Nodo],Caminho, Tipo).

depthFirstT(Nodo,_,[], Tipo):-
    final(Nodo).

depthFirstT(Nodo, Historico, [NodoProx|Caminho], Tipo):-
    getLixo(NodoProx, Tipo),
    adjacente(Nodo, NodoProx,_),
    nao(membro(NodoProx, Historico)),
    depthFirstT(NodoProx, [NodoProx|Historico], Caminho, Tipo).

resolveDPTiposAll(L,T):- findall((S,C), (resolveDPTipos(S, T), length(S,C)),L).





%%----------------------Depth first limitada -------------------------


menorNArcosDPlimitada(Limite,Cam,NA):- findall((Ca,N), (resolveDPlimitada(Ca,Limite), length(Ca,N)),L),minimo(L,(Cam,NA)).
maiorNArcosDPlimitada(Limite,Cam,NA):- findall((Ca,N), (resolveDPlimitada(Ca,Limite), length(Ca,N)),L),maximo(L,(Cam,NA)).

resolveDPlimitadaAll(L,Size):- findall((S,C), (resolveDPlimitada(S,Size), length(S,C)), L).

resolveDPlimitada(Solucao,L) :-
    inicial(No),
    depthFirstLimited([],No,Sol,L),
    reverseL(Sol,Solucao).

depthFirstLimited(Caminho,No,[No|Caminho],_) :-
    final(No),!.

depthFirstLimited(Caminho,No,S,L) :-
    L > 0,
    adjacente(No,No1,_),
    \+ membro(No1,Caminho),
    L1 is L - 1,
    depthFirstLimited([No|Caminho],No1,S,L1).



%%---------------------Depth first limitada custo ------------------


melhorDPlimitada(Limite,Cam,Custo):- findall((Ca,Cus), (resolveDPlimitadaC(Ca,Limite,Cus)),L),minimo(L,(Cam,Custo)).
piorDPlimitada(Limite,Cam,Custo):- findall((Ca,Cus), (resolveDPlimitadaC(Ca,Limite,Cus)),L),maximo(L,(Cam,Custo)).

resolveDPlimitadaCostAll(L,Size) :- findall((S,C,Cost), (resolveDPlimitadaC(S,Size,Cost), comprimento(S,C)), L).

resolveDPlimitadaC(Solucao,L,Custo) :-
    inicial(No),
    depthFirstLimitedC([],No,Sol,L,Custo),
    reverseL(Sol,Solucao).



depthFirstLimitedC(Caminho,No,[No|Caminho],L,0) :-
    final(No),!.

depthFirstLimitedC(Caminho,No,S,L, Custo) :-
    L > 0,
    adjacente(No,No1,CustoArco),
    \+ membro(No1,Caminho),
    L1 is L - 1,
    depthFirstLimitedC([No|Caminho],No1,S,L1, CustoAux),
    Custo is CustoArco + CustoAux.



%-----------------------Depth First Limitada Tipos ----------------------------



resolveDPlimitadaTipo(Solucao,L, Tipo) :-
    inicial(No),
    depthFirstLimitedT([],No,Sol,L,Tipo),
    reverseL(Sol,Solucao).

depthFirstLimitedT(Caminho,No,[No|Caminho],L, Tipo) :-
    final(No),!.

depthFirstLimitedT(Caminho,No,S,L,Tipo) :-
    L > 0,
    getLixo(No1, Tipo),
    adjacente(No,No1,_),
    \+ membro(No1,Caminho),
    L1 is L - 1,
    depthFirstLimitedT([No|Caminho],No1,S,L1,Tipo).




%%----------------Breadth First --------------------------------------
resolveBFSall(L) :- findall((S,C), (resolveBFS(S,C)), L).

%menorNodosBFS(Cam,NA):- findall((A,S,N), (resolveBFS(A,S), length(S,N)),L) ,minimo(L,(Cam,NA)).
%menorNArcosDP(Nodo,Cam,NA):- findall((Ca,N), (resolveDP(Ca), length(Ca,N)),L),minimo(L,(Cam,NA)).

resolveBFS(No,Solucao) :-
    inicial(No),
    breadthFirst([[No]],Sol),
    reverseL(Sol,Solucao).


breadthFirst([[No|Caminho]|_],[No|Caminho]) :-
    final(No).

breadthFirst([Caminho|Caminhos],Solucao) :-
    estender(Caminho,NovosCaminhos),
    append(Caminhos,NovosCaminhos,Caminhos1),
    breadthFirst(Caminhos1,Solucao).

estender([No|Caminho],NovosCaminhos) :-
    findall([NovoNo,No|Caminho],
        (adjacente(No,NovoNo,_), \+ membro(NovoNo,[No|Caminho])),
        NovosCaminhos).





%%----------------Breadth First Custos --------------------------------------

melhorBFS(Nodo,Cam,Custo):- findall((Ca,Cus), (resolveBFSC(Ca,Cus)), L),minimo(L,(Cam,Custo)).


resolveBFSCustosAll(L):- findall((S,C), (resolveBFSC(S,C)),L).

resolveBFSC(Solucao, Custo) :-
    inicial(No),
    resolveBFSC2([[No]],Sol),
    reverseL(Sol,Solucao),
    custoTotal(Solucao, Custo). 

resolveBFSC2([[No|Caminho]|_],[No|Caminho]) :-
    final(No).

resolveBFSC2([Caminho|Caminhos],Solucao) :-
    estender(Caminho,NovosCaminhos),
    append(Caminhos,NovosCaminhos,Caminhos1),
    resolveBFSC2(Caminhos1,Solucao).


custoTotal([],0).
custoTotal([No],0).
custoTotal([No1,No2|Caminho],Custo) :-
    adjacente(No1,No2,CustoArco),
    custoTotal([No2|Caminho],CustoResto),
    Custo is CustoArco + CustoResto.



%%------------------- Procura informada ---------------------------------

%%------------------Procura gulosa -------------------------------


resolveGulosa(Nodo, Caminho/Custo) :-
    estimativa(Nodo, Estimativa),
    agulosa([[Nodo]/0/Estimativa], CaminhoInverso/Custo/_),
    reverseL(CaminhoInverso, Caminho).

agulosa(Caminhos, Caminho) :-
    obtem_melhor_g(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,final(Nodo).

agulosa(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expandeGulosa(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    agulosa(NovoCaminhos, SolucaoCaminho).

obtem_melhor_g([Caminho], Caminho) :- !.

obtem_melhor_g([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-
	Est1 =< Est2, !,
	obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).

obtem_melhor_g([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_g(Caminhos, MelhorCaminho).

expandeGulosa(Caminho, ExpCaminhos) :-
	findall(NovoCaminho, adjacenteG(Caminho,NovoCaminho), ExpCaminhos).

adjacenteG([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
	adjacente(Nodo, ProxNodo, PassoCusto),
    nao(membro(ProxNodo, Caminho)),
	NovoCusto is Custo + PassoCusto,
	estimativa(ProxNodo, Est).



%%-------------------------Gulosa Tipos----------------------------------

resolveGulosaT(Nodo,Caminho/Custo,Tipo):-
    estimativa(Nodo,Estimativa),
    agulosaT([[Nodo]/0/Estimativa],CaminhoInverso/Custo/_,Tipo),
    reverse(CaminhoInverso,Caminho).

agulosaT(Caminhos,Caminho,Tipo):-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,final(Nodo).

agulosaT(Caminhos,SolucaoCaminho,Tipo):-
    obtem_melhor_g(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expandeGulosaT(MelhorCaminho,ExpCaminhos,Tipo),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaT(NovoCaminhos,SolucaoCaminho,Tipo).

expandeGulosaT(Caminho,ExpCaminhos,Tipo):-
    findall(NovoCaminho,adjacenteGT(Caminho,NovoCaminho,Tipo),ExpCaminhos).

adjacenteGT([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/Est,Tipo):-
    getLixo(NodoProx,Tipo),
    adjacente(Nodo,ProxNodo,PassoCusto),
    nao(membro(ProxNodo,Caminho)),
    NovoCusto is Custo+PassoCusto,
    estimativa(ProxNodo,Est).





%---------------------------A* -------------------------------------------

resolveAEstrela(Caminho/Custo) :-
    inicial(Nodo),
    pontos_recolha(_,_,Nodo,_,Cap,_),
    aestrela([[Nodo]/0/pontos_recolha], CaminhoInverso/Custo/_),
    reverseL(CaminhoInverso, Caminho).

aestrela(Caminhos, Caminho) :-
    obtem_melhor(Caminhos, Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
    obtem_melhor(Caminhos, MelhorCaminho),
    seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
    expandeAEstrela(MelhorCaminho, ExpCaminhos),
    append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrela(NovoCaminhos, SolucaoCaminho).

obtem_melhor([Caminho], Caminho) :- !.

obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos], MelhorCaminho) :-

    Custo1 + Est1 =< Custo2 + Est2, !,
    obtem_melhor([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
    
obtem_melhor([_|Caminhos], MelhorCaminho) :- 
    obtem_melhor(Caminhos, MelhorCaminho).
    
expandeAEstrela(Caminho, ExpCaminhos) :-
findall(NovoCaminho, adjacenteG(Caminho,NovoCaminho), ExpCaminhos).

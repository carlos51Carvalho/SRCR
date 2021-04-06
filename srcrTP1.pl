% ----------------------------------------------------------------------
% PROLOG: Declaracoes iniciais
% ----------------------------------------------------------------------

:- style_check(-discontiguous).
:- style_check(-singleton).

% ----------------------------------------------------------------------
% PROLOG: definicoes iniciais
% ----------------------------------------------------------------------

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic utente/10.
:- dynamic centro_saude/5.
:- dynamic staff/4.
:- dynamic vacinacao_covid/5.

% ----------------------------------------------------------------------
% Fases de Vacinação
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Primeira Fase  
% A partir de dezembro de 2020:
%       Profissionais de saúde envolvidos na prestação de cuidados a doentes
%       Profissionais das forças armadas, forças de segurança e serviços críticos
%       Profissionais e residentes em Estruturas Residenciais para Pessoas Idosas (ERPI) e instituições similares
%       Profissionais e utentes da Rede Nacional de Cuidados Continuados Integrados (RNCCI).
% A partir de fevereiro de 2021:
%       Pessoas de idade ≥50 anos, com pelo menos uma das seguintes patologias:
%           Insuficiência cardíaca
%           Doença coronária
%           Insuficiência renal (Taxa de Filtração Glomerular < 60ml/min)
%           (DPOC) ou doença respiratória crónica sob suporte ventilatório e/ou oxigenoterapia de longa duração
%       Pessoas com 80 ou mais anos de idade.
% ----------------------------------------------------------------------

primeira_fase(R):- append(morethan80(X),morethan50_diseased(Y),L),append(L,profissional(Z),R).


doenca_grave([]):-!,fail.
doenca_grave([Insuficiencia_cardiaca|_]).
doenca_grave([Insuficiencia_renal|_]).
doenca_grave([Doenca_respiratorio|_]).
doenca_grave([H|T]):- doenca_grave(T).


profissional_prioritario("enfermeiro").
profissional_prioritario("medico").
profissional_prioritario("militar").


profissional(R):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,P,_,_),profissional_prioritario(P)),R).

morethan50_diseased(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),doenca_grave(LDC),K is 2021, K-A >= 50),R),remove_dups(R,RS).

morethan80(R):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),K is 2021, K-A >= 80),R).

% ----------------------------------------------------------------------
% Utente: Idutente,NºSegSocial, Nome, Data_Nasc, Email, Telefone, Morada,
%           Profissão, [Doenças_Crónicas], IdCentroSaúde -> {V,F}
% ----------------------------------------------------------------------

utente(1,123123123,"luis",02-02-2000,"teste@gmail.com",911222333,"braga",
	"estudante",[cego|coxo],1).

utente(2,123123124,"joao",02-02-2000,"teste2@gmail.com",911222334,"braga",
	"estudante",[Insuficiencia_cardiaca],1).

utente(3,124123124,"manuel",02-02-1930,"teste3@gmail.com",911232334,"braga",
	"velho",[Insuficiencia_cardiaca| Insuficiencia_renal],1).

utente(4,124123125,"xavier",02-02-1930,"testePri@gmail.com",911232355,"alentejo",
	"enfermeiro",[],1).

registaUtente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS):- evolucao(utente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS)).

removeUtente(ID):- involucao(utente(ID,_,_,_,_,_,_,_,_,_)).

% ----------------------------------------------------------------------
% centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}
% ----------------------------------------------------------------------

centro_saude(1,bragaHospital,braga,966777888,"braga@hospital.com").

registaCentroSaude(ID,N,M,T,E):- evolucao(centro_saude(ID,N,M,T,E)).

removeCentroSaude(ID):- involucao(centro_saude(ID,_,_,_,_)).

% ----------------------------------------------------------------------
% staff: Idstaff, Idcentro, Nome, email -> {V,F}
% ----------------------------------------------------------------------

staff(1,1,enfermeiro1,"enfermeiro1@hospital.com").

registaStaff(IDS,IDC,N,E):- evolucao(staff(IDS,IDC,N,E)).

removeStaff(ID):- involucao(staff(ID,_,_,_)).

% ----------------------------------------------------------------------
% vacinação_Covid: #Staff, #utente, Data, Vacina, Toma -> {V,F}
% ----------------------------------------------------------------------


vacinacao_covid(1,1,23-03-2021,pfizer,2).

registaVacinacaoCovid(IDS,IDU,D,V,T):- evolucao(vacinacao_covid(IDS,IDU,D,V,T)).

removeVacinacaoCovid(IDS,IDU):- involucao(vacinacao_covid(IDS,IDU,_,_,_)).

% ----------------------------------------------------------------------
% falta_segunda_vacina: Lista com IDs e nomes de utentes a quem falta 2a vacina
% ----------------------------------------------------------------------

falta_segunda_vacina(R):-solucoes( (IDU,N),
		  (utente(IDU,_,N,_,_,_,_,_,_,_),vacinacao_covid(_,IDU,_,_,1),nao(vacinacao_covid(_,IDU,_,_,2))), R ).


% ----------------------------------------------------------------------
% nao_vacinado: Lista com IDs e nomes de utentes não vacinados
% ----------------------------------------------------------------------

nao_vacinado(R):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),nao(vacinacao_covid(_,ID,_,_,_))),R).

% ----------------------------------------------------------------------
% vacinado: Lista com IDs e nomes de utentes vacinados completamente
% ----------------------------------------------------------------------

vacinado(R):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),vacinacao_covid(_,ID,_,_,2)),R).

% ----------------------------------------------------------------------
% Invariantes de inserção
% ----------------------------------------------------------------------


+utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS ) :: (solucoes( ID,
		  (utente( ID,_,_,_,_,_,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+staff( IDS,IDC,N,E ) :: (solucoes( IDS,
		  (staff( IDS,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+centro_saude( IDC,N,M,T,E ) :: (solucoes( IDC,
		  (centro_saude( IDC,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+vacinacao_covid( IDS,IDU,D,V,1 ) :: (solucoes( IDU,
		  (vacinacao_covid( _,IDU,_,_,1 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+vacinacao_covid( IDS,IDU,D,V,2 ) :: (solucoes( IDU,
		  (vacinacao_covid( _,IDU,_,_,2 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% ----------------------------------------------------------------------
% Invariantes de remoção
% ----------------------------------------------------------------------

-utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS ) :: (solucoes( ID,
		  (utente( ID,_,_,_,_,_,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-staff( IDS,IDC,N,E ) :: (solucoes( IDS,
		  (staff( IDS,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-centro_saude( IDC,N,M,T,E ) :: (solucoes( IDC,
		  (centro_saude( IDC,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-vacinacao_covid( IDS,IDU,D,V,1 ) :: (solucoes( IDU,
		  (vacinacao_covid( _,IDU,_,_,1 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-vacinacao_covid( IDS,IDU,D,V,2 ) :: (solucoes( IDU,
		  (vacinacao_covid( _,IDU,_,_,2 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% ----------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}
% ----------------------------------------------------------------------

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% ----------------------------------------------------------------------
% Extensao do predicado que permite a evolução do conhecimento
% ----------------------------------------------------------------------

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

% ----------------------------------------------------------------------
% Extensão do predicado que permite a involucao do conhecimento
% ----------------------------------------------------------------------

involucao(Termo):- solucoes(Invariante,-Termo::Invariante,Lista),
		   remocao(Termo),
                   teste(Lista).

remocao(Termo):- retract(Termo).
remocao(Termo):- assert(Termo),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

% ----------------------------------------------------------------------
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }
% ----------------------------------------------------------------------

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.

% ----------------------------------------------------------------------

remove_dups([], []).
remove_dups([First | Rest], NewRest) :- member(First, Rest), remove_dups(Rest, NewRest).
remove_dups([First | Rest], [First | NewRest]) :- nao(member(First, Rest)), remove_dups(Rest, NewRest).


pertence(X,[X|_]).
pertence(X,[H|T]):- X \= H, pertence(X,T).

adicionar(X,L,L):-pertence(X,L).
adicionar(X,L,[X|L]).

concatenar(T,[],T).
concatenar([],T,T).
concatenar([H|T],L,F):- adicionar(H,N,F), concatenar(T,L,N).
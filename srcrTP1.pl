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

primeira_fase().
segunda_fase().
terceira_fase().

% ----------------------------------------------------------------------
% Utente: Idutente,NºSegSocial, Nome, Data_Nasc, Email, Telefone, Morada,
%           Profissão, [Doenças_Crónicas], IdCentroSaúde -> {V,F}
% ----------------------------------------------------------------------

utente(1,123123123,"luis","02-02-2000","teste@gmail.com",911222333,"braga",
	"estudante",[cego|coxo],1).

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


vacinacao_covid(1,1,23-03-2021,pfizer,1).

registaVacinacaoCovid(IDS,IDU,D,V,T):- evolucao(vacinacao_covid(IDS,IDU,D,V,T)).

removeVacinacaoCovid(IDS,IDU):- involucao(vacinacao_covid(IDS,IDU,_,_,_)).

% ----------------------------------------------------------------------
% falta_segunda_vacina: Lista com IDs utentes a quem falta 2a vacina
% ----------------------------------------------------------------------

falta_segunda_vacina(R):-solucoes( ( _,IDU,_,V,1 ),
		  (vacinacao_covid( IDS,IDU,D,V,1)), R ),R.


% ----------------------------------------------------------------------
% Invariantes de inserção
% ----------------------------------------------------------------------


+utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS ) :: (solucoes( (ID,NSS,N,DN,E,T,M,P,LDC,IDCS),
		  (utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+staff( IDS,IDC,N,E ) :: (solucoes( (IDS,IDC,N,E),
		  (staff( IDS,IDC,N,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+centro_saude( IDC,N,M,T,E ) :: (solucoes( (IDC,N,M,T,E),
		  (centro_saude( IDC,N,M,T,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+vacinacao_covid( IDS,IDU,D,V,1 ) :: (solucoes( (IDS,IDU,D,V,1),
		  (vacinacao_covid( IDS,IDU,D,V,1 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

+vacinacao_covid( IDS,IDU,D,V,2 ) :: (solucoes( (IDS,IDU,D,V,2),
		  (vacinacao_covid( IDS,IDU,D,V,2 )), S ),
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

-vacinacao_covid( IDS,IDU,D,V,1 ) :: (solucoes( (IDS,IDU,D,V,1),
		  (vacinacao_covid( IDS,IDU,D,V,1 )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-vacinacao_covid( IDS,IDU,D,V,2 ) :: (solucoes( (IDS,IDU,D,V,2),
		  (vacinacao_covid( IDS,IDU,D,V,2 )), S ),
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
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% ----------------------------------------------------------------------
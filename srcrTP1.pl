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

primeira_fase(RS):- morethan80(X),morethan50_diseased(Y),concatenar(X,Y,L),profissional(Z),concatenar(L,Z,R),remove_dups(R,RS),!.


doenca_1afase([]):-!,fail.
doenca_1afase([Insuficiencia_cardiaca|_]).
doenca_1afase([Insuficiencia_renal|_]).
doenca_1afase([Doenca_respiratoria|_]).
doenca_1afase([H|T]):- doenca_1afase(T).


profissional_prioritario("enfermeiro").
profissional_prioritario("enfermeira").
profissional_prioritario("medico").
profissional_prioritario("medica").
profissional_prioritario("militar").


profissional(R):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,P,_,_),profissional_prioritario(P)),R).

morethan50_diseased(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),doenca_1afase(LDC),K is 2021, K-A >= 50),R),remove_dups(R,RS).

morethan80(R):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),K is 2021, K-A >= 80),R).

% ----------------------------------------------------------------------
% Segunda Fase
%      Pessoas de idade ≥65 anos (que não tenham sido vacinadas previamente)
%      Pessoas entre os 50 e os 64 anos de idade, inclusive, com pelo menos uma das seguintes patologias:
%           Diabetes
%           Neoplasia maligna ativa
%           Doença renal crónica (Taxa de Filtração Glomerular > 60ml/min)
%           Insuficiência hepática
%           Hipertensão arterial
%           Obesidade
% ----------------------------------------------------------------------

segunda_fase(RS):- morethan65_notvacinated(X),between50and64_diseaded(Y),concatenar(X,Y,R),remove_dups(R,RS),!.

doenca_2afase([]):-!,fail.
doenca_2afase([Diabetes|_]).
doenca_2afase([Neoplasia_maligna|_]).
doenca_2afase([Doenca_renal_cronica|_]).
doenca_2afase([Insuficiencia_hepatica|_]).
doenca_2afase([Hipertensao_arterial|_]).
doenca_2afase([Obesidade|_]).
doenca_2afase([H|T]):- doenca_2afase(T).

between50and64_diseaded(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),doenca_2afase(LDC),between50and64(A)),R),remove_dups(R,RS).

between50and64(A):- K is 2021, K-A > 50, K-A =< 64.

morethan65_notvacinated(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,_,_),nao_vacinado(X),pertence((ID,N),X),K is 2021, K-A >= 65),R),remove_dups(R,RS).


% ----------------------------------------------------------------------
% Utente: Idutente,NºSegSocial, Nome, Data_Nasc, Email, Telefone, Morada,
%           Profissão, [Doenças_Crónicas], IdCentroSaúde -> {V,F}
% ----------------------------------------------------------------------

utente(1,123123123,"luis",02-02-2000,"teste@gmail.com",911222333,"braga",
	"estudante",[cego|coxo],1).

utente(2,123123124,"joao",02-02-2000,"teste2@gmail.com",911222334,"braga",
	"estudante",[Insuficiencia_renal],1).

utente(3,124123124,"manuel",02-02-1969,"teste3@gmail.com",911232334,"braga",
	"velho",[Insuficiencia_cardiaca, Insuficiencia_renal, Insuficiencia_hepatica],1).

utente(4,124123125,"xavier",02-02-1930,"testePri@gmail.com",911232355,"alentejo",
	"enfermeiro",[],1).

showAllUtentes():- listing(utente).

registaUtente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS):- evolucao(utente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS)).

removeUtente(ID):- involucao(utente(ID,_,_,_,_,_,_,_,_,_)).

removeUtente(NSS):- involucao(utente(_,NSS,_,_,_,_,_,_,_,_)).

% ----------------------------------------------------------------------
% centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}
% ----------------------------------------------------------------------

centro_saude(1,bragaHospital,braga,966777888,"braga@hospital.com").

showAllCentroSaude():- listing(centro_saude).

registaCentroSaude(ID,N,M,T,E):- evolucao(centro_saude(ID,N,M,T,E)).

removeCentroSaude(ID):- involucao(centro_saude(ID,_,_,_,_)).

% ----------------------------------------------------------------------
% staff: Idstaff, Idcentro, Nome, email -> {V,F}
% ----------------------------------------------------------------------

staff(1,1,enfermeiro1,"enfermeiro1@hospital.com").

showAllStaff():- listing(staff).

registaStaff(IDS,IDC,N,E):- evolucao(staff(IDS,IDC,N,E)).

removeStaff(ID):- involucao(staff(ID,_,_,_)).

% ----------------------------------------------------------------------
% vacinação_Covid: #Staff, #utente, Data, Vacina, Toma -> {V,F}
%                   pfizer, astrazeneca
% ----------------------------------------------------------------------


vacinacao_covid(1,1,23-03-2021,pfizer,1).
vacinacao_covid(1,1,31-03-2021,pfizer,2).
vacinacao_covid(1,2,07-04-2021,pfizer,1).

showAllVacinacao():- listing(vacinacao_covid).

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
% vacinado_indevidamente: Lista com IDs e nomes de utentes vacinados indevidamente
% ----------------------------------------------------------------------

vacinado_indevidamente(RS) :- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),
                                vacinado_indevidamente_1afase(X),
                                vacinado_indevidamente_2afase(Z),concatenar(X,Z,R),pertence((ID,N),R)),R),
                                remove_dups(R,RS).

vacinado_indevidamente_1afase(RS):- solucoes((ID,N),
                                    (utente(ID,_,N,_,_,_,_,_,_,_),
                                    vacinado(X),pertence((ID,N),X),
                                    nao(check_1afase((ID,N)))),R),
                                    remove_dups(R,RS).

vacinado_indevidamente_2afase(RS):- solucoes((ID,N),
                                    (utente(ID,_,N,_,_,_,_,_,_,_),
                                    vacinado(X),pertence((ID,N),X),
                                    nao(check_2afase((ID,N)))),R),
                                    remove_dups(R,RS).

check_1afase((ID,N)):- primeira_fase(Y),pertence((ID,N),Y).

check_2afase((ID,N)):- segunda_fase(Y),pertence((ID,N),Y).

% ----------------------------------------------------------------------
% candidato_nao_vacinado: Lista com IDs e nomes de utentes candidatos que ainda não foram vacinados
% ----------------------------------------------------------------------

candidato_nao_vacinado(RS):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),
                                nao_vacinado(X),pertence((ID,N),X),
                                candidato(Z),pertence((ID,N),Z)),R),
                                remove_dups(R,RS).

candidato(RS):- primeira_fase(X),segunda_fase(Y),concatenar(X,Y,RS).

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
%                            Resposta = { verdadeiro,falso }
% ----------------------------------------------------------------------

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.

% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Extensao do predicado remove_dups: ListaInicial, ListaFinal 
% ----------------------------------------------------------------------

remove_dups([], []).
remove_dups([First | Rest], NewRest) :- member(First, Rest), remove_dups(Rest, NewRest).
remove_dups([First | Rest], [First | NewRest]) :- nao(member(First, Rest)), remove_dups(Rest, NewRest).


% ----------------------------------------------------------------------
% Extensao do predicado concatenar: Lista1, Lista2, ListaFinal 
% ----------------------------------------------------------------------

pertence(X,[X|_]).
pertence(X,[H|T]):- X \= H, pertence(X,T).

adicionar(X,L,L):-pertence(X,L).
adicionar(X,L,[X|L]).

concatenar(T,[],T).
concatenar([],T,T).
concatenar([H|T],L,F):- adicionar(H,N,F), concatenar(T,L,N).


intersection(A,B,AnB):- subtract(A,B,AminusB),subtract(A,AminusB,K),sort(K,AnB).
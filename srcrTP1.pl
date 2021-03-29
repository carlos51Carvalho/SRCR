% PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% ----------------------------------------------------------------------
% PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.

% ----------------------------------------------------------------------
% Utente: Idutente,NºSegSocial, Nome, Data_Nasc, Email, Telefone, Morada,
%           Profissão, [Doenças_Crónicas], IdCentroSaúde -> {V,F}

utente(1,123123123,luis,"02-02-2000","teste@gmail.com",911222333,braga,
	estudante,[cego|coxo],1).

% ----------------------------------------------------------------------
% centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}

centro_saude(1,bragaHospital,braga,966777888,"braga@hospital.com").

% ----------------------------------------------------------------------
% staff: Idstaff, Idcentro, Nome, email -> {V,F}

staff(1,1,enfermeiro1,"enfermeiro1@hospital.com").

% ----------------------------------------------------------------------
% vacinação_Covid: #Staf, #utente, Data, Vacina, Toma -> {V,F}


vacinacao_covid(1,1,23-03-2021,pfizer,1).


% ----------------------------------------------------------------------
% falta_segunda_vacina: Lista com IDs utentes a quem falta 2a vacina

falta_segunda_vacina(R).

% ----------------------------------------------------------------------
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% Extensao do predicado que permite a evolução do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% ----------------------------------------------------------------------
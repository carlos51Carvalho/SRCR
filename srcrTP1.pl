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

% ----------------------------------------------------------------------
% Extensão do predicado primeira_fase que averigua os utentes candidatos à primeira fase
% ----------------------------------------------------------------------

primeira_fase(RS):- morethan80(X),morethan50_diseased(Y),concatenar(X,Y,L),profissional(Z),concatenar(L,Z,R),remove_dups(R,RS),!.

% ----------------------------------------------------------------------
% Extensão do predicado doenca_1afase que averigua se uma doença de um utente
% é uma das referidas nos critérios da primeira fase
% ----------------------------------------------------------------------

doenca_1afase([]):-!,fail.
doenca_1afase(["Insuficiencia_cardiaca"|_]).
doenca_1afase(["Insuficiencia_renal"|_]).
doenca_1afase(["Doenca_respiratoria"|_]).
doenca_1afase([H|T]):- doenca_1afase(T).

% ----------------------------------------------------------------------
% Extensão do predicado profissional_prioritario que averigua se a profissao 
% de um utente é uma das referidas nos critérios da primeira fase
% ----------------------------------------------------------------------

profissional_prioritario("enfermeiro").
profissional_prioritario("enfermeira").
profissional_prioritario("medico").
profissional_prioritario("medica").
profissional_prioritario("militar").


% ----------------------------------------------------------------------
% Extensão do predicado profissional que averigua os utentes candidatos à primeira fase
% de acordo com a sua profissão
% ----------------------------------------------------------------------

profissional(R):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,P,_,_),profissional_prioritario(P)),R).

% ----------------------------------------------------------------------
% Extensão do predicado morethan50_diseased que averigua os utentes candidatos à primeira fase
% de acordo com a sua idade e a lista de doenças crónicas
% ----------------------------------------------------------------------

morethan50_diseased(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),doenca_1afase(LDC),K is 2021, K-A >= 50),R),remove_dups(R,RS),!.

% ----------------------------------------------------------------------
% Extensão do predicado morethan80 que averigua os utentes candidatos à primeira fase
% de acordo com a sua idade
% ----------------------------------------------------------------------

morethan80(R):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),K is 2021, K-A >= 80),R).

% ----------------------------------------------------------------------
% Segunda Fase
% A partir de abril de 2021:
%      Pessoas de idade ≥65 anos (que não tenham sido vacinadas previamente)
%      Pessoas entre os 50 e os 64 anos de idade, inclusive, com pelo menos uma das seguintes patologias:
%           Diabetes
%           Neoplasia maligna ativa
%           Doença renal crónica (Taxa de Filtração Glomerular > 60ml/min)
%           Insuficiência hepática
%           Hipertensão arterial
%           Obesidade
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Extensão do predicado segunda_fase que averigua os utentes candidatos à segunda fase
% ----------------------------------------------------------------------

segunda_fase(RS):- morethan65_notvacinated(X),between50and64_diseaded(Y),concatenar(X,Y,R),remove_dups(R,RS),!.

% ----------------------------------------------------------------------
% Extensão do predicado doenca_2afase que averigua se uma doença de um utente
% é uma das referidas nos critérios da segunda fase
% ----------------------------------------------------------------------

doenca_2afase([]):-!,fail.
doenca_2afase(["Diabetes"|_]).
doenca_2afase(["Neoplasia_maligna"|_]).
doenca_2afase(["Doenca_renal_cronica"|_]).
doenca_2afase(["Insuficiencia_hepatica"|_]).
doenca_2afase(["Hipertensao_arterial"|_]).
doenca_2afase(["Obesidade"|_]).
doenca_2afase([H|T]):- doenca_2afase(T).

% ----------------------------------------------------------------------
% Extensão do predicado between50and64_diseaded que averigua os utentes candidatos à primeira fase
% de acordo com a sua idade e a lista de doenças crónicas
% ----------------------------------------------------------------------

between50and64_diseaded(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,LDC,_),doenca_2afase(LDC),between50and64(A)),R),remove_dups(R,RS),!.

% ----------------------------------------------------------------------
% Extensão do predicado between50and64 que averigua se a idade de um utente
% se encontra entre os 50 e os 64
% ----------------------------------------------------------------------

between50and64(A):- K is 2021, K-A > 50, K-A =< 64.

% ----------------------------------------------------------------------
% Extensão do predicado morethan65_notvacinated que averigua os utentes candidatos à segunda fase
% que têm mais de 65 anos e ainda não foram vacinados
% ----------------------------------------------------------------------

morethan65_notvacinated(RS):- solucoes((ID,N),(utente(ID,_,N,D-M-A,_,_,_,_,_,_),nao_vacinado(X),pertence((ID,N),X),K is 2021, K-A >= 65),R),remove_dups(R,RS).

% ----------------------------------------------------------------------
% Terceira Fase  
%   Todos os outros
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Extensão do predicado terceira_fase que averigua os utentes candidatos à terceira fase
% ----------------------------------------------------------------------

terceira_fase(RS) :- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),primeira_fase(X),nao(pertence((ID,N),X)),segunda_fase(Y),nao(pertence((ID,N),Y))),R),remove_dups(R,RS).

% ----------------------------------------------------------------------
% Utente: Idutente,NºSegSocial, Nome, Data_Nasc, Email, Telefone, Morada,
%           Profissão, [Doenças_Crónicas], IdCentroSaúde -> {V,F}
% ----------------------------------------------------------------------

% Vacinado indevidamente

utente(1,123123123,"Luis",02-02-2000,"luis@gmail.com",911222333,"braga","estudante",[cego,coxo],1).
utente(5,123456789,"Marta",02-02-1993,"marta@gmail.com",912345677,"algarve","esteticista",[],3).

    % Não pertence a nenhuma das fases
utente(2,123123124,"Joao",24-04-2000,"joao@gmail.com",911222334,"braga","estudante",["Insuficiencia_renal"],2).

% Pertence a ambas as fases
utente(3,124123124,"Manuel",02-02-1969,"manuel@gmail.com",911232334,"braga","arquiteto",["Insuficiencia_cardiaca", "Insuficiencia_renal", "Insuficiencia_hepatica"],1).

    % Pertence a ambas as fases - candidato
utente(4,124123125,"Xavier",02-02-1930,"xavier@gmail.com",911232355,"alentejo","enfermeiro",[],3).

% Pertence a 2a fase - candidato
utente(6,124123129,"Oliver",02-02-1958,"oliver@gmail.com",911232634,"braga","professor",["Insuficiencia_hepatica"],2).

% ----------------------------------------------------------------------
% Extensão do predicado showAllUtentes 
%                       que apresenta ao utilizador todos os utentes
% ----------------------------------------------------------------------

showAllUtentes():- listing(utente).

% ----------------------------------------------------------------------
% Extensão do predicado registaUtente 
%                       que permite o registo de um utente
% ----------------------------------------------------------------------

registaUtente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS):- evolucao(utente(ID,NSS,N,DT,E,T,M,P,LDC,IDCS)).

% ----------------------------------------------------------------------
% Extensão do predicado removeUtenteID
%                       que permite a remoção de um utente pelo seu ID
% ----------------------------------------------------------------------

removeUtenteID(ID):- involucao(utente(ID,_,_,_,_,_,_,_,_,_)).

% ----------------------------------------------------------------------
% Extensão do predicado removeUtenteNSS
% que permite a remoção de um utente pelo seu numero de segurança social
% ----------------------------------------------------------------------

removeUtenteNSS(NSS):- involucao(utente(_,NSS,_,_,_,_,_,_,_,_)).

% ----------------------------------------------------------------------
% Extensão do predicado removeUtenteAllInfoByID
% que permite a remoção de toda a informação sobre um utente pelo seu ID
% ----------------------------------------------------------------------

removeUtenteAllInfoByID(ID):- solucoes(ID,(vacinacao_covid(_,ID,_,_,_)),R), removeInfoVacinacao(R), removeUtenteID(ID).

% ----------------------------------------------------------------------
% Extensão do predicado removeInfoVacinacao
% que permite a remoção das vacinações de um utente pelo seu ID
% ----------------------------------------------------------------------

removeInfoVacinacao([]).
removeInfoVacinacao([IDU|Tail]):- removeVacinacaoCovid(IDU),removeInfoVacinacao(Tail).

% ----------------------------------------------------------------------
% Extensão do predicado numVacinasUtentes
% que calcula o número de vacinas tomadas por um utente
% ----------------------------------------------------------------------

numVacinasUtentes(ID,T):- solucoes(ID,(vacinacao_covid(_,ID,_,_,_)),R), comprimento(R,T).

% ----------------------------------------------------------------------
% centro_saúde: Idcentro, Nome, Morada, Telefone, Email -> {V,F}
% ----------------------------------------------------------------------

centro_saude(1,"Hospital de Braga","Braga",253027000,"hospital_braga@sns.pt").
centro_saude(2,"Hospital da Trofa Braga Norte","Braga",253027002,"hospital_trofa_braga@sns.pt").
centro_saude(3,"Hospital de São João","Porto",225512100,"hospital_sao_joao@sns.pt").

% ---------------------------------------------------------------------------
% Extensão do predicado showAllCentroSaude 
%                       que apresenta ao utilizador todos os centros de saude
% ---------------------------------------------------------------------------

showAllCentroSaude():- listing(centro_saude).

% ----------------------------------------------------------------------
% Extensão do predicado registaCentroSaude 
%                       que permite o registo de um centro de saude
% ----------------------------------------------------------------------

registaCentroSaude(IDCS,N,M,T,E):- evolucao(centro_saude(IDCS,N,M,T,E)).

% ----------------------------------------------------------------------
% Extensão do predicado removeCentroSaudeID
%                que permite a remoção de um centro de saude pelo seu ID
% ----------------------------------------------------------------------

removeCentroSaudeID(IDCS):- involucao(centro_saude(IDCS,_,_,_,_)).

% ----------------------------------------------------------------------
% Extensão do predicado removeCentroSaudeAndStaff
% que permite a remoção de um centro de saude pelo seu ID e o staff associado
% ----------------------------------------------------------------------

removeCentroSaudeAndStaff(IDCS):- solucoes(IDS,(staff(IDS,IDCS,_,_)),R), removeStaffAtCentroSaude(R), involucao(centro_saude(IDCS,_,_,_,_)).

removeStaffAtCentroSaude([]).
removeStaffAtCentroSaude([IDS|T]):- involucao(staff(IDS,_,_,_)), removeStaffAtCentroSaude(T).

% ----------------------------------------------------------------------
% Extensão do predicado showStaffAtCentroSaude
% que apresenta uma lista de staff que está designado para um dado centro de saúde 
% ----------------------------------------------------------------------

showStaffAtCentroSaude(IDCS,RS):- solucoes((IDS,N),(staff(IDS,IDCS,N,_)),R),remove_dups(R,RS).

% ----------------------------------------------------------------------
% staff: Idstaff, Idcentro, Nome, email -> {V,F}
% ----------------------------------------------------------------------

staff(1,1,"Jorge","jorge_andrade@gmail.com").
staff(2,2,"Liliana","liliana_albernaz@gmail.com").
staff(3,3,"Andre","martinsdr@gmail.com").
staff(4,1,"Antonio","antonioalves@gmail.com").
staff(5,2,"Ruben","rubenpteixeira@gmail.com").
staff(6,3,"Julian","julianaribeiro@gmail.com").

% ----------------------------------------------------------------------
% Extensão do predicado showAllStaff 
%                       que apresenta ao utilizador todo o staff
% ----------------------------------------------------------------------

showAllStaff():- listing(staff).

% ----------------------------------------------------------------------
% Extensão do predicado registaStaff 
%                       que permite o registo de um staff
% ----------------------------------------------------------------------

registaStaff(IDS,IDC,N,E):- evolucao(staff(IDS,IDC,N,E)).

% ----------------------------------------------------------------------
% Extensão do predicado removeStaffID
%                       que permite a remoção de um staff pelo seu ID
% ----------------------------------------------------------------------

removeStaffID(ID):- involucao(staff(ID,_,_,_)).

% ----------------------------------------------------------------------
% Extensão do predicado showVacinacaoDoneByStaff
% que apresenta uma lista de vacinações feitas por um determinado staff 
% ----------------------------------------------------------------------

showVacinacaoDoneByStaff(IDS,RS):- solucoes((vacinacao_covid(IDS,IDU,DT,V,T)),(staff(IDS,_,_,_),vacinacao_covid(IDS,IDU,DT,V,T)),R),remove_dups(R,RS).

% ----------------------------------------------------------------------
% vacinação_Covid: #Staff, #utente, Data, Vacina, Toma -> {V,F}
%                   pfizer, astrazeneca
% ----------------------------------------------------------------------

% Vacinacao invalida - nao pertence a nenhuma das fase

vacinacao_covid(1,1,23-03-2021,pfizer,1).
vacinacao_covid(1,1,31-03-2021,pfizer,2).

vacinacao_covid(3,5,23-03-2021,pfizer,1).

vacinacao_covid(3,2,12-12-2020,pfizer,1).

% Vacinacao valida - pertence a ambas as fases

vacinacao_covid(2,3,07-02-2021,astrazeneca,1).

% Vacinacao valida - pertence a primeira fase

vacinacao_covid(3,2,12-2-2021,pfizer,2).

% Vacinacao valida - pertence a segunda fase


% ----------------------------------------------------------------------
% Extensão do predicado showAllVacinacao 
%                       que apresenta ao utilizador todas as vacinacoes
% ----------------------------------------------------------------------

showAllVacinacao():- listing(vacinacao_covid).

% ----------------------------------------------------------------------
% Extensão do predicado registaVacinacaoCovid 
%                       que permite o registo de uma vacinação feita
% ----------------------------------------------------------------------

registaVacinacaoCovid(IDS,IDU,D,V,T):- evolucao(vacinacao_covid(IDS,IDU,D,V,T)).

% ----------------------------------------------------------------------
% Extensão do predicado removeVacinacaoCovid
%     que permite a remoção de uma vacinação a partir do ID de um utente
% ----------------------------------------------------------------------

removeVacinacaoCovid(IDU):- involucao(vacinacao_covid(_,IDU,_,_,_)).

% ----------------------------------------------------------------------
% Funcionalidades minimas
% ----------------------------------------------------------------------

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
% Extensão do predicado vacinado_indevidamente que apresenta uma lista
% com os utentes que foram mal vacinados de acordo com os critérios estabelecidos
% ----------------------------------------------------------------------

vacinado_indevidamente(RS) :- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),
                                vacinado_indevidamente_1afase(X),
                                vacinado_indevidamente_2afase(Z),concatenar(X,Z,R),pertence((ID,N),R)),R),
                                remove_dups(R,RS).

% ----------------------------------------------------------------------
% Extensão do predicado vacinado_indevidamente_1afase que apresenta uma lista
% com os utentes que foram mal vacinados de acordo com os critérios estabelecidos para a primeira fase
% ----------------------------------------------------------------------

vacinado_indevidamente_1afase(RS):- solucoes((ID,N),
                                    (utente(ID,_,N,_,_,_,_,_,_,_),
                                    atleast_one_vacina(ID),
                                    (vacinacao_covid(_,ID,D-M-A,_,_),(M\=12,A==2020;M>3,A==2021);
                                    nao(check_1afase((ID,N))))),R),
                                    remove_dups(R,RS).

% ----------------------------------------------------------------------
% Extensão do predicado vacinado_indevidamente_1afase que apresenta uma lista
% com os utentes que foram mal vacinados de acordo com os critérios estabelecidos para a segunda fase
% ----------------------------------------------------------------------

vacinado_indevidamente_2afase(RS):- solucoes((ID,N),
                                    (utente(ID,_,N,_,_,_,_,_,_,_),
                                    atleast_one_vacina(ID),
                                    (vacinacao_covid(_,ID,D-M-A,_,_),((M<4;M>8),A\=2021);
                                    nao(check_2afase((ID,N))))),R),
                                    remove_dups(R,RS).

% ----------------------------------------------------------------------
% Extensão do precidado atleast_one_vacina que indica se um utente
% já tem pelo menos uma vacina
% ----------------------------------------------------------------------

atleast_one_vacina(ID):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),vacinacao_covid(_,ID,_,_,_)),R),comprimento(R,N), N>=1.

% ----------------------------------------------------------------------
% Extensão do precidado check_1afase que indica se um utente
% pertence aos utentes aptos à primeira fase de vacinação
% ----------------------------------------------------------------------

check_1afase((ID,N)):- primeira_fase(Y),pertence((ID,N),Y).

% ----------------------------------------------------------------------
% Extensão do precidado check_2afase que indica se um utente
% pertence aos utentes aptos à segunda fase de vacinação
% ----------------------------------------------------------------------

check_2afase((ID,N)):- segunda_fase(Y),pertence((ID,N),Y).


% ----------------------------------------------------------------------
% Extensão do predicado candidato_nao_vacinado que apresenta uma lista
% com os utentes que ainda não foram vacinados e são candidatos
% ----------------------------------------------------------------------

candidato_nao_vacinado(RS):- solucoes((ID,N),(utente(ID,_,N,_,_,_,_,_,_,_),
                                nao_vacinado(X),pertence((ID,N),X),
                                candidato(Z),pertence((ID,N),Z)),R),
                                remove_dups(R,RS).

% ----------------------------------------------------------------------
% Extensão do precidado candidato que indica se um utente é candidato a ser vacinado
% ----------------------------------------------------------------------                                

candidato(RS):- primeira_fase(X),segunda_fase(Y),concatenar(X,Y,RS).

% ----------------------------------------------------------------------
% Invariantes de inserção
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Utente
% ----------------------------------------------------------------------

% Verificar se já existe utente com este ID
+utente( ID,_,_,_,_,_,_,_,_,_ ) :: (solucoes( ID,
		  (utente( ID,_,_,_,_,_,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se já existe utente com este número de segurança social
+utente( _,NSS,_,_,_,_,_,_,_,_ ) :: (solucoes( NSS,
		  (utente( _,NSS,_,_,_,_,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).   

% Verificar se existe email
+utente( _,_,_,_,E,_,_,_,_,_ ) :: (solucoes( E,
		  (utente( _,_,_,_,E,_,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).  

% Verificar se existe telefone
+utente( _,_,_,_,_,T,_,_,_,_ ) :: (solucoes( T,
		  (utente( _,_,_,_,_,T,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se existe centro de saúde
+utente( _,_,_,_,_,_,_,_,_,IDCS ) :: (solucoes( IDCS,
		  (centro_saude( IDCS,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).



% ----------------------------------------------------------------------
% Staff
% ----------------------------------------------------------------------


% Verificar se já existe staff inserido com este ID
+staff( IDS,_,_,_ ) :: (solucoes( IDS,
		  (staff( IDS,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).
                  
% Verificar se existe centro_saude inserido                  
+staff( _,IDCS,_,_ ) :: (solucoes( IDCS,
		  (centro_saude( IDCS,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se existe email
+staff( _,_,_,E ) :: (solucoes( E,
		  (staff( _,_,_,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).                  

% ----------------------------------------------------------------------
% Centro de Saude
% ----------------------------------------------------------------------


% Verificar se já existe centro_saude inserido com este ID
+centro_saude( IDC,_,_,_,_ ) :: (solucoes( IDC,
		  (centro_saude( IDC,_,_,_,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se existe telefone 
+centro_saude( _,_,_,T,_ ) :: (solucoes( T,
		  (centro_saude( _,_,_,T,_ )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se existe email
+centro_saude( _,_,_,_,E ) :: (solucoes( E,
		  (centro_saude( _,_,_,_,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).                  

% ----------------------------------------------------------------------
% Vacinação Covid
% ----------------------------------------------------------------------

% Verificar se existe staff inserido
+vacinacao_covid( IDS,_,_,_,_ ) :: (solucoes( IDS,
		  (staff(IDS,_,_,_)), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verificar se existe utente inserido
+vacinacao_covid( _,IDU,_,_,_ ) :: (solucoes( IDU,
		  (utente(IDU,_,_,_,_,_,_,_,_,_)), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

% Verifica se é a toma correta, isto é, se já tomou a primeira só pode tomar a segunda
% e se ainda não tomou nenhuma, só pode tomar a primeira
+vacinacao_covid(_,IDU,_,_,T):: (
            (utente(ID,_,_,_,_,_,_,_,_,_)),
            numVacinasUtentes(ID,TS),T=<2,
            T =:= TS
            ). 

% Verifica se está a tomar a mesma vacina no caso de já ter tomado uma primeira dose
+vacinacao_Covid(_,IDU,_,V,T) :: (
            (T == 1;
            (solucoes(IDU,vacinacao_Covid(_,IDU,_,_,_,V,_),L),
            comprimento(L,N),
            N == 2)
            )).

% Verifica se já existe inserida uma dose para uma dada vacina
+vacinacao_covid(_,IDU,_,V,1)::(
            (solucoes(IDU,(vacinacao_covid(_,IDU,_,V,1)),R),
            comprimento(R,N),
            N==1
            )).            
            
% Verifica se está a tomar a segunda dose depois da primeira
+vacinacao_covid(_,IDU,D-M-A,_,T) ::(
            (T == 1;
            (vacinacao_covid(_,IDU,D2-M2-A2,_,1)),
            T==2,
            (D>D2,M==M2,A==A2;M>M2,A==A2;A>A2)
            )).


% Verifica se o utente já tem as duas vacinas
+vacinacao_covid(_,IDU,_,_,_)::(
            (solucoes(IDU,(vacinacao_covid(_,IDU,_,_,_)),R),
            comprimento(R,N),
            N=<2
            )).


% ----------------------------------------------------------------------
% Invariantes de remoção
% ----------------------------------------------------------------------

-utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS ) :: (solucoes( (ID,NSS,N,DN,E,T,M,P,LDC,IDCS),
		  (utente( ID,NSS,N,DN,E,T,M,P,LDC,IDCS )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-staff( IDS,IDC,N,E ) :: (solucoes( (IDS,IDC,N,E),
		  (staff( IDS,IDC,N,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-centro_saude( IDC,N,M,T,E ) :: (solucoes( (IDC,N,M,T,E),
		  (centro_saude( IDC,N,M,T,E )), S ),
                  comprimento( S,NS ), 
		  NS == 1
                  ).

-vacinacao_covid( IDS,IDU,D,V,T ) :: (solucoes( (IDS,IDU,D,V,T),
		  (vacinacao_covid( IDS,IDU,D,V,T )), S ),
                  comprimento( S,NS ), 
		  NS =< 2
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
                   teste(Lista),
		           remocao(Termo).

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
demo(Questao,desconhecido) :- 
    nao(Questao), nao(-Questao).

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


% ----------------------------------------------------------------------
% Extensao do predicado intersecção: Lista1, Lista2, ListaFinal 
% ----------------------------------------------------------------------

intersection(A,B,AnB):- subtract(A,B,AminusB),subtract(A,AminusB,K),sort(K,AnB).
% Consulter l'annexe pour plus de detailles

%-------------------------------------------------------------------
%			    Predicats de base
%-------------------------------------------------------------------
	% 1
	% Signature : member(? element, + liste)
	% Description : Verifie si l’element appartient a la liste.

		member(X,[X|_]).
		member(X,[_|L]) :- member(X,L).


	% 2
	% Signature : listeMul(+element,+nombre,-liste)
	% Description : Renvoie une liste de la forme [element,element,….element] nombre fois.
	%		exemple : listeMul(x,3,L) nous donne L=[x , x , x]

		listeMul(_,0,[]).
		listeMul(Element,Nombre,[Element|Liste]) :- Nombre>0,Nombre2 is Nombre - 1, listeMul(Element,Nombre2,Liste).


	% 3
	% Signature : longeur(+liste, ?longeur)
	% Description : Renvoyer dans longeur la taille de la liste

		longeur([],0).
		longeur([_|L],S) :- longeur(L,T), S is T+1.


	% 4
	% Signature : replace(+element, +rang, +liste, +listeResultante)
	% Description : la listeResultante recois la liste ou l’element du rang « rang » est remplace par element.
	%		exemple : replace(y,2,[x,x,x],L).

		replace(Element,1,[_|L],[Element|L]).
		replace(Element,Rang,[X|LS],[X|LD]) :- longeur(LS,Long),L2 is Long+1,Rang=<L2,Rang2 is Rang - 1,
		        replace(Element,Rang2,LS,LD).


	% 5
	% Signature : element( ?element, ?rang, + Liste)
	% Description : verifie si l’element du rang rang dans la Liste est bel est bien element.
	%	        peut etre utilise pour connaitre le rang d’un element ou bien l’element qui
	%		se trouve dans le rang rang.
	%		exemple : element(y,2,[x,z,x]) nous donne false.

		element(Rang,X,[X|_]) :- Rang is 1.
		element(Rang,X,[_|L]) :- element(Rang2,X,L),Rang is Rang2+1.


	% 6
	% Signature : choose_max_solution(+liste,-solution,-valeur)
	% Description : A partir de « liste » qui est une liste de couple [X,Y], « valeur » represente le maximum des Y,
	%		et « solution » represente le X associe a « valeur »

		choose_max_solution([[P,V]],P,V).
		choose_max_solution([[P1,V1],[_,V2]],P1,V1) :- V1>V2,!.
		choose_max_solution([[_,V1],[P2,V2]],P2,V2) :- V2>=V1,!.
	        % le maximum sera le max entre (la tete) et (le max du reste de la liste) :
		choose_max_solution([[P,V]|L],X,Y) :- choose_max_solution(L,P1,V1),
		                    choose_max_solution([[P,V],[P1,V1]],X,Y).


	% 7
	% Signature : choose_min_solution(+liste,-solution,-valeur)
	% Description : A partir de « liste » qui est une liste de couple [X,Y], « valeur » represente le minimum des Y,
	%		et « solution » represente le X associe a « valeur »

		choose_min_solution([[P,V]],P,V).
		choose_min_solution([[P1,V1],[_,V2]],P1,V1) :- V2>=V1,!.
		choose_min_solution([[_,V1],[P2,V2]],P2,V2) :- V2<V1,!.
                % le mimimum sera le min entre (la tete) et (le min du reste de la liste) :
		choose_min_solution([[P,V]|L],X,Y) :- choose_min_solution(L,P1,V1),
				    choose_min_solution([[P,V],[P1,V1]],X,Y).


%-------------------------------------------------------------------
%			    Predicats du jeu :
%-------------------------------------------------------------------

%----------------------------------------
%	     Predicats globaux :
%----------------------------------------
	% 1
	% Signature : inverse(+Joueur, -Adversaire)
	% Description : Renvoie dans Adversaire l’adversaire du Joueur :
	%		  Si joueur = blanc alors Adversaire = noir
	%		  Si joueur = noir alors Adversaire = blanc

		inverse(noir,blanc).
		inverse(blanc,noir).


	% 2
	% Signature : getList(+Plateau,+element, -L)
	% Description : Renvoie dans la liste L l’ensemble des rangs des elements element
	%	        Exemple : getListe([libre, noir,noir],noir,L) nous renvoie L=[2,3]

		getList(Plateau,Dot,L) :- findall(X,element(X,Dot,Plateau),L).


	% 3
	% Signature : initP(-Plateau)
	% Description : Initialise le Plateau du jeu au debut de la partie.

		initP(P) :- listeMul(libre,64,P1), replace(blanc,28,P1,P2),replace(noir,29,P2,P3),
		            replace(noir,36,P3,P4),replace(blanc,37,P4,P).


	% 4
	% Signature : isIndiceLibre(+Indice, +Plateau)
	% Description : Verifie si la cellule n° « Indice » du Plateau est libre.

		isIndiceLibre(1,[libre|_]).
		isIndiceLibre(Indice,[_|L]) :- isIndiceLibre(Indice2,L),Indice is Indice2 + 1.


	% 5
	% Signature : estNonBorne(+Indice, +Pas)
	% Description : Verifie si on n’a pas depasse les bornes du plateau en
	%	        le parcourant dans la direction associee au Pas, et en
        %               etant a la cellule n° « Indice ».

		estNonBorne(Indice,-8) :- Indice>=1.
		estNonBorne(Indice,-7) :- Indice>=1, Indice2 is Indice mod 8, not(Indice2==1).
		estNonBorne(Indice,1)  :- Indice=<64,Indice2 is Indice mod 8, not(Indice2==1).
		estNonBorne(Indice,9)  :- Indice=<64,Indice2 is Indice mod 8, not(Indice2==1).
		estNonBorne(Indice,8)  :- Indice=<64.
		estNonBorne(Indice,7)  :- Indice=<64,Indice2 is Indice mod 8, not(Indice2==0).
		estNonBorne(Indice,-1) :- Indice>=1,Indice2 is Indice mod 8, not(Indice2==0).
		estNonBorne(Indice,-9) :- Indice>=1,Indice2 is Indice mod 8, not(Indice2==0).


	% 6
	% Signature : isCadrerAvecPas(+PositionCourante, +Joueur, +Pas, +IndiceOrigine, +Indice)
	% Description : Ce predicat verifie si en posant un pion de Joueur dans la cellule n°  « Indice »,
	%		on va encadrer les pions de l’adversaire sur la direction correspondante au Pas.

                % la premiere cellule doit contenir un pion de l'adversaire
		isCadrerAvecPas(PosCourante,Joueur,Pas,IndiceOrigine,Indice) :- estNonBorne(Indice,Pas),
			        I is (IndiceOrigine+Pas),Indice==I, inverse(Joueur,Joueur2),
				element(Indice,Joueur2,PosCourante),IndiceProchains is Indice+Pas,
			        isCadrerAvecPas(PosCourante,Joueur,Pas,IndiceOrigine,IndiceProchains).
                % pour les autres cellules, si on arrive a une qui contient un pion de notre joueur, alors
		% des pions de l'adversaire ont été effectivement encadres, donc le predicat est vrai.
		isCadrerAvecPas(PosCourante,Joueur,Pas,IndiceOrigine,Indice) :- estNonBorne(Indice,Pas),
		                I is (IndiceOrigine+Pas),Indice\==I, element(Indice,Joueur,PosCourante),!.
		% si on trouve encore des pions de l'adversaire sur cette cellule, alors on continue
                %  l'exploration sur la direction courante
		isCadrerAvecPas(PosCourante,Joueur,Pas,IndiceOrigine,Indice) :- estNonBorne(Indice,Pas),
				I is (IndiceOrigine+Pas),Indice\==I, inverse(Joueur,Joueur2),
				element(Indice,Joueur2,PosCourante),IndiceProchains is Indice+Pas,
			        isCadrerAvecPas(PosCourante,Joueur,Pas,IndiceOrigine,IndiceProchains).


	% 7
	% Signature : direction(+PositionCourante,+Joueur,+Indice,+Pas)
	% Description : Ce predicat lance le premier appel au predicat isCadrerAvecPas, en lui precisant
	%	        l’indice de la  premiere cellule sur la direction etudiee.

		direction(PosCourante,Joueur,Indice,Pas) :- Indice2 is Indice+Pas,
		          isCadrerAvecPas(PosCourante,Joueur,Pas,Indice,Indice2).


	% 8
	% Signature : aEncadrer(+PosCourante,+Joueur, +Indice)
	% Description : Renvoie True si le joueur peut poser un pion dans Indice dans la position PosCourante

                % on explore les 8 directions pour verifier si en jouant ce coup, on a encadre des pions
                % de l'adversaire sur au moins une direction
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,-8),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,-7),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,1),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,9),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,8),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,7),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,-1),!.
		aEncadrer(PosCourante,Joueur,Indice) :- direction(PosCourante,Joueur,Indice,-9),!.


	% 9
	% Signature : move(+PosCourante,+Joueur, ?Indice)
	% Description : Renvoie dans Indice un des mouvements possibles pour le joueur dans la position PosCourante.
	%		si Indice est instancie, elle verifie si le mouvement est possible.

		move(PosCourante,Joueur,Indice) :- isIndiceLibre(Indice,PosCourante),
		     aEncadrer(PosCourante,Joueur,Indice).


	% 10
	% Signature : score(+Plateau, -Blanc, -Noir)
	% Description : Retourne le score du joueur blanc et du joueur noir respectivement dans Blanc et Noir.

		score([],0,0).
		score([blanc|Q],Blanc,Noir) :-score(Q,Blanc1,Noir), Blanc is Blanc1+1.
		score([noir|Q],Blanc,Noir) :- score(Q,Blanc,Noir1),Noir is Noir1+1.
		score([libre|Q],Blanc,Noir) :- score(Q,Blanc,Noir).


	% 11
	% Signature : final(+PlateauCourant, -Gagnant)
	% Description : Verifie si sur le PlateauCourant la partie est finie, et retourne aussi
	%		le Gagnant qui peut prendre trois valeurs (noir, blanc, nul).

		final(PosCourante,blanc) :- not(move(PosCourante,noir,_)),not(move(PosCourante,blanc,_)),
		      score(PosCourante,B,N),B>N.
		final(PosCourante,noir) :- not(move(PosCourante,noir,_)),not(move(PosCourante,blanc,_)),
		      score(PosCourante,B,N),N>B.
		final(PosCourante,nul) :- not(move(PosCourante,noir,_)),not(move(PosCourante,blanc,_)),
		      score(PosCourante,B,N),B==N.


	% 12
	% Signature : changer(+PositionCourante, +Joueur, +Indice, -PositionSuivante, +Pas)
	% Description : Apres qu’un nouveau pion est pose par un Joueur, ce predicat effectue sur
        %               la PositionCourante les changements necessaires correspondants seulement
        %               a la direction associee au Pas, a partir de l’Indice, et retourne le
        %		resultat dans PositionSuivante.

                % si on arrive a une cellule contenant un pion de notre joueur, alors le parcours est termine
		changer(PosCourante,Joueur,Indice,PosCourante,_) :- element(Indice,Joueur,PosCourante).
                % a chaque fois qu'une cellule parcourue contient un pion de l'adversaire, son
		% pion sera transforme en un pion de notre joueur, et puis on continue le parcours
		changer(PosCourante,Joueur,Indice,PosSuivante,Pas) :- inverse(Joueur,Joueur2),
		        element(Indice,Joueur2,PosCourante), replace(Joueur,Indice,PosCourante,PosSuivante2),
		        Indice2 is Indice + Pas, changer(PosSuivante2,Joueur,Indice2,PosSuivante,Pas).


	% 13
	% Signature : maj(+PositionCourante, +Joueur, +Indice, -PositionSuivante, +Pas)
	% Description : Apres qu’un nouveau pion est pose par un Joueur, ce predicat verifie
        %		s’il y a des changements a faire sur la direction associee au Pas, et
        %               si c’est le cas alors elle les fait a travers le predicat changer.

	        % si le predicat direction est vrai, donc il y a des modification a faire, donc
		% on realise le predicat changer
		maj(PosCourante,Joueur,Indice,PosSuivante,Pas) :- direction(PosCourante,Joueur,Indice,Pas),
		    Indice2 is Indice + Pas,changer(PosCourante,Joueur,Indice2,PosSuivante,Pas).
                % si le predicat retourne faux, alors il n'y aucun changement a faire sur cette direction
		maj(PosCourante,Joueur,Indice,PosCourante,Pas) :- not(direction(PosCourante,Joueur,Indice,Pas)).


	% 14
	% Signature : transform(+PositionCourante, +Joueur, +Indice, -PositionSuivante)
	% Description : Apres qu’un nouveau pion est pose par un Joueur sur la cellule n° « Indice »,
	%		le predicat effectue tous les changements possible dans toutes les directions,
        %               et retourne le resultat dans la PositionSuivante.

		% on realise le predicat maj sur toutes les 8 directions
		transform(PosCourante,Joueur,Indice,PosSuivante) :- replace(Joueur,Indice,PosCourante,PosSuivante2),
								    maj(PosSuivante2,Joueur,Indice,PosSuivante3,-8),
								    maj(PosSuivante3,Joueur,Indice,PosSuivante4,-7),
								    maj(PosSuivante4,Joueur,Indice,PosSuivante5,1),
								    maj(PosSuivante5,Joueur,Indice,PosSuivante6,9),
								    maj(PosSuivante6,Joueur,Indice,PosSuivante7,8),
								    maj(PosSuivante7,Joueur,Indice,PosSuivante8,7),
								    maj(PosSuivante8,Joueur,Indice,PosSuivante9,-1),
								    maj(PosSuivante9,Joueur,Indice,PosSuivante,-9).


	% 15
	% Signature : jouer(+PositionCourante, +Joueur, +Indice, -PositionSuivante)
	% Description : Verifie si le Joueur peut poser son pion sur la cellule n° « Indice »,
        %               si c’est le cas,il effectue tous les changements necessaires
	%               et retourne le resultat dans la PositionSuivante.

		jouer(PosCourante,Joueur,IndiceChoisi,PosSuivante) :- move(PosCourante,Joueur,IndiceChoisi),
		      transform(PosCourante,Joueur,IndiceChoisi,PosSuivante).


%----------------------------------------
%	   Predicats pour l'IA1
%----------------------------------------
	% 1
	% Signature : iA1(+PosCourante,+Joueur,-PosSuivante,-MvtFait)
	% Description : Renvoie dans PosSuivant le plateau qui suit PosCourant ,apres que l’IA
	%               Joueur (noir ou blanc) choisit le mouvement aleatoire retourne dans MvtFait

                % on recupere les mouvements possibles avec le predicat bagof, et puis on choisit au hasard
                % l'un d'eux et on l'applique
		iA1(PosCourante,Joueur,PosSuivante,MvtFait) :- bagof(X,move(PosCourante,Joueur,X),L),longeur(L,SizeL),
			                                      Size2 is SizeL+1,random(1,Size2,RangChoisi),
							      element(RangChoisi,MvtFait,L),
							      transform(PosCourante,Joueur,MvtFait,PosSuivante).


%----------------------------------------
%	Affichage sous Prolog de IA1
%----------------------------------------

	% 1
	% Signature : affichage(+Plateau)
	% Description : affiche le Plateau

		affichage(Plateau)  :-
		         writeln('--------------------------------------------------------------------------------'),
			 writeln(Plateau),writeln(''),
			 write('noir : '),getList(Plateau,noir,L),writeln(L),
			 write('Blanc : '),getList(Plateau,blanc,L2),writeln(L2),
			 writeln('').


	% 2
	% Signature : writeMvtPossible(+Plateau,+Joueur)
	% Description : Affiche la liste des mouvements Possible pour le Joueur (noir ou blanc) dans le plateau Plateau

		writeMvtPossible(Plateau,Joueur) :- findall(X,move(Plateau,Joueur,X),L),write('Choisir de :'),writeln(L).


%-------------------------------------------------
% Deroulement Jeu sous Prolog entre IA1 et Humain
%-------------------------------------------------

	% 1
	% Signature : avancerJeu(+Humain,+Plateau,+Mvt)
	% Description : prend compte du mouvement choisit Mvt par l’humain (noir ou blanc) et :
	%	             1. Dans le cas ou le mouvement est non permis, elle rappelle
        %			deroulerJeu(Humain,Plateau,Humain)
	%		     2. Dans le cas ou le mouvement est permis, elle rappelle
        %			deroulerJeu(Humain, PlateauSuivant,IA)en passant la main
        %			a l’intellgience artificiel IA1

		avancerJeu(Humain,Plateau,Mvt) :- jouer(Plateau,Humain,Mvt,Plateau2),
			   inverse(Humain,IA),deroulerJeu(Humain,Plateau2,IA).
		avancerJeu(Humain,Plateau,Mvt) :- not(jouer(Plateau,Humain,Mvt,_)),
			   writeln('Mouvement Impossible !'),deroulerJeu(Humain,Plateau,Humain).


	% 2
	% Signature : deroulerJeu(+Humain,+Plateau,+JoueurCourant)
	% Description : Ce predicat affiche le plateau actuel et :
	%		     1. Si le joueurCourant est l’humain il lit a partir de la console
	%                       le mouvement est fait appelle a avancerJeu(Humain,Plateau,Mvt)
	%		     2. Si le joueurCourant esst l’IA (inverse de Humain) on fait appelle a ia1
	%               Ce predicat prend fin lorsque le plateau est final est affiche le gagnat,
        %               et dans le cas ou pour l’un des deux joeurs aucun mouvement n’est possible,
        %               il fait passer la main a l’adversaire.

		deroulerJeu(_,Plateau,_) :- final(Plateau,V),writeln(V),!.
		deroulerJeu(Humain,Plateau,Humain) :- affichage(Plateau),move(Plateau,Humain,_),
			    writeMvtPossible(Plateau,Humain),write('Mouvement : '),read(Mvt),
			    avancerJeu(Humain,Plateau,Mvt).
		deroulerJeu(Humain,Plateau,Humain) :- affichage(Plateau),not(move(Plateau,Humain,_)),
		            writeln('Aucun mouvements possible'),inverse(Humain,IA),deroulerJeu(Humain,Plateau,IA).
		deroulerJeu(Humain,Plateau,IA) :- inverse(Humain,IA),affichage(Plateau),write('tour de la machine : '),
			    iA1(Plateau,IA,Plateau2,MvtFait), writeln(MvtFait),deroulerJeu(Humain,Plateau2,Humain).
		deroulerJeu(Humain,Plateau,IA) :- inverse(Humain,IA),affichage(Plateau),write('tour de la machine : '),
			    not(iA1(Plateau,IA,_,_)),writeln('Aucun mouvement possibles'),
			    deroulerJeu(Humain,Plateau,Humain).


	% 3
	% Signature : jeuIA1Humain(+Humain)
	% Description : lance le jeu Humain contre l’intelligence artificielle ia1 (si Humain est
	%		noir le joueur commence sinon l’IA commence)

		jeuIA1Humain(Humain) :- initP(Plateau),deroulerJeu(Humain,Plateau,noir).


%----------------------------------------
%			Predicats pour l'IA2 :
%----------------------------------------

	% 1
	% Signature : listePoids(-ListePonderation)
	% Description : renvoie la ListePonderation

		listePoids([500,-150,30,10,10,30,-150,500,
			    -150,-250,0,0,0,0,-250,-150,
			    30,0,1,2,2,1,0,30,
			    10,0,2,16,16,2,0,10,
			    10,0,2,16,16,2,0,10,
			    30,0,1,2,2,1,0,30,
			    -150,-250,0,0,0,0,-250,-150,
			    500,-150,30,10,10,30,-150,500]).


	% 2
	% Signature : calculerValeur(+Plateau,+ListePonderation,-Valeur)
	% Description : Renvoie dans Valeur la valeur du joueur noir dans le Plateau c'est-a-dire
	%		la somme des ponderation des cases ocupee par le joueur noir

		calculeValeur([],[],0).
		calculeValeur([noir|Plateau],[Y|Poids],Z) :- calculeValeur(Plateau,Poids,Z1),Z is Y+Z1.
		calculeValeur([X|Plateau],[_|Poids],Z) :- X\==noir, calculeValeur(Plateau,Poids,Z).


	% 3
	% Signature : h(+Plateau,-Valeur)
	% Description : Renvoie dans Valeur la valeur du joueur noir dans le plateau,
	%		si le plateau est en position finale :
	%		       1. Noir a gagne : Valeur = 5000
	%		       2. Blanc a gagne : Valeur = -5000
	%		       3. Nul a gagne : Valeur = 0
	%		Si la position n’est pas finale on renvoie dans valeur,
	%	        CalculerValeur(Plateau,ListePonderation,valeur)

		h(Plateau,5000) :- final(Plateau,noir).
		h(Plateau,-5000) :- final(Plateau,blanc).
		h(Plateau,0) :- final(Plateau,nul).
		h(Plateau,Valeur) :- listePoids(Poids),calculeValeur(Plateau,Poids,Valeur).


	% 4
	% Signature : minimax(+Joueur, +PositionCourante, +Valeur, -PositionSuivante, -MouvementFait, +Profondeur)
	% Description : Selon l’algorithme de minimax, ce predicat choisit le mouvement optimal a faire
	%		(MouvementFait) pour le Joueur, selon la Profondeur donnee, et retourne la Valeur
	%		d’heuristique associee, et le resultat dans la PositionSuivante.

                % si on est à une position finale, donc on retourne directement la valeur d'heuristique
		minimax(_,Pos_courante,Valeur,_,_,_):- final(Pos_courante,_), h(Pos_courante,Valeur), !.
                % si on est a la profondeur 0, donc on retourne la valeur de
                % l'heuristique de la position courante
		minimax(_,Pos_courante,Valeur,_,_,0):- h(Pos_courante,Valeur), !.
		% si on est à une profondeur!=0, donc on parcourt le niveau suivant pour tous les mouvements possibles
		% et on choisit celui qui maximise la valeur d'heuristique (vu que c'est pour le joueur noir)
		minimax(noir,Pos_courante,Valeur,Pos_suivante,MvtFait,Profondeur) :-bagof(X,move(Pos_courante,noir,X),L),
		        Profondeur2 is Profondeur-1,Profondeur2>=0, findall([MvtAFaire,Valeur2],
			(member(MvtAFaire,L), transform(Pos_courante,noir,MvtAFaire,PS),
			minimax(blanc,PS,Valeur2,_,_,Profondeur2)),L2), choose_max_solution(L2,MvtFait,Valeur),
		        transform(Pos_courante,noir,MvtFait,Pos_suivante).
		% si on est à une profondeur!=0, donc on parcourt le niveau suivant pour tous les mouvements possibles
		% et on choisit celui qui minimise la valeur d'heuristique (vu que c'est pour le joueur blanc)
	        minimax(blanc,Pos_courante,Valeur,Pos_suivante,MvtFait,Profondeur) :-bagof(X,move(Pos_courante,blanc,X),L),
		        Profondeur2 is Profondeur-1,Profondeur2>=0,findall([MvtAFaire,Valeur2],
			(member(MvtAFaire,L),transform(Pos_courante,blanc,MvtAFaire,PS),
			minimax(noir,PS,Valeur2,_,_,Profondeur2)),L2),
		        choose_min_solution(L2,MvtFait,Valeur),
		        transform(Pos_courante,blanc,MvtFait,Pos_suivante).
		% si a la position courante, le joueur courant n'a aucun coup a jouer
		minimax(Joueur,Pos_courante,Valeur,_,_,Profondeur):- not(move(Pos_courante,Joueur,_)),
		                                                     inverse(Joueur,Adv),
								    minimax(Adv,Pos_courante,Valeur,_,_,Profondeur).


	% 5
	% Signature : iA2(+PositionCourante, +Joueur, -PositionSuivante, -MouvementFait, +Niveau)
	% Description : En utilisant l’heuristique minimax et avec une profondeur « Niveau » qui doit etre
	%	        superieur ou egale a 1, ce predicat choisit le mouvement optimal (MouvementFait) a faire
        %		par le Joueur, et retourne le resultat obtenu apres ce mouvement dans la PositionSuivante.

		iA2(PosCourante,Joueur,PosSuivante,MvtFait,Niveau) :- move(PosCourante,Joueur,_),
	            minimax(Joueur,PosCourante,_,PosSuivante,MvtFait,Niveau).

%----------------------------------------
%	Deroulement Jeu entre IA2 et Humain
%----------------------------------------
	% 1
	% Signature : avancerJeu2(+JoueurHumain, +Plateau, +Mouvement, +Niveau)
	% Description : Verifie si le Mouvement choisi par le JoueurHumain est valable, si c’est le cas
	%		il met a jour le Plateau, et il passe la main a la machine pour qu’elle joue
	%		son tour en utilisant iA2.

		avancerJeu2(Humain,Plateau,Mvt,Niveau) :- jouer(Plateau,Humain,Mvt,Plateau2),inverse(Humain,IA),
			                                  deroulerJeu2(Humain,Plateau2,IA,Niveau).
		avancerJeu2(Humain,Plateau,Mvt,Niveau) :- not(jouer(Plateau,Humain,Mvt,_)),
		            writeln('Mouvement Impossible !'), deroulerJeu2(Humain,Plateau,Humain,Niveau).


	% 2
	% Signature : deroulerJeu2(+JoueurHumain, +Plateau, +JoueurCourant, +Niveau)
	% Description : Deroule le jeu selon le principe suivant :
	%		  - Si le JoueurCourant est humain, alors ce predicat lui donne la main pour jouer,
	%		  - Sinon il effectue un mouvement choisi par l’iA2 selon la profondeur « Niveau ».

		% si on est a une position finale, alors la partie doit se terminer
		deroulerJeu2(_,Plateau,_,_) :- final(Plateau,V),writeln(V),!.
                % si c'est a l'humain de jouer, alors on lui donne la main, et puis on avance
		deroulerJeu2(Humain,Plateau,Humain,Niveau) :- affichage(Plateau),move(Plateau,Humain,_),
			     writeMvtPossible(Plateau,Humain),write('Mouvement : '),read(Mvt),
			     avancerJeu2(Humain,Plateau,Mvt,Niveau).
                % si l'humain n'a aucun coup a jouer, alors on lui informe et puis on passe la main
		% a l'iA2
		deroulerJeu2(Humain,Plateau,Humain,Niveau) :- affichage(Plateau),not(move(Plateau,Humain,_)),
		             writeln('Aucun mouvements possible'),
			     inverse(Humain,IA),deroulerJeu2(Humain,Plateau,IA,Niveau).
		% si c'est à l'iA a jouer, alors elle joue un coup et ensuite elle passe la main
		% a l'humain
		deroulerJeu2(Humain,Plateau,IA,Niveau) :- inverse(Humain,IA),affichage(Plateau),
		             write('tour de la machine : '), iA2(Plateau,IA,Plateau2,MvtFait,Niveau),
			     writeln(MvtFait),deroulerJeu2(Humain,Plateau2,Humain,Niveau).
		% si l'iA2 n'a aucune coup a jouer, alors elle passe la main a l'humain
		deroulerJeu2(Humain,Plateau,IA,Niveau) :- inverse(Humain,IA),affichage(Plateau),
		             write('tour de la machine : '), not(iA2(Plateau,IA,_,_,Niveau)),
			     writeln('Aucun mouvement possibles'), deroulerJeu2(Humain,Plateau,Humain,Niveau).


	% 3
	% Signature : jeuIA2Humain(+JoueurHumain, +Niveau)
	% Description : Lance une partie entre le JoueurHumain et l’iA2 avec une profondeur « Niveau »

		jeuIA2Humain(Humain,Niveau) :- initP(Plateau),deroulerJeu2(Humain,Plateau,noir,Niveau).


%----------------------------------------
%			Predicats pour l'IA3 :
%----------------------------------------



	% 1
	% Signature : h2(+Plateau,-Valeur)
	% Description : Renvoie dans Valeur la valeur du joueur noir dans le plateau,
	%		si le plateau est en position finale :
	%		       1. Noir a gagne : Valeur = 5000
	%		       2. Blanc a gagne : Valeur = -5000
	%		       3. Nul a gagne : Valeur = 0
	%		Si la position n’est pas finale on renvoie dans valeur,
	%	        difference de score : scoreNoir - scoreBlanc

		h2(Plateau,100) :- final(Plateau,noir).
		h2(Plateau,-100) :- final(Plateau,blanc).
		h2(Plateau,0) :- final(Plateau,nul).
		h2(Plateau,Valeur) :- score(Plateau,Blanc,Noir),Valeur is (Noir - Blanc).


	% 2
	% Signature : minimax2(+Joueur, +PositionCourante, +Valeur, -PositionSuivante, -MouvementFait, +Profondeur)
	% Description : Selon l’algorithme de minimax, ce predicat choisit le mouvement optimal a faire
	%		(MouvementFait) pour le Joueur, selon la Profondeur donnee, et retourne la Valeur
	%		d’heuristique associee, et le resultat dans la PositionSuivante.

                % si on est à une position finale, donc on retourne directement la valeur d'heuristique
		minimax2(_,Pos_courante,Valeur,_,_,_):- final(Pos_courante,_), h2(Pos_courante,Valeur),!.
                % si on est a la profondeur 0, donc on retourne la valeur de
                % l'heuristique de la position courante
		minimax2(_,Pos_courante,Valeur,_,_,0):- h2(Pos_courante,Valeur), !.
		% si on est à une profondeur!=0, donc on parcourt le niveau suivant pour tous les mouvements possibles
		% et on choisit celui qui maximise la valeur d'heuristique (vu que c'est pour le joueur noir)
		minimax2(noir,Pos_courante,Valeur,Pos_suivante,MvtFait,Profondeur) :-bagof(X,move(Pos_courante,noir,X),L),
		        Profondeur2 is Profondeur-1,Profondeur2>=0, findall([MvtAFaire,Valeur2],
			(member(MvtAFaire,L), transform(Pos_courante,noir,MvtAFaire,PS),
			minimax2(blanc,PS,Valeur2,_,_,Profondeur2)),L2), choose_max_solution(L2,MvtFait,Valeur),
		        transform(Pos_courante,noir,MvtFait,Pos_suivante).
                % si on est à une profondeur!=0, donc on parcourt le niveau suivant pour tous les mouvements possibles
		% et on choisit celui qui minimise la valeur d'heuristique (vu que c'est pour le joueur blanc)
		minimax2(blanc,Pos_courante,Valeur,Pos_suivante,MvtFait,Profondeur) :-bagof(X,move(Pos_courante,blanc,X),L),
		        Profondeur2 is Profondeur-1,Profondeur2>=0,findall([MvtAFaire,Valeur2],
			(member(MvtAFaire,L),transform(Pos_courante,blanc,MvtAFaire,PS),
			minimax2(noir,PS,Valeur2,_,_,Profondeur2)),L2),
		        choose_min_solution(L2,MvtFait,Valeur),
		        transform(Pos_courante,blanc,MvtFait,Pos_suivante).
		%si a la position courante, le joueur courant n'a aucun coup a jouer
		minimax2(Joueur,Pos_courante,Valeur,_,_,Profondeur):- not(move(Pos_courante,Joueur,_)), inverse(Joueur,Adv),
								    minimax2(Adv,Pos_courante,Valeur,_,_,Profondeur).



	% 3
	% Signature : iA3(+PositionCourante, +Joueur, -PositionSuivante, -MouvementFait, +Niveau)
	% Description : En utilisant l’heuristique minimax et avec une profondeur « Niveau » qui doit etre
	%	        superieur ou egale a 1, ce predicat choisit le mouvement optimal (MouvementFait) a faire
        %		par le Joueur, et retourne le resultat obtenu apres ce mouvement dans la PositionSuivante.

		iA3(PosCourante,Joueur,PosSuivante,MvtFait,Niveau) :- move(PosCourante,Joueur,_),
	            minimax2(Joueur,PosCourante,_,PosSuivante,MvtFait,Niveau).


%----------------------------------------
%	Deroulement Jeu entre IA3 et Humain
%----------------------------------------
	% 1
	% Signature : avancerJeu4(+JoueurHumain, +Plateau, +Mouvement, +Niveau)
	% Description : Verifie si le Mouvement choisi par le JoueurHumain est valable, si c’est le cas
	%		il met a jour le Plateau, et il passe la main a la machine pour qu’elle joue
	%		son tour en utilisant iA3.

		avancerJeu4(Humain,Plateau,Mvt,Niveau) :- jouer(Plateau,Humain,Mvt,Plateau2),inverse(Humain,IA),
			                                  deroulerJeu4(Humain,Plateau2,IA,Niveau).
		avancerJeu4(Humain,Plateau,Mvt,Niveau) :- not(jouer(Plateau,Humain,Mvt,_)),
		            writeln('Mouvement Impossible !'), deroulerJeu4(Humain,Plateau,Humain,Niveau).


	% 2
	% Signature : deroulerJeu4(+JoueurHumain, +Plateau, +JoueurCourant, +Niveau)
	% Description : Deroule le jeu selon le principe suivant :
	%		  - Si le JoueurCourant est humain, alors ce predicat lui donne la main pour jouer,
	%		  - Sinon il effectue un mouvement choisi par l’iA3 selon la profondeur « Niveau ».

		% si on est a une position finale, alors la partie doit se terminer
		deroulerJeu4(_,Plateau,_,_) :- final(Plateau,V),writeln(V),!.
                % si c'est a l'humain de jouer, alors on lui donne la main, et puis on avance
		deroulerJeu4(Humain,Plateau,Humain,Niveau) :- affichage(Plateau),move(Plateau,Humain,_),
			     writeMvtPossible(Plateau,Humain),write('Mouvement : '),read(Mvt),
			     avancerJeu4(Humain,Plateau,Mvt,Niveau).
                % si l'humain n'a aucun coup a jouer, alors on lui informe et puis on passe la main
		% a l'iA3
		deroulerJeu4(Humain,Plateau,Humain,Niveau) :- affichage(Plateau),not(move(Plateau,Humain,_)),
		             writeln('Aucun mouvements possible'),
			     inverse(Humain,IA),deroulerJeu4(Humain,Plateau,IA,Niveau).
		% si c'est à l'iA a jouer, alors elle joue un coup et ensuite elle passe la main
		% a l'humain
		deroulerJeu4(Humain,Plateau,IA,Niveau) :- inverse(Humain,IA),affichage(Plateau),
		             write('tour de la machine : '), iA3(Plateau,IA,Plateau2,MvtFait,Niveau),
			     writeln(MvtFait),deroulerJeu4(Humain,Plateau2,Humain,Niveau).
		% si l'iA3 n'a aucune coup a jouer, alors elle passe la main a l'humain
		deroulerJeu4(Humain,Plateau,IA,Niveau) :- inverse(Humain,IA),affichage(Plateau),
		             write('tour de la machine : '), not(iA3(Plateau,IA,_,_,Niveau)),
			     writeln('Aucun mouvement possibles'), deroulerJeu4(Humain,Plateau,Humain,Niveau).


	% 3
	% Signature : jeuIA3Humain(+JoueurHumain, +Niveau)
	% Description : Lance une partie entre le JoueurHumain et l’iA3 avec une profondeur « Niveau »

		jeuIA3Humain(Humain,Niveau) :- initP(Plateau),deroulerJeu4(Humain,Plateau,noir,Niveau).




%----------------------------------------------
% Deroulement Jeu sous Prolog entre IA1 et IA2
%----------------------------------------------
	% 1
	% Signature : deroulerJeuA1IA2(+Plateau, +JoueurCourant,  +NiveauIA2, -Vainqueur)
	% Description : Deroule le jeu entre le noir qui est « iA2 »
	%				(avec une profondeur « Niveau ») et le blanc qui est « iA1 ».

		deroulerJeuIA1IA2(Plateau,_,_,V) :- final(Plateau,V),writeln(V),!.
		deroulerJeuIA1IA2(Plateau,noir,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA2 (noir) : '),
						  iA2(Plateau,noir,Plateau2,MvtFait,Niveau),
						  writeln(MvtFait),deroulerJeuIA1IA2(Plateau2,blanc,Niveau,Vainqueur).
		deroulerJeuIA1IA2(Plateau,blanc,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA1 (blanc) : '),
						  iA1(Plateau,blanc,Plateau2,MvtFait),
						  writeln(MvtFait),deroulerJeuIA1IA2(Plateau2,noir,Niveau,Vainqueur).
		deroulerJeuIA1IA2(Plateau,noir,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA2 (noir) : '),
			                          not(iA2(Plateau,noir,_,_,Niveau)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA1IA2(Plateau,blanc,Niveau,Vainqueur).
		deroulerJeuIA1IA2(Plateau,blanc,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA1 (blanc) : '),
			                          not(iA1(Plateau,blanc,_,_)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA1IA2(Plateau,noir,Niveau,Vainqueur).


	% 2
	% Signature : jeuIA1IA2(+Niveau)
	% Description : Lance une partie entre iA1 (jouant avec le blanc) et iA2
	%				(jouant avec le noir et avec une profondeur « Niveau »).
		jeuIA1IA2(NiveauIA2) :- initP(Plateau),deroulerJeuIA1IA2(Plateau,noir,NiveauIA2,_).



%----------------------------------------------
% Deroulement Jeu sous Prolog entre IA1 et IA3
%----------------------------------------------
	% 1
	% Signature : deroulerJeuIA1IA3(+Plateau, +JoueurCourant,  +NiveauIA3, -Vainqueur)
	% Description : Deroule le jeu entre le noir qui est « iA3 »
	%				(avec une profondeur « Niveau ») et le blanc qui est « iA1 ».

		deroulerJeuIA1IA3(Plateau,_,_,V) :- final(Plateau,V),writeln(V),!.
		deroulerJeuIA1IA3(Plateau,noir,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA3 (noir) : '),
						  iA3(Plateau,noir,Plateau2,MvtFait,Niveau),
						  writeln(MvtFait),deroulerJeuIA1IA3(Plateau2,blanc,Niveau,Vainqueur).
		deroulerJeuIA1IA3(Plateau,blanc,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA1 (blanc) : '),
						  iA1(Plateau,blanc,Plateau2,MvtFait),
						  writeln(MvtFait),deroulerJeuIA1IA3(Plateau2,noir,Niveau,Vainqueur).
		deroulerJeuIA1IA3(Plateau,noir,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA3 (noir) : '),
			                          not(iA3(Plateau,noir,_,_,Niveau)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA1IA3(Plateau,blanc,Niveau,Vainqueur).
		deroulerJeuIA1IA3(Plateau,blanc,Niveau,Vainqueur) :- affichage(Plateau),write('tour de la machine IA1 (blanc) : '),
			                          not(iA1(Plateau,blanc,_,_)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA1IA3(Plateau,noir,Niveau,Vainqueur).



%----------------------------------------------
% Deroulement Jeu sous Prolog entre IA2 et IA3
%----------------------------------------------
	% 1
	% Signature : deroulerJeuIA2IA3(+Plateau, +JoueurCourant, +NiveauIA2, +NiveauIA3, -Vainqueur)
	% Description : Deroule le jeu entre le blanc qui est « iA2 »
	%				(avec une profondeur « Niveau ») et le noir qui est « iA3 ».

		deroulerJeuIA2IA3(Plateau,_,_,_,V) :- final(Plateau,V),writeln(V),!.
		deroulerJeuIA2IA3(Plateau,noir,NiveauIA2,NiveauIA3,Vainqueur) :- affichage(Plateau),
		                  write('tour de la machine IA3 (noir) : '),
						  iA3(Plateau,noir,Plateau2,MvtFait,NiveauIA3),
						  writeln(MvtFait),
						  deroulerJeuIA2IA3(Plateau2,blanc,NiveauIA2,NiveauIA3,Vainqueur).
		deroulerJeuIA2IA3(Plateau,blanc,NiveauIA2,NiveauIA3,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA2 (blanc) : '),
						  iA2(Plateau,blanc,Plateau2,MvtFait,NiveauIA2),
						  writeln(MvtFait),
						  deroulerJeuIA2IA3(Plateau2,noir,NiveauIA2,NiveauIA3,Vainqueur).
		deroulerJeuIA2IA3(Plateau,noir,NiveauIA2,NiveauIA3,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA3 (noir) : '),
			                          not(iA3(Plateau,noir,_,_,NiveauIA3)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA2IA3(Plateau,blanc,NiveauIA2,NiveauIA3,Vainqueur).
		deroulerJeuIA2IA3(Plateau,blanc,NiveauIA2,NiveauIA3,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA2 (blanc) : '),
			                          not(iA2(Plateau,blanc,_,_,_)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA2IA3(Plateau,noir,NiveauIA2,NiveauIA3,Vainqueur).


						  
%----------------------------------------------
% Deroulement Jeu sous Prolog entre IA2 et IA2
%----------------------------------------------
	% 1
	% Signature : deroulerJeuIA2IA2(+Plateau, +JoueurCourant, +NiveauIA2, +Niveau2IA2, -Vainqueur)
	% Description : Deroule le jeu entre iA2 et iA2

		deroulerJeuIA2IA2(Plateau,_,_,_,V) :- final(Plateau,V),writeln(V),!.
		deroulerJeuIA2IA2(Plateau,noir,NiveauIA2,Niveau2IA2,Vainqueur) :- affichage(Plateau),
		                  write('tour de la machine IA3 (noir) : '),
						  iA2(Plateau,noir,Plateau2,MvtFait,Niveau2IA2),
						  writeln(MvtFait),
						  deroulerJeuIA2IA2(Plateau2,blanc,NiveauIA2,Niveau2IA2,Vainqueur).
		deroulerJeuIA2IA2(Plateau,blanc,NiveauIA2,Niveau2IA2,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA2 (blanc) : '),
						  iA2(Plateau,blanc,Plateau2,MvtFait,NiveauIA2),
						  writeln(MvtFait),
						  deroulerJeuIA2IA2(Plateau2,noir,NiveauIA2,Niveau2IA2,Vainqueur).
		deroulerJeuIA2IA2(Plateau,noir,NiveauIA2,Niveau2IA2,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA3 (noir) : '),
			                          not(iA2(Plateau,noir,_,_,Niveau2IA2)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA2IA2(Plateau,blanc,NiveauIA2,Niveau2IA2,Vainqueur).
		deroulerJeuIA2IA2(Plateau,blanc,NiveauIA2,Niveau2IA2,Vainqueur) :- affichage(Plateau),
		write('tour de la machine IA2 (blanc) : '),
			                          not(iA2(Plateau,blanc,_,_,_)),writeln('Aucun mouvement possibles'),
						  deroulerJeuIA2IA2(Plateau,noir,NiveauIA2,Niveau2IA2,Vainqueur).

						  
						  


%----------------------------------------
%      Etude de performance des IA
%----------------------------------------
 % le premier joueur est toujours blanc, le deuxieme joueur est noir




	% 1
	% Signature : testerGagnantEtLancerProchaine(+J1,+J2, +NbrRestant, -Score1, -Score2, +Gagnant)
	% Description : Incrémenter le score du gagnant (si la partie n’est pas nulle) et fait un appel
	%           au predicat qui lancera les prochaines parties.

            testerGagnantEtLancerProchaine(J1,J2, NbrRestant, Score1, Score2, blanc):-
	                            lancerPlusieursParties(J1,J2, NbrRestant, ScoreS1, Score2),
				      Score1 is ScoreS1+1.
            testerGagnantEtLancerProchaine(J1,J2, NbrRestant, Score1, Score2, noir):-
	                            lancerPlusieursParties(J1,J2, NbrRestant, Score1, ScoreS2),
				      Score2 is ScoreS2+1.
	    testerGagnantEtLancerProchaine(J1,J2, NbrRestant, Score1, Score2, nul):-
	                            lancerPlusieursParties(J1,J2, NbrRestant, Score1, Score2).



								

	% 2
	% Signature : lancerPlusieursParties(+J1, +J2,+NbrPartie,-ScoreIA1,-ScoreIA2)
	% Description : Lance un nombre "NbrPartie" de parties entre J1 et J2 qui peuvent etre iA1,
	%              iA2, ou iA3, et calcule le score de chacun d'eux


            lancerPlusieursParties(_, _,0,0,0) :- !.
            lancerPlusieursParties(iA1, [iA2,NiveauIA2],NbrPartie,ScoreIA1,ScoreIA2) :-
                               initP(Plateau), deroulerJeuIA1IA2(Plateau ,noir ,NiveauIA2,Vainqueur),
			       NbrRestant is NbrPartie-1,
			       testerGagnantEtLancerProchaine(iA1, [iA2,NiveauIA2],NbrRestant,ScoreIA1,ScoreIA2,Vainqueur).


	    lancerPlusieursParties(iA1, [iA3,NiveauIA3],NbrPartie,ScoreIA1,ScoreIA3) :-
                               initP(Plateau), deroulerJeuIA1IA3(Plateau,noir,NiveauIA3,Vainqueur),
			       NbrRestant is NbrPartie-1,
			       testerGagnantEtLancerProchaine(iA1, [iA3,NiveauIA3],NbrRestant,ScoreIA1,ScoreIA3,Vainqueur).

            lancerPlusieursParties([iA2,NiveauIA2], [iA3,NiveauIA3],NbrPartie,ScoreIA2,ScoreIA3) :-
                               initP(Plateau), deroulerJeuIA2IA3(Plateau,noir,NiveauIA2,NiveauIA3,Vainqueur),
			       NbrRestant is NbrPartie-1,
		   testerGagnantEtLancerProchaine([iA2,NiveauIA2], [iA3,NiveauIA3],NbrRestant,ScoreIA2,ScoreIA3,Vainqueur).

		   
		   lancerPlusieursParties([iA2,NiveauIA2], [iA2,Niveau2IA2],NbrPartie,ScoreIA2,Score2IA2) :-
                               initP(Plateau), deroulerJeuIA2IA2(Plateau,noir,NiveauIA2,Niveau2IA2,Vainqueur),
			       NbrRestant is NbrPartie-1,
		   testerGagnantEtLancerProchaine([iA2,NiveauIA2], [iA2,Niveau2IA2],NbrRestant,ScoreIA2,Score2IA2,Vainqueur).


% ------------------------------------------------------------------------
%				    FIN
% ------------------------------------------------------------------------






























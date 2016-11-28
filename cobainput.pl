%%%%%%%%% DYNAMIC FACTS %%%%%%%%
:- dynamic(currloc/1).
:- dynamic(items/3).
:- dynamic(itemcnt/2).
:- dynamic(chapter/1).
:- dynamic(hp/1).
:- dynamic(npcd/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% GAME START CONTROLLER %%%%%%%%%
start :-
	init,
	currloc(X),
	describe(X),nl,
	readinputgeneral.

init :-
	retractall(items(A,B)),								%% Retract all Dynamic Facts
	retractall(currloc(C)),
	retractall(itemcnt(D)),
	retractall(hp(E)),
	asserta(items(bandage,questitems,inventory)),				%% Initialize facts
	asserta(items(chocolate,consumables,table)),
	asserta(currloc(rumah)),
	asserta(itemcnt(0,consumables)),
	asserta(itemcnt(0,tools)),
	asserta(hp(100)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% INPUT CONTROLLER %%%%%%%%%

readinputgeneral :- % READ INPUT FOR MAIN MENU %
	hp(B),
	B > 0,
	repeat,
    write('> '),
    read(Input),
    menu(Input),
    !.

readinputgeneral :- write('you can only quit now').

readinputinvent :- % READ INPUT WHEN OPENING INVENTORY %
	repeat,
    write('> Inventory > '),
    read(Input),
    menuinvent(Input),
    !.

readinputtalk :- % READ INPUT WHEN OPENING INVENTORY %
	repeat,
    write('> Talk > '),
    read(Input),
    menutalk(Input),
    !.

readinputobj :- % READ INPUT TO SELECT ACTIVE OBJECT %
	repeat,
    write('> '),
    read(Input),
    selectActive(Input),
    !.

readinputobjpas(X) :- % READ INPUT TO SELECT PASSIVE OBJECT %
	repeat,
    write('> '),write(X),write(' > '),
    read(Input),
    selectPassive(Input),
    !.

readans :- % READ INPUT TO ANSWER SIDE QUEST %
	repeat,
	write('> answer > '),
	read(Input),
	sideQ(Input),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% GENERAL MENU CONTROLLER %%%%%%%%%
%% General Actions %%
menu(inventory) :-
	write('Which item do you want to see ?'),nl,
	write('- questitems'),nl,
	write('- consumables'),nl,
	write('- tools'),nl,
	write('- cancel'),nl,
	readinputinvent, !, fail.

menu(talk) :-
	currloc(X),
	shownpc(X),
	!,fail.

menu(object) :-
	currloc(X),
	showobj(X),
	!,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  Move Actions  %%
menu(n) :- move(n), nl, !, fail.
menu(s) :- move(s), nl, !, fail.
menu(e) :- move(e), nl, !, fail.
menu(w) :- move(w), nl, !, fail.

menu(quit) :- !.
menu(describe) :- currloc(X), describe(X), !, fail.
menu(help) :- help, !, fail.
menu(y) :- !.
menu(_) :- write('That option is not available'), nl, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% INVENTORY CONTROLLER %%%%%%%%%

%% Inventory Menu %%
menuinvent(questitems) :-
    write('You have '),
    inventlist(questitems),
    write('in your quest items slot'),nl, !, fail.

menuinvent(consumables) :-
    write('You have '),
    inventlist(consumables),
    write('in your consumables items slot'),nl, !, fail.

menuinvent(cancel) :- !.

menuinvent(_) :- write('That option is not available'), nl, fail.

%% Inventory Listing %%
inventlist(X) :- items(Y,X,inventory), write(Y), write(', ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% MAP CONTROLLER %%%%%%%%%

%% PATH LIST %%
% Path (n,a,b) --> north of a is b
/*
	List of Location :
	- rumah
	- jalan1
	- nbhouse
	- jalanraya1
	- tamankota
	- toko
	- kantorpolisi
	- tokoobat
	- jalanraya2
	- jalanraya3
	- mall
	- rumahsakit
	- tokosenjata
	- lab
*/
path(s,rumah,jalan1).
path(n,jalan1,rumah).
path(e,jalan1,nbhouse).
path(s,jalan1,jalanraya1).
path(w,nbhouse,jalan1).
path(n,jalanraya1,jalan1).
path(s,jalanraya1,tamankota).
path(n,tamankota,jalanraya1).
path(s,tamankota,jalanraya2).
path(w,tamankota,kantorpolisi).
path(e,tamankota,toko).
path(e,kantorpolisi,tamankota).
path(w,toko,tamankota).
path(n,jalanraya2,tamankota).
path(s,jalanraya2,rumahsakit).
path(w,jalanraya2,tokoobat).
path(e,jalanraya2,jalanraya3).
path(e,tokoobat,jalanraya2).
path(w,jalanraya3,jalanraya2).
path(s,jalanraya3,tokosenjata).
path(e,jalanraya3,mall).
path(w,mall,jalanraya3).
path(n,tokosenjata,jalanraya3).
path(n,rumahsakit,jalanraya2).
path(s,rumahsakit,lab).
path(n,lab,rumahsakit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Movement Controller %%
move(A) :-
	currloc(X),
	path(A,X,Y),
	retractall(currloc(X)),
	asserta(currloc(Y)),
	write('You\'re now at '), tag(Y),nl,
	describe(Y),nl,
	hp(B),
	C is B - 5,
	retract(hp(B)),
	asserta(hp(C)),
	write('HP : '), write(C),nl,
	!.

move(_) :-
	write('You can\'t go that way!'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% NPC CONTROLLER %%%%%%%%%
%% NPC Location %%
npc(ghost,nbhouse).
npc(girl,tamankota).
npc(pemiliktoko,jalanraya3).
npc(survivor,mall).
npc(doctor,lab).


%% NPC Listing %%
shownpc(X) :-
	write('Talk to :'),nl,
	listnpc(X),
	write('- Cancel'), nl,
	readinputtalk,!.

shownpc(_) :-
	write('There\'s nobody here but yourself'),nl.

listnpc(X) :-
	npc(Y,X),
	write('- '), tag(Y), write(' ('), write(X), write(')'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Talk Menu %%
menutalk(girl) :-
	currloc(Y),
	npc(X,Y),
	dialogue(X),
	hp(B),
	C is 0,
	retract(hp(B)),
	asserta(hp(C)),
	write('HP : '), write(C),nl,
	write('YOU DIE .... '),nl,
	write('GAME OVER'),nl,
	write('Quit?(y)'),nl,
	write('You can only choose yes hahaha!'),nl,
	!.

menutalk(X) :-
	currloc(Y),
	npc(X,Y),
	dialogue(X),
	!,fail.

menutalk(cancel) :- !.

menutalk(_) :-
	write('There\'s nobody with that name...'),nl,fail.

%% NPC Dialogue List %%
dialogue(ghost) :-
	write('Ghost : Ah! My assignment is finally done. Now I can meet my lecturer peacefully in the afterlife.'),nl,
	write('Ghost : Thank you, my neighbor. Be careful with your surrounding and don\'t easily trust anyone.'),nl,
	write('Ghost : <vanished into thin air>'),nl,
	write('You   : (So, should I trust him or not?)').

dialogue(girl) :-
	write('<crying in english>'),nl,
	write('<stopped crying>'),nl,
	write('He suddenly turned around towards you. You felt his hand grabbing your throat.'),nl,
	write('You desperately fought back as he choked you to your death. You died a foolish death.').

dialogue(survivor) :-
	write('You      : (Ah, finally a fellow living human being. Maybe he know something.)'),nl,
	write('You      : Hey! What are...'),nl,
	write('Survivor : <suddenly drew and raised his gun towards you>'),nl,
	write('Survivor : Wh.. who are you?'),nl,
	write('You      : Whoa, whoa.. Lower that gun, would you?'),nl,
	write('Survivor : Just identify yourself!'),nl,
	write('You      : I, I don\'t remember my name...'),nl,
	write('Survivor : Why should I trust someone who cannot even give his name?'),nl,
	write('You      : Because you don\'t have a choice. I know you have been infected too and we must work together to survive this madness.'),nl,
	write('You      : So, put down your gun, okay?'),nl,
	write('Survivor : <hesitantly put his gun down>'),nl,
	write('Survivor : Okay, then. Now tell me what are you doing here.'),nl,
	write('You      : I\'m going to find a cure for those who has been infected but not turned into a zombie yet, like you and me.'),nl,
	write('You      : Tell me what you know.'),nl,
	write('Survivor : Actually, there is a doctor who is trying his best to make that cure. But now he is trapped inside a laboratory in the hospital.'),nl,
	write('Survivor : He had assigned me to collect the ingredients for the cure. I\'m pretty sure he would really appreciate your help too.'),nl,
	write('You      : Give me the list of the ingredients and I will see what I can do.'),nl,
	write('You recieved The Recipe').

dialogue(doctor) :-
	write('You        : (He must be the doctor who is working on the cure)'),nl,
	write('You        : Hey! Are you..'),nl,
	write('The Doctor : <suddenly drew and raised his gun towards you>'),nl,
	write('The Doctor : Wh.. Who are you?'),nl,
	write('You        : Whoa, whoa.. Lower that gun would you?'),nl,
	write('You        : (Somehow this feels like deja vu)'),nl,
	write('The Doctor : Oh, thank god. Another human.'),nl,
	write('You        : I bring the ingredient to complete the cure.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% SIDE QUEST %%%%%%%%%
sideQ(b) :-
	write('Something appeared...'),nl,
	write('It\'s your neighbor\'s ghost!!'),nl,
	write('(you can talk to him)'),!.

sideQ(cancel) :- !.

sideQ(_) :-
	write('Hahaha I told you it\'s hard!! Try again!!'),nl,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% OBJECT CONTROLLER %%%%%%%%%
%%%% ACTIVE OBJECT LOCATION %%%%
activeObj(table,rumah).
/*
activeObj(,jalan1).
*/

activeObj(computer,nbhouse).
/*
activeObj(,jalanraya1).

activeObj(,tamankota).

activeObj(,toko).

activeObj(,kantorpolisi).

activeObj(,tokoobat).

activeObj(,jalanraya2).

activeObj(,jalanraya3).

activeObj(,mall).

activeObj(,rumahsakit).

activeObj(,tokosenjata).

activeObj(,lab).
*/
%%%% ACTIVE OBJ LIST %%%%
showobj(X) :-
	write('Go to :'),nl,
	listobj(X),
	write('- cancel'), nl,
	readinputobj,!.

showobj(_) :-
	write('There\'s nothing here but yourself'),nl.

listobj(X) :-
	activeObj(Y,X),
	write('- '), tag(X), write(' ('), write(Y), write(')'), nl.

%%%% ACTIVE OBJ CONTROLLER %%%%
selectActive(computer):-
	write('(/*RULES*/)'),nl,
	write('append([ ], X, X) :- !.'),nl,
	write('append([A|S], C, [A|D]) :- append(B, C, D).'),nl,
	write('/*this rule is used to concate two lists*/'),nl,
	write('/*Can you find the mistake?*/'),nl,
	write('/*(clue : change S to another alphabet :D*/'),nl,
	readans,
	!.
selectActive(X) :-
	currloc(Y),
	activeObj(X,Y),
	%%%dialogue(X) buat keterangan objek%%%
	%%%tampilin ada objek pasif apa aja di situ%%%
	showobjpas(X),
	!,fail.

selectActive(cancel) :- !.

selectActive(X) :-
	write('There\'s no '),write(X),write(' in this room!'),nl,fail.



%%%% PASSIVE OBJECT LOCATION %%%%
/* rumah */
passiveObj(chocolate,table).
/*
jalan1
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
*/
/*
nbhouse
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

jalanraya1
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

tamankota
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

toko
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

kantorpolisi
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

tokoobat
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

jalanraya2
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

jalanraya3
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

mall
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

rumahsakit
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

tokosenjata
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).

lab
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
passiveObj(chocolate,table).
*/
%%%% PASSIVE OBJ LIST %%%%
showobjpas(X) :-
	write('Take :'),nl,
	listobjpas(X,Y),
	write('- Cancel'), nl,
	readinputobjpas(X),!.

showobjpas(_) :-
	write('There\'s nothing here'),nl.

listobjpas(X,Y) :-
	currloc(Y),
	activeObj(X,Y),
	passiveObj(Z,X),
	items(Z,_,X),
	write('- '), write('('), write(Z), write(')'), nl.

%%%% PASSIVE OBJ CONTROLLER %%%%
selectPassive(X) :-
	currloc(Z),
	activeObj(Y,Z),
	passiveObj(X,Y),
	retract(items(X,V,Y)),
	asserta(items(X,V,inventory)),
	%%%dialogue(X) buat keterangan objek%%%
	%%%tampilin pilihan buat ambil objek ke tangan atau disimpen balik atau ke inventory%%%
	!,fail.

selectPassive(cancel) :- !.

selectPassive(X) :-
	write('There\'s no '),write(X),write(' in this room!'),nl,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% DIALOGUE CONTROLLER %%%%%%%%%
describe(rumah) :-
	write('You\'re at your house.'),nl,
	write('It\'s quite dark here. Luckily it\'s safe here because of your electric fence'),nl,
	write('The door is to the south').

describe(jalan1) :-
	write('There\'re a lot of dead zombies here'),nl,
	write('To the north is your lovely house'),nl,
	write('To the east is your neighbor\'s house').

describe(nbhouse) :-
	write('Your neighbor\'s house look messy.'),nl,
	write('You saw your dead neighbor if front of his still turned on computer, looking at your with his empty eye.'),nl,
	write('The air is reeking of his rotten flesh.'),nl,
	write('To the west is the exit.').

describe(jalanraya1) :-
	write('You are at the main road.'),nl,
	write('There is a car in the middle of the road. It\'s front is smashed and it\'s window broken.'),nl,
	write('To the north is the suburbs.'),nl,
	write('To the south is the City Park.').

describe(tamankota) :-
	write('The City Park was green before. Now it is a deserted place.'),nl,
	write('The only living things are some herbs and flower that once beautifully decorated the park...'),nl,
	write('...and a little girl sitting on the bench. Her quiet but terrifiying cry sent a chill through your spine.'),nl,
	write('The suburbs is to the north.'),nl,
	write('The southern main road is to the south.').

describe(toko) :-
	write('You are at the convenience store.'),nl,
	write('No one\'s there.'),nl,
	write('The exit is to the west.').

describe(kantorpolisi) :-
	write('The police station is no better than other places.'),nl,
	write('The floor is full of broken glass from the shattered window.'),nl,
	write('The stench of death and rotten dead bodies filling the air.'),nl,
	write('The exit is to the east.').

describe(tokoobat) :-
	write('You are at the only drugstore in your town.'),nl,
	write('There are prescription hanging all over the wall.'),nl,
	write('The drug cabinet was unlocked and filled with tools strange to you.').

describe(jalanraya2) :-
	write('You arrived at the southern main road.'),nl,
	write('You saw a person walking towards you. As he was getting closer, you realized it was not a person.'),nl,
	write('It was a zombie. You were thinking to run away, but the thought vanished when you saw another zombie closing from the direction you came from.'),nl,
	write('Another zombie closing from your left and more zombies from your right. You are surrounded.'),nl,
	write('What will you do: flee or fight?').

describe(jalanraya3) :-
	write('You are at the eastern road.'),nl,
	write('You saw a mall to the east and the gun dealer to the south.'),nl,
	write('The southern main road is to the west.').

describe(mall) :-
	write('The mall is nothing but a mess.'),nl,
	write('You suddenly caught a movement. Your heart skipped a beat.'),nl,
	write('You realized there is someone who appeared to be busy searching for something. It seems that he didn\'t realize your presence.'),nl,
	write('You are unsure whether he is still a human or not.'),nl.

describe(rumahsakit) :- write('Hospital').
describe(tokosenjata) :-
	write('It\'s the gun dealer. The owner is there.'),nl,
	write('He is alive and kicking. He doesn\'t seem to be bothered by the situation.').

describe(lab) :- write('Lab').


help :-
	write('          -- List of Commands --'),nl,
	write('n,s,e,w - Move to the selected direction'),nl,
	write('inventory - Opens the inventory'),nl,
	write('object - To show active objects'),nl,
	write('talk - shows a list of npc to talk to'),nl,
	write('describe - shows your current location and description of place'),nl,
	write('examine - To examine objects'),nl,
	write('help - shows this dialogue'),nl,
	write('quit - quits the game'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Line Tag %%
tag(line) :- write('___________________________________________').

%% PLACE TAG %%
tag(rumah) :- write('House').
tag(jalan1) :- write('Suburbs').
tag(nbhouse) :- write('Neighbor\'s House').
tag(jalanraya1) :- write('Main Road').
tag(tamankota) :- write('City Park').
tag(toko) :- write('Convenience Store').
tag(kantorpolisi) :- write('Police Station').
tag(tokoobat) :- write('Drugstore').
tag(jalanraya2) :- write('Southern Main Road').
tag(jalanraya3) :- write('Eastern Main Road').
tag(mall) :- write('Mall').
tag(rumahsakit) :- write('Hospital').
tag(tokosenjata) :- write('Gun Dealer').
tag(lab) :- write('Lab').

%% NPC Tag %%
tag(ghost) :- write('Ghost').
tag(girl) :- write('A Little Girl').
tag(pemiliktoko) :- write('Shop Owner').
tag(survivor) :- write('Survivor').
tag(doctor) :- write('Doctor').

%%%%%%%%% DYNAMIC FACTS %%%%%%%%
:- dynamic(currloc/1).
:- dynamic(items/3).
:- dynamic(itemcnt/1). %jumlah item dalam inventory%
:- dynamic(hp/1).
:- dynamic(sq1/1).
:- dynamic(story/1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% GAME START CONTROLLER %%%%%%%%%
start :-
	init,
	scene(one),nl,
	readinputgeneral.

init :-
	retractall(items),								%% Retract all Dynamic Facts
	retractall(currloc),
	retractall(itemcnt),
	retractall(hp),
	asserta(items([bandage],questitems,inventory)),				%% Initialize facts
	asserta(items([chocolate,cottoncandy],consumables,table)),
	asserta(items([apple],consumables,inventory)),
	asserta(items([],consumables,bloodyfloor)),
	asserta(sq1(0)),
	asserta(story(0)),
	asserta(currloc(rumah)),
	asserta(itemcnt(2)),
	asserta(hp(100)),
	%asserta(itemcnt(0,tools)),%
	scene(prologue),
	nl.

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
    currloc(Y),
	tag(Y),
	fixObj(L,Y),
	write(' > '),
    read(Input),
    selectFix(Input,Y,L),
    !.

readans :- % READ INPUT TO ANSWER SIDE QUEST %
	repeat,
	write('> Answer > '),
	read(Input),
	sideQ(Input),
	!.

readinputconsume :- %READ INPUT TO CONSUME FOOD%
	repeat,
	write('> Consume > '),
	read(Input),
	consumes(Input),
	!.
/*
readinputusequestitem :-
	repeat,
	write('> Use > '),
	read(Input),
	use_qi(Input),
	!.
*/
readinputdrop :- %READ INPUT TO DROP ITEM%
	repeat,
	write('> Drop > '),
	read(Input),
	currloc(X),
	drops(Input,X),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% GENERAL MENU CONTROLLER %%%%%%%%%
%% General Actions %%
menu(inventory) :-
	itemcnt(X),
	write('amount of items : '),write(X),nl,
	write('Which item do you want to see ?'),nl,
	write('- questitems'),nl,
	write('- consumables'),nl,
	write('- tools'),nl,
	write('- consume (only if you want to consume your consumable items)'),nl,
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

menu(save(X)) :-
	save(X),
	write('File Saved!'),nl,
	!,fail.

menu(load(X)) :-
	write('File Loaded!'),nl,
	loadf(X),
	!,fail.

menu(use(X)) :-
	use(X), !,fail.

menu(take(X)) :-
	take(X), !,fail.

menu(drop(X)) :-
	drop(X), !,fail.

menu(sleep):-
	sleep, !,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  Move Actions  %%
menu(n) :- move(n), nl, !, fail.
menu(s) :- move(s), nl, !, fail.
menu(e) :- move(e), nl, !, fail.
menu(w) :- move(w), nl, !, fail.

menu(quit) :- true,!.
menu(describe) :- currloc(X), describe(X), !, fail.
menu(help) :- help, !, fail.
menu(y) :- !.
menu(save) :- !.
menu(_) :- write('That option is not available'), nl, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% INVENTORY CONTROLLER %%%%%%%%%

%% Inventory Menu %%
menuinvent(questitems) :-
    write('You have '),
    inventlist(questitems),
    write('in your quest items slot'),nl,
	/*
		readinputusequestitem,
		*/!.

menuinvent(consumables) :-
    write('You have '),
    inventlist(consumables),
    write('in your consumables items slot'),nl, !, fail.

menuinvent(consume) :-
    write('You have '),
    inventlist(consumables),
    write('in your consumables items slot'),nl,
	readinputconsume,
	!.
/*
menuinvent(drop) :-
	write('You have '),
	inventlist(questitems),
	inventlist(consumables),
	write('in your inventory'),nl,
	readinputdrop,
	!.
*/
menuinvent(cancel) :- !.

menuinvent(_) :- write('That option is not available'), nl, fail.

%% Inventory Listing %%
inventlist(X) :- items(L,X,inventory), printlist(L).

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
	story(Z),
	Z > 0,
	currloc(X),
	path(A,X,Y),
	retractall(currloc),
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
	write('- '), tag(Y), write(' ('), write(Y), write(')'), nl.

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
	write('You   : (So, should I trust him or not?)'),nl.

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
	sq1(X),
	Y is 1,
	retract(sq1(X)),
	asserta(sq1(Y)),
	write('Side Quest Completed!!'),nl,
	write('Something appeared...'),nl,
	write('It\'s your neighbor\'s ghost!!'),nl,
	write('(you can talk to him)'),nl,
	!.

sideQ(cancel) :- !.

sideQ(_) :-
	write('Hahaha I told you it\'s hard!! Try again!!'),nl,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% OBJECT CONTROLLER %%%%%%%%%
%%%% ACTIVE OBJECT LOCATION %%%%
fixObj([table,bloodyfloor],rumah).
fixObj([road],jalan1).
fixObj([computer,dirtyfloor],nbhouse).
fixObj([car,road],jalanraya1).
fixObj([herbs,footpath],tamankota).
fixObj([refrigerator,shelf,messyfloor],toko).
fixObj([monitor,table,slipperyfloor],kantorpolisi).
fixObj([recipes,shelf,cleanfloor],tokoobat).
fixObj([wideroad],jalanraya2).
fixObj([smallroad],jalanraya3).
fixObj([sportstore,floor],mall).
fixObj([guncabinets],tokosenjata).
fixObj([machine],lab).

%%%% ACTIVE OBJ LIST %%%%
showobj(X) :-
	write('There are some things here :'),nl,
	fixObj(L,X),
	listobj(L),
	write('- cancel'), nl,
	readinputobj,!.

showobj(X) :-
	fixobj([],X),
	write('There\'s nothing here, it\'s full of nothingness'),nl,!.

listobj([]).

listobj([Y|T]) :-
	write('- '), tag(Y), write(' ('), write(Y), write(')'), nl,
	listobj(T).

%%%% ACTIVE OBJ CONTROLLER %%%%
selectFix(cancel,_,_) :- !.
selectFix(computer,nbhouse,_):-
	write('(/*RULES*/)'),nl,
	write('append([ ], X, X) :- !.'),nl,
	write('append([A|S], C, [A|D]) :- append(B, C, D).'),nl,
	write('/*this rule is used to concate two lists*/'),nl,
	write('/*Can you find the mistake?*/'),nl,
	write('/*(clue : change S to another alphabet :D*/'),nl,
	readans,
	!.

selectFix(X,_,_) :-
	items(List,_,X),
	listobjpas(List),
	!.

selectFix(_,_,[]) :-
	write('There\'s nothing in there '),nl,fail.


listobjfix([]).
listobjfix([Z|T]) :-
	write('- '), tag(Z),write('('), write(Z), write(')'), nl,
	listobjfix(T).
%%%% PASSIVE OBJECT LOCATION %%%%
/* rumah */
/*objects([chocolate,apple,milk,painkiller,juice,cottoncandy,coffee,tokemasnack,
		softdrink,bandage,molotov,baseballbat,syringes,mortarAndpestle,
		bugspray,alcohol,water,lebarancookie,mangosten,zombiesblood,mistletoe]).*/

%%%% PASSIVE OBJ LIST %%%%
showobjpas(X) :-
	/*write('Take :'),nl,*/
	items(L2,_,X),
	listobjpas(L2),
	write('- Cancel'), nl.

showobjpas(_) :-
	write('There\'s nothing here'),nl.

listobjpas([]).
listobjpas([Z|T]) :-
	write('- '), write(tag(Z)),write('('), write(Z), write(')'), nl,
	listobjpas(T).

%%%% PASSIVE OBJ CONTROLLER %%%%
find(X,Y) :-
	items(L,_,Y),
	ismember(X,L).

take(_) :-
	itemcnt(A),
	A =:= 3,
	write('Your inventory is full'),nl,
	!.

take(X) :-
	find(X,Y),
	items(L,V,Y),
	rmember(X,L,L2),
	%printlist(L2),%
	retract(items(L,V,Y)),
	asserta(items(L2,V,Y)),
	itemcnt(A),
	A < 3, %max item (percobaan dulu angkanya nanti ganti lg) di inventory%
	B is A+1,
	retract(itemcnt(A)),
	asserta(itemcnt(B)),
	retract(items(Li,V,inventory)),
	asserta(items([X|Li],V,inventory)),

	%%%dialogue(X) buat keterangan objek%%%
	%%%tampilin pilihan buat ambil objek ke tangan atau disimpen balik atau ke inventory%%%
	!.

take(X) :-
	write('There\'s no '),write(X),write(' in this room!'),nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% DROP ITEM %%%%

drops(cancel,_) :- !.
drops(_,_) :-
	itemcnt(A),
	A =:= 0,
	write('You don\'t have any item in your inventory!'),nl,
	!,fail.

drops(X,rumah) :-
	currloc(rumah),
	itemcnt(A),
	B is A-1,
	retract(itemcnt(A)),
	asserta(itemcnt(B)),
	find(X,inventory),
	items(L,V,inventory),
	rmember(X,L,L2),
	retract(items(L,V,inventory)),
	asserta(items(L2,V,inventory)),
	retract(items(Li,V,bloodyfloor)),
	asserta(items([X|Li],V,bloodyfloor)), !,
	fail.

drops(X,jalan1) :-
	currloc(jalan1),
	itemcnt(A),
	B is A-1,
	retract(itemcnt(A)),
	asserta(itemcnt(B)),
	items(L,V,inventory),
	rmember(X,L,L2),
	retract(items(L,V,inventory)),
	asserta(items(L2,V,inventory)),
	retract(items(Li,V,road)),
	asserta(items([X|Li],V,road)), !,
	fail.

drops(X,nbhouse) :-
	currloc(jalan1),
	itemcnt(A),
	B is A-1,
	retract(itemcnt(A)),
	asserta(itemcnt(B)),

	items(L,V,inventory),
	rmember(X,L,L2),
	retract(items(L,V,inventory)),
	asserta(items(L2,V,inventory)),
	retract(items(Li,V,dirtyfloor)),
	asserta(items([X|Li],V,dirtyfloor)), !,
	fail.
/*lanjutin lagi buat lokasi lainnya di peta*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% remove list member %%%%
rmember(X,[],[]).
rmember(X,[X|T],T).
rmember(X,[Y|T],[Y|T2]) :- X\==Y.

%%%% printlist element %%%%
printlist([]).
printlist([X]):- write(X),write(' ').
printlist([X|T]) :-
	write(X),write(', '),printlist(T).

%%%% ismember% %%%%
ismember(X,[]):- fail.
ismember(X,[X|T]).
ismember(X,[H|T]) :- X\==H, ismember(X,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
findtype(X,Y) :-
	items(L,Y,_),
	ismember(X,L).

consumes(cancel):- !.

consumes(X) :-
	findtype(X,consumable),
	find(X,inventory),
	items(L,consumables,inventory),
	rmember(X,L,L2),
	retract(items(L,consumables,inventory)),
	asserta(items(L2,consumables,inventory)),
	hp(B),
	C is B+5,
	retract(hp(B)),
	asserta(hp(C)),
	itemcnt(D),
	E is D-1,
	retract(itemcnt(D)),
	asserta(itemcnt(E)),
	write('hmm... delicious'),nl,
	write('HP : '),write(C),nl,
	!,fail.

consumes(X) :-
	write('You cannot consume '), write(X),write(' !'),nl,
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use(X) :-
	items(L,questitems,inventory),
	rmember(X,L,L2),
	retract(items(L,questitems,inventory)),
	asserta(items(L2,questitems,inventory)),
	itemcnt(D),
	E is D-1,
	retract(itemcnt(D)),
	asserta(itemcnt(E)),
	event(X), !.

use(_) :-
	write('You don\'t have that item in your INVENTORY.'), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%% DIALOGUE CONTROLLER %%%%%%%%%
describe(rumah) :-
	write('You\'re at your house.'),nl,
	write('It\'s quite dark here. Luckily it\'s safe here because of your electric fence'),nl,
	write('The door is to the south'),nl.

describe(jalan1) :-
	scene(two),nl,
	write('There\'re a lot of dead zombies here'),nl,
	write('To the north is your lovely house'),nl,
	write('To the east is your neighbor\'s house'),nl.

describe(nbhouse) :-
	scene(three),
	write('Your neighbor\'s house look messy.'),nl,
	write('You saw your dead neighbor in front of his still turned on computer, looking at you with his empty eye.'),nl,
	write('The air is reeking of his rotten flesh.'),nl,
	write('To the west is the exit.'),nl.

describe(jalanraya1) :-
	write('You are at the main road.'),nl,
	write('There is a car in the middle of the road. It\'s front is smashed and it\'s window broken.'),nl,
	write('To the north is the suburbs.'),nl,
	write('To the south is the City Park.'),nl.

describe(tamankota) :-
	sq1(0),
	write('The City Park was green before. Now it is a deserted place.'),nl,
	write('The only living things are some herbs and flower that once beautifully decorated the park...'),nl,
	write('...and a little girl sitting on the bench. Her quiet but terrifiying cry sent a chill through your spine.'),nl,
	write('Maybe I should talk to her.'),nl,
	write('The suburbs is to the north.'),nl,
	write('The southern main road is to the south.'),nl,!.

describe(tamankota) :-
	sq1(1),
	write('The City Park was green before. Now it is a deserted place.'),nl,
	write('The only living things are some herbs and flower that once beautifully decorated the park...'),nl,
	write('...and a little girl sitting on the bench. Her quiet but terrifiying cry sent a chill through your spine.'),nl,
	write('The suburbs is to the north.'),nl,
	write('The southern main road is to the south.'),nl,
	write('(ghost) Be careful with your surrounding and don\'t easily trust anyone ....'),nl.

describe(toko) :-
	write('You are at the convenience store.'),nl,
	write('No one\'s there.'),nl,
	write('The exit is to the west.'),nl.

describe(kantorpolisi) :-
	write('The police station is no better than other places.'),nl,
	write('The floor is full of broken glass from the shattered window.'),nl,
	write('The stench of death and rotten dead bodies filling the air.'),nl,
	write('The exit is to the east.'),nl.

describe(tokoobat) :-
	write('You are at the only drugstore in your town.'),nl,
	write('There are prescription hanging all over the wall.'),nl,
	write('The drug cabinet was unlocked and filled with tools strange to you.'),nl.

describe(jalanraya2) :-
	write('You arrived at the southern main road.'),nl,
	write('You saw a person walking towards you. As he was getting closer, you realized it was not a person.'),nl,
	write('It was a zombie. You were thinking to run away, but the thought vanished when you saw another zombie closing from the direction you came from.'),nl,
	write('Another zombie closing from your left and more zombies from your right. You are surrounded.'),nl,
	write('What will you do: flee or fight?'),nl.

describe(jalanraya3) :-
	write('You are at the eastern road.'),nl,
	write('You saw a mall to the east and the gun dealer to the south.'),nl,
	write('The southern main road is to the west.'),nl.

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

%%%%%%%% STORY %%%%%%%%%
%% Story Tag %%
scene(prologue) :-
	write('My breath short and my heart racing. I dared not to look back. I knew they were right behind me.'),nl,
	write('I was running like it\'s the end of the world, which probably is, keeping my speed and paying no attention'),nl,
	write('to my wounded foot. It seems that I still got some luck since I was able to reach my house, slammed the gate'),nl,
	write('and turned on my electric fence. It will keep them off for some time, God knows how long. It\'s getting darker'),nl,
	write('outside. I fell to the floor in my living room, weak and powerless. My whole body was hurting all over and I'),nl,
	write('realized how painful my wound was. Blood was pooling on the floor, soaked my carpet dark red. It was a miracle'),nl,
	write('that I could be still alive after been infected for some time. '),nl,nl,nl, write('Rise of the Zombie [UNCENSORED]'),nl.

scene(one) :-
	write('For now, I should stop the bleeding. I remembered that I have a BANDAGE in my INVENTORY. I should use it.'),nl,
	write('Type \'inventory.\' to open INVENTORY,'),nl,
	write('then type \'questitems.\' to access QUEST ITEMS,'),nl,
	write('I should make sure I didn\'t drop it.'),nl.


scene(two) :-
	story(1),
	write('I\'m not going to die like those miserable creature. I will cure my infection and survive this ordeal.'),nl,
	write('So I think I should start looking right away. I will start by investigating my neighborhood.'),nl,nl,
	write('You opened the door to the outside. A strong, unpleasant smell of burnt flesh filled your nose.'),nl,
	write('The smell came from a number of burnt zombies that was trying to eat you few minutes ago. You opened'),nl,
	write('the gate and pushed a body of a zombie to clear the way.'),nl,
	story(X), Y is 2, retract(story(X)), asserta(story(Y)).

scene(three) :-
	story(2),
	write('You decided to check on your neighbor. You were not that close with him, but it would be nice to have a living companion.'),nl,
	write('You pushed open the unlocked door. It was dark there. The only light sources are the light from the outside and the flickering'),nl,
	write('light across the room. The light is from a turned on monitor. It is illuminating a familiar figure of your neighbor. Your'),nl,
	write('dead neighbor, to be exact. His flesh was rotten and his eyes was open, staring back into your eyes.'),nl,
	story(X), Y is 3, retract(story(X)), asserta(story(Y)).

% Event Tag %
event(bandage) :-
	story(C),
	C == 0,
	write('You used the bandage to wrap your wounded foot.'),nl,
	write('It should stop the bleeding for now.'),nl,hp(A),write('HP: '),write(A),nl,
	story(X), Y is 1, retract(story(X)), asserta(story(Y)).

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


%% Fixed Obj Tag %%
tag(table) :- write('Table').
tag(bloodyfloor) :- write('Bloody Floor').
tag(computer) :- write('Computer').
tag(dirtyfloor) :- write('Dirty Floor').
tag(refrigerator) :- write('Refrigerator').
tag(shelf) :- write('Shelf').
tag(messyfloor) :- write('Messyfloor').
tag(car) :- write('Car').
tag(road) :- write('Road').
tag(monitor) :- write('Monitor').
tag(undertable) :- write('Under Table').
tag(slipperyfloor) :- write('Slippery Floor').
tag(recipes) :- write('Recipes').
tag(drugshelf) :- write('Drug Shelf').
tag(cleanfloor) :- write('Clean Floor').
tag(wideroad) :- write('Wide Road').
tag(smallroad) :- write('Small Road').
tag(sportstore) :- write('Sport Store').
tag(floor) :- write('Floor').
tag(guncabinets) :- write('Gun Cabinets').
tag(machine) :- write('Machine').
tag(herbs) :- write('Herbs').
tag(footpath) :- write('Foot Path').


%% objek yang bisa diambil tag %%
% consumables %
tag(chocolate) :- write('Chocolate').
tag(apple) :- write('Apple').
tag(milk) :- write('Milk').
tag(painkiller) :- write('Painkiller').
tag(juice) :- write('Juice').
tag(cottoncandy) :- write('Cotton Candy').
tag(coffee) :- write('Coffee').
tag(tokemasnack) :- write('Tokema Snack').

% questitems %
tag(softdrink) :- write('Soft Drink').
tag(bandage) :- write('Bandage').
tag(molotov) :- write('Molotov').
tag(baseballbat) :- write('Baseball Bat').
tag(syringes) :- write('Syringe').
tag(mortarAndpestle) :- write('Mortar and Pestle').

tag(bugspray) :- write('Bug Spray (B*ygon)').
tag(alcohol) :- write('C2H5OH').
tag(water) :- write('Aquadest').
tag(lebarancookie) :- write('Khong Guan').
tag(mangosten) :- write('Mas*in').
tag(zombiesblood) :- write('Zombie\'s Blood').
tag(mistletoe) :- write('Mistletoe').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loadFile(X) :-
    open(X,read,Pita),
    repeat,
    read(Pita,C),
    asserta(C),
    (at_end_of_stream(Pita)),
    close(Pita).

savePos(Pita) :-
	currloc(A),
	write(Pita,'currloc('),
	write(Pita,A),
	write(Pita,').'),
	nl(Pita).

saveHP(Pita) :-
	hp(A),
	write(Pita,'hp('),
	write(Pita,A),
	write(Pita,').'),
	nl(Pita).

saveItems(Pita) :-
	items(L,X,Y),
	write(Pita,'items('),
	write(Pita,L),
	write(Pita,','),
	write(Pita,X),
	write(Pita,','),
	write(Pita,Y),
	write(Pita,').'),
	nl(Pita),fail;true.


saveItemCnt(Pita) :-
	itemcnt(X),
	write(Pita,'itemcnt('),
	write(Pita,X),
	write(Pita,').'),
	nl(Pita).

saveSQ(Pita) :-
	sq1(X),
	write(Pita,'sq1('),
	write(Pita,X),
	write(Pita,').'),
	nl(Pita).

saveStory(Pita) :-
	sq1(X),
	write(Pita,'story('),
	write(Pita,X),
	write(Pita,').'),
	nl(Pita).

/*
:- dynamic(currloc/1).
:- dynamic(items/3).
:- dynamic(itemcnt/1). %jumlah item dalam inventory%
:- dynamic(hp/1).
:- dynamic(sq1/1).
*/

loadf(X) :-
	%% facts to delete %%
	retractall(items),
	retractall(hp),
	retractall(currloc),
	retractall(itemcnt),
	%% end of facts to delete %%
	loadFile(X),nl,nl,nl,nl,
	write('Previously on ROTZ:U'),nl,
	currloc(Y),
	describe(Y),nl.

save(X) :-
	open(X,write,Pita),
	%% add facts to write here %%
	savePos(Pita),
	saveHP(Pita),
	saveItems(Pita),
	saveItemCnt(Pita),
	saveSQ(Pita),
	saveScene(Pita),
	%% add facts to write here %%
	close(Pita).

%% General Actions %%
sleep :-
	currloc(rumah), hpadd(20), hp(X),
	write('You closed your eyes for today, it\'s been a rough day'), nl,
	write('HP :'), nl,
	 write(X); write('You can\'t sleep here, it\'s too dangerous to sleep here'), nl.

hpadd(X) :-
	hp(Y),
	retractall(hp),
	Y+X >= 100 -> asserta(hp(100));
	asserta(hp(Y+X)).

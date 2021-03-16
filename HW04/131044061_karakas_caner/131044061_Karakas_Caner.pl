%%%%%part1%%%%%
flight(istanbul,izmir).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).
flight(rize,istanbul).
flight(rize,van).
flight(van,rize).
flight(van,ankara).
flight(van,istanbul).
flight(ankara,istanbul).
flight(ankara,van).
flight(ankara,konya).
flight(konya,ankara).
flight(konya,antalya).
flight(gaziantep,istanbul).
flight(gaziantep,antalya).
flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).
flight(izmir,istanbul).
flight(izmir,isparta).
flight(isparta,izmir).
flight(isparta,burdur).
flight(burdur,isparta).
flight(edirne,edremit).
flight(edremit,edirne).
flight(edremit,erzincan).
flight(erzincan,edremit).

route(X,Y):-
    flight(X,Y),
    X\=Y.

route(X,Y):-
    flight(X,Z),
    flight(Z,Y),
    X\=Y.

%%%%%part2%%%%%
distance(istanbul,izmir,328).
distance(istanbul,antalya,482).
distance(istanbul,gaziantep,847).
distance(istanbul,ankara,351).
distance(istanbul,van,1262).
distance(istanbul,rize,967).
distance(rize,istanbul,967).
distance(rize,van,373).
distance(van,rize,373).
distance(van,ankara,920).
distance(van,istanbul,1262).
distance(ankara,istanbul,351).
distance(ankara,van,920).
distance(ankara,konya,227).
distance(konya,ankara,227).
distance(konya,antalya,192).
distance(gaziantep,istanbul,847).
distance(gaziantep,antalya,592).
distance(antalya,istanbul,482).
distance(antalya,konya,192).
distance(antalya,gaziantep,592).
distance(izmir,istanbul,328).
distance(izmir,isparta,308).
distance(isparta,izmir,308).
distance(isparta,burdur,24).
distance(burdur,isparta,24).
distance(edirne,edremit,243).
distance(edremit,edirne,243).
distance(edremit,erzincan,1026).
distance(erzincan,edremit,1026).

ways(A,B,Len) :-
       way(A,B,[A],Len).


sroute(A,B,Length) :-
   setof([P,L],ways(A,B,L),Set),
   Set = [_|_],
   minimal(Set,[P,Length]).

way(A,B,P,L) :-
       connectedWay(A,B,L).

way(A,B,Visited,L) :-
       connectedWay(A,C,D),
       C \== B,
       \+member(C,Visited),
       way(C,B,[C|Visited],L1),
       L is D+L1.

minimal([F|R],M) :- minimum(R,F,M).
minimum([],M,M).
minimum([[P,L]|R],[_,M],Min) :- L < M, !, minimum(R,[P,L],Min).
minimum([_|R],M,Min) :- minimum(R,M,Min).

connectedWay(X,Y,L) :-
    distance(X,Y,L),
    distance(Y,X,L).


%%%%%part3%%%%%
when1(102,10).
when1(108,12).
when1(341,14).
when1(455,16).
when1(452,17).

where1(102,z23).
where1(108,z11).
where1(341,z06).
where1(455,207).
where1(452,207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

schedule(X,P,T):-
    enroll(X,C),
    where1(C,P),
    when1(C,T).

usage(P,T):-
    where1(C,P),
    when1(C,T).

% sinif ve ya zaman cakismasini kontrol eder
isSameRoom(X,Y):-
    where1(X,A),
    where1(Y,B),
    A\=B.

isSameTime(X,Y):-  when1(X,A),
                 when1(Y,B),
                 A \= B.
conflict(X,Y):- (not((isSameRoom(X,Y);(isSameTime(X,Y))))).


% iki ogrenci ders olarak karsilasiyor mu?

meet(X,Y):- enroll(X,C1), enroll(Y,C2),
            C1==C2,!.


%%%%%part4%%%%%
element(E,S) :- member(E,S),!.

union1([], L, L) :- !.
union1([H|T], L, R) :-
	memberchk(H, L),!,
	union1(T, L, R).
union1([H|T], L, [H|R]) :-
	union1(T, L, R).

intersect([], _, []) :- !.
intersect([Y|Temp], Left, Intersect) :-
	memberchk(Y, Left),!,
	Intersect = [Y|Right],
	intersect(Temp, Left, Right).
intersect([_|Temp], Left, Right) :-
	intersect(Temp, Left, Right).

difference([],Y,[]).
   difference([X|R],Y,Z) :- member(X,Y),!,difference(R,Y,Z).
   difference([X|R],Y,[X|Z]) :- difference(R,Y,Z).
equivalent(X,X).
  equivalent(X,Y) :- difference(X,Y,[]), difference(Y,X,[]).

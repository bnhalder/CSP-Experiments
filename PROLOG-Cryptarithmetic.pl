% Cryptarithmetic Puzzle Problem
% Biswanath Halder – 21CS91W01  
% Subrangshu Kumar Das – 21CS91W08   
% Utkalika Satapathy – 21CS91P07  
% Method used: Prolog/Logic

% Without delete predicate, it was resulting in Time-Limit-Exceeded (TLE). 
% This is primarily because 10^N is lot bigger than 10!
delete([X], [X|L], L).
delete([X], [Y|L], [Y|L1]) :- delete([X], L, L1).

% Unique predicate is required to determine the unique symbols used in a given Cryptarithmetic problem
unique([X], [X]).
unique([X|Tail], Y) :-
    	member(X, Tail), unique(Tail, Y).
unique([X|Tail], [X|Y]) :- 
    	\+ member(X, Tail), unique(Tail, Y).

% Conc predicate is used to concatenate two lists into a third list
conc([], X, X).
conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

% Number_all predicate is used to create an exhaustive lists of assignments for all the symbols.
number_all([R|Rs],Numbers,[[R,N]|A]) :- 
        member(N,Numbers), delete([N], Numbers, K), number_all(Rs, K, A). 
number_all([],_,[]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TWO + TWO = FOUR
%Query: L = [t,w,o,t,w,o,f,o,u,r], totallegalcoding(L, [0,1,2,3,4,5,6,7,8,9], K)
%Solutions: 7
%Soultion details:- [t, 7], [w, 3], [o, 4], [f, 1], [u, 6], [r, 8]
%Solution details:- [t, 7], [w, 6], [o, 5], [f, 1], [u, 3], [r, 0],
%Solution details:- [t, 8], [w, 3], [o, 6], [f, 1], [u, 7], [r, 2], 
%Solution details:- [t, 8], [w, 4], [o, 6], [f, 1], [u, 9], [r, 2],
%Solution details:- [t, 8], [w, 6], [o, 7], [f, 1], [u, 3], [r, 4], 
%Solution details:- [t, 9], [w, 2], [o, 8], [f, 1], [u, 5], [r, 6], 
%Solution details:- [t, 9], [w, 3], [o, 8], [f, 1], [u, 7], [r, 6]

checkmath([[_,T], [_,W], [_,O], [_,F], [_,U], [_,R]]):-
    	F = 1, member(C1, [0,1]), member(C2, [0,1]), member(C3, [0,1]),
    	O+O =:= R + 10*C1,
    	C1+W+W =:= U + 10*C2,
    	C2+T+T =:= O + 10*C3,
    	F =:= C3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SEND + MORE = MONEY
%Query: L = [s,e,n,d,m,o,r,e,m,o,n,e,y], totallegalcoding(L, [0,1,2,3,4,5,6,7,8,9], K).
%Solutions: 1
%%Solution details:- [s, 9], [e, 5], [n, 6], [d, 7], [m, 1], [o, 0], [r, 8], [y, 2]

%checkmath([[_, S], [_, E], [_, N], [_, D], [_, M], [_, O], [_, R], [_, Y]]) :- 
%    	M = 1, member(C1, [0,1]), member(C2, [0,1]), member(C3, [0,1]), member(C4, [0,1]),
%    	D+E =:= Y+10*C1, 
%    	C1+N+R =:= E+10*C2, 
%    	C2+E+O =:= N+10*C3,  
%    	C3+S+M =:= O+10*C4, 
%   	M =:= C4.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CROSS + ROADS = DANGER
%Query: L = [c,r,o,s,s,r,o,a,d,d,a,n,g,e,r], totallegalcoding(L, [0,1,2,3,4,5,6,7,8,9], K).
%Solutions: 1
%Solution details: [c, 9], [r, 6], [o, 2], [s, 3], [a, 5], [d, 1], [n, 8], [g, 7], [e, 4]

%checkmath([[_, C], [_, R], [_, O], [_, S], [_, A], [_, D], [_, N], [_, G], [_, E]]) :- 
%    	D = 1, member(D, [0, 1]), member(C1, [0, 1]), member(C2, [0, 1]), member(C3, [0, 1]), 
%    	member(C4, [0, 1]), member(C5, [0, 1]),
%    	S+S =:= R + 10*C1,
%    	C1+S+D =:= E + 10*C2,
%    	C2+O+A =:= G+10*C3,
%    	C3+R+O =:= N+10*C4,
%    	C4+C+R =:= A+10*C5,
%    	D =:= C5.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%BARREL + BROOMS = SHOVELS
%Query: L = [b,a,r,r,e,l,b,r,o,o,m,s,s,h,o,v,e,l,s], totallegalcoding(L, [0,1,2,3,4,5,6,7,8,9], K).
%Solutions: 1
%Solution details: [b, 8], [a, 9], [r, 3], [e, 6], [l, 0], [o, 2], [m, 4], [s, 1], [h, 7], [v, 5]

%checkmath([[_, B], [_, A], [_, R], [_, E], [_, L], [_, O], [_, M], [_, S], [_, H], [_, V]|_]) :- 
% 	   	S = 1, member(C1, [0, 1]), member(C2, [0, 1]), member(C3, [0, 1]), 
%    	member(C4, [0, 1]), member(C5, [0, 1]), member(C6, [0, 1]),	
%       L+S =:= S + 10*C1,
%    	C1+E+M =:= L + 10*C2,
%    	C2+R+O =:= E+10*C3,
%     	C3+R+O =:= V+10*C4,
%    	C4+A+R =:= O+10*C5,
%    	C5+B+B =:= H+10*C6,
%	   	S =:= C6.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% legalcoding predicate is used to filter those symbol assignments that will satisfy the 
% constraints of the cryptarithmetic puzzle
legalcoding(L, Numbers, K) :-
   		number_all(L, Numbers, K), checkmath(K).

% allsolutions predicate is used to identify ALL the symbol assignments that wil satisfy the 
% constraints of the cryptarithmetic puzzle
allsolutions(_, _, _, _, _, N, N).
allsolutions(L1, L2, L3, Numbers, K1, Count, Itr):-
    	Itr < Count,
     	conc(L1, L2, Y1), conc(Y1, L3, L4), 
    	%reverse(L4, SL), unique(SL, SLX), reverse(SLX, SX), findall(Z1, legalcoding(SX, Numbers, Z1), K1),
    	reverse(L4, SL), unique(SL, SLX), reverse(SLX, SX), legalcoding(SX, Numbers, K1),
    	length(K1, N1), write(N1), write(' '), write(K1), nl,
    	Temp is Itr+1,
    	allsolutions(L1, L2, L3, Numbers, K1, Count, Temp).

% averagetimetoallsolutions predicate is used to measure the average time taken across #Trials
% of calling allsolutions predicate. T is in msec, Avg is in seconds. 
averagetimetoallsolutions(L1, L2, L3, Domain, Sol, Trials):-
    	statistics(walltime, [_|[_]]),
    	Itr is 0,
    	allsolutions(L1, L2, L3, Domain, Sol, Trials, Itr),
        statistics(walltime, [_|[T]]), 
    	Denom is 1000*Trials,
    	Avg is T/Denom, print(Avg), nl.





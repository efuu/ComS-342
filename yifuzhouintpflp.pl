
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%CODE for the DYNAMIC scoping%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% semantics of a number is the number itself
evaldyn(Expr,Env,Expr) :-
number(Expr).

%%semantics of Variable,just like the symbol? in racket
%%and then when it is atom,find the val in Env
evaldyn(Var,Env,Val) :-
atom(Var),
member(Var,Env,Val).

%%getApplyEnv function and return the ValEnv.
%%first I eval the expr in apply function,
%%then just append it with the Env and then construst a new Env
getApplyEnv([],ValArgs,Env,Env).
getApplyEnv([Expr|RestE],[Var|RestV],Env,ValEnv) :-
evaldyn(Expr,Env,Val),
append([(Var,Val)],Env,E),
getApplyEnv(RestE,RestV,E,ValEnv).

%% semantics of plus operation is addition of
%% of the semantics of the operands
%% Use this pattern to get the semantics of other
%% arithmatic expressions
evaldyn(plus(Expr1, Expr2),Env, Val) :-
evaldyn(Expr1,Env, Val1),
evaldyn(Expr2,Env, Val2),
Val is Val1 + Val2.
%%semantics of minus operation
evaldyn(minus(Expr1, Expr2),Env, Val) :-
evaldyn(Expr1,Env, Val1),
evaldyn(Expr2,Env, Val2),
Val is Val1 - Val2.
%%semantics of mult operation
evaldyn(mult(Expr1, Expr2),Env, Val) :-
evaldyn(Expr1, Env,Val1),
evaldyn(Expr2,Env, Val2),
Val is Val1 * Val2.
%%semantics of div operation
evaldyn(div(Expr1, Expr2),Env, Val) :-
evaldyn(Expr1,Env, Val1),
evaldyn(Expr2,Env, Val2),
Val is Val1 / Val2.

%%this is the question!!!!!! the sequence needed to considered
%%eval the function below.
evaldyn(let((Var, Vexpr), Expr),Env,Val) :-
evaldyn(Vexpr,Env, VVal),
append([(Var, VVal)],Env,E),
evaldyn(Expr,E,Val).

%%eval the semantics fo FExpr
%%I do it for the semantics is completely true!
evaldyn(letp(((Fname,Formalparams), Fexpr),Expr),Env,Val) :-
append([((Fname,Formalparams), Fexpr)],Env,E),
evaldyn(Expr,E,Val).
%%eval the semantics for ApplyF
%%match the function name first,then find the newEnv,
%%then eval the function in env.
evaldyn(apply(Fname,Args),Env,Val) :-
matchFunName(Fname,Env,ValExpr,ValArgs),
getApplyEnv(Args,ValArgs,Env,ValEnv),
evaldyn(ValExpr,ValEnv,Val).
%% Semantics of conditional statement is
%% the semantics of the thenexpression if
%% the semantics of the condition is true
%% otherwise, it is the semantics of the
%% elseexpression

evaldyn((Cond, ThenExpr, ElseExpr),Env, Val) :-
evalconditionD(Cond,Env, BVal),
(BVal == true
->   evaldyn(ThenExpr,Env, Val)
;    evaldyn(ElseExpr,Env, Val)
).

%% Semantics of conditions
%% Semantics of the gt than is true
%% if the semantics of the first expression
%% is greater than the semantics of the second
%% Use the same pattern to realize the semantics
%% of lt, eq.
evalconditionD(gt(Expr1, Expr2),Env, BVal) :-
evaldyn(Expr1,Env, V1),
evaldyn(Expr2,Env, V2),
(V1 > V2
->   BVal = true
;    BVal = false
).
evalconditionD(lt(Expr1, Expr2),Env, BVal) :-
evaldyn(Expr1,Env, V1),
evaldyn(Expr2,Env, V2),
(V1 < V2
->   BVal = true
;    BVal = false
).
evalconditionD(eq(Expr1, Expr2),Env, BVal) :-
evaldyn(Expr1,Env, V1),
evaldyn(Expr2,Env, V2),
(V1 = V2
->   BVal = true
;    BVal = false
).
%% Semantics of an or condition is
%% the disjunction of the semantics of
%% of the disjuncts
%% Use similar patterns for and and or
evalconditionD(or(Cond1, Cond2),Env, BVal) :-
evalconditionD(Cond1,Env, BVal1),
evalconditionD(Cond2,Env, BVal2),
or(BVal1, BVal2, BVal).
evalconditionD(and(Cond1, Cond2),Env, BVal) :-
evalconditionD(Cond1,Env, BVal1),
evalconditionD(Cond2,Env, BVal2),
and(BVal1, BVal2, BVal).
evalconditionD(not(Cond1),Env, BVal) :-
evalconditionD(Cond1,Env, BVal1),
not(BVal1, BVal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%CODE for the STATICS scoping%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%most of the code are similar with the dyn eval,just change the place when apply is used
%% semantics of a number is the number itself
evalstat(Expr,Env,Expr) :-
number(Expr).

%%semantics of Variable
evalstat(Var,Env,Val) :-
atom(Var),
member(Var,Env,Val).

%%matchFunName function and return the ValExpr and the ValArgs.
matchFunName(Fname,[],false,false).
matchFunName(Fname,[((Fname,ValArgs),VarExpr)|_],VarExpr,ValArgs).
matchFunName(Fname,[_|Rest],VarExpr,ValArgs) :-
matchFunName(Fname,Rest,VarExpr,ValArgs).

%%here is new thing!
%%getApplyEnv function and return the Argument that in apply function's argument
%% combining with the new Env
getApplyEnvS([],ValArgs,Env,NewEnv,NewEnv).
getApplyEnvS([Expr|RestE],[Var|RestV],Env,NewEnv,E) :-
evalstat(Expr,Env,Val),
append([(Var,Val)],NewEnv,ResEnv),
getApplyEnvS(RestE,RestV,Env,ResEnv,E).

%%for the satatic that can cut the envirnment elements that appears before function.
getInitialEnv(Fname,[((Fname,_),_)|_],Env,Env).
getInitialEnv(Fname,[_|Rest],EnvCopy,ValEnv) :-
getInitialEnv(Fname,Rest,Rest,ValEnv).

%% semantics of plus operation is addition of
%% of the semantics of the operands
%% Use this pattern to get the semantics of other
%% arithmatic expressions
evalstat(plus(Expr1, Expr2),Env, Val) :-
evalstat(Expr1,Env, Val1),
evalstat(Expr2,Env, Val2),
Val is Val1 + Val2.
%%semantics of minus operation
evalstat(minus(Expr1, Expr2),Env, Val) :-
evalstat(Expr1,Env, Val1),
evalstat(Expr2,Env, Val2),
Val is Val1 - Val2.
%%semantics of mult operation
evalstat(mult(Expr1, Expr2),Env, Val) :-
evalstat(Expr1, Env,Val1),
evalstat(Expr2,Env, Val2),
Val is Val1 * Val2.
%%semantics of div operation
evalstat(div(Expr1, Expr2),Env, Val) :-
evalstat(Expr1,Env, Val1),
evalstat(Expr2,Env, Val2),
Val is Val1 / Val2.

%%eval the function below.
evalstat(let((Var, Vexpr), Expr),Env,Val) :-
evalstat(Vexpr,Env, VVal),
append([(Var, VVal)],Env,E),
evalstat(Expr,E,Val).

%%eval the semantics fo FExpr
evalstat(letp(((Fname,Formalparams), Fexpr),Expr),Env,Val) :-
append([((Fname,Formalparams), Fexpr)],Env,E),
evalstat(Expr,E,Val).
%%eval the semantics for ApplyF
evalstat(apply(Fname,Args),Env,Val) :-
matchFunName(Fname,Env,ValExpr,ValArgs),
getInitialEnv(Fname,Env,Env,E),
getApplyEnvS(Args,ValArgs,Env,E,ValEnv),
evalstat(ValExpr,ValEnv,Val).
%% Semantics of conditional statement is
%% the semantics of the thenexpression if
%% the semantics of the condition is true
%% otherwise, it is the semantics of the
%% elseexpression

evalstat((Cond, ThenExpr, ElseExpr),Env, Val) :-
evalconditionS(Cond,Env, BVal),
(BVal == true
->   evalstat(ThenExpr,Env, Val)
;    evalstat(ElseExpr,Env, Val)
).

%% Semantics of conditions
%% Semantics of the gt than is true
%% if the semantics of the first expression
%% is greater than the semantics of the second
%% Use the same pattern to realize the semantics
%% of lt, eq.
evalconditionS(gt(Expr1, Expr2),Env, BVal) :-
evalstat(Expr1,Env, V1),
evalstat(Expr2,Env, V2),
(V1 > V2
->   BVal = true
;    BVal = false
).
evalconditionS(lt(Expr1, Expr2),Env, BVal) :-
evalstat(Expr1,Env, V1),
evalstat(Expr2,Env, V2),
(V1 < V2
->   BVal = true
;    BVal = false
).
evalconditionS(eq(Expr1, Expr2),Env, BVal) :-
evalstat(Expr1,Env, V1),
evalstat(Expr2,Env, V2),
(V1 = V2
->   BVal = true
;    BVal = false
).
%% Semantics of an or condition is
%% the disjunction of the semantics of
%% of the disjuncts
%% Use similar patterns for and and or
evalconditionS(or(Cond1, Cond2),Env, BVal) :-
evalconditionS(Cond1,Env, BVal1),
evalconditionS(Cond2,Env, BVal2),
or(BVal1, BVal2, BVal).
evalconditionS(and(Cond1, Cond2),Env, BVal) :-
evalconditionS(Cond1,Env, BVal1),
evalconditionS(Cond2,Env, BVal2),
and(BVal1, BVal2, BVal).
evalconditionS(not(Cond1),Env, BVal) :-
evalconditionS(Cond1,Env, BVal1),
not(BVal1, BVal).
not(true, false).
not(false, true).
%% Truth-facts for the or
or(true, _, true).
or(_, true, true).
or(false, false, false).
%% Truth-facts for the and
and(true, true, true).
and(_, false, false).
and(false, _, false).
%% Truth-facts for the not

append([], L, L).
append([X|Xs], L, [X|Ys]) :-
append(Xs, L, Ys).

%%semantics of member
member(X, [(X,Result)|_],Result) :- !. %% X is a member of a list if it is at the head
member(X, [_|Rest],Result) :-
member(X, Rest, Result).

%%test
%%evalstat(letp(((f1,[n]),(gt(n,1),plus(apply(f1,[minus(n,1)]),apply(f1,[minus(n,2)])),(gt(n,0),1,0))),apply(f1,[6])),[(x,6)],N)
%%return 8
%%evalstat(letp(((f1,[x]),(gt(x,0),mult(x,apply(f1,[minus(x,1)])),1)),apply(f1,[x])),[(x,4)],N)
%%return 24
%%evaldyn(let((x,1),letp(((f1,[]),minus(x,1)),letp(((g1,[y]),let((x,y),apply(f1,[]))),apply(g1,[100])))),[],N)
%%return 99

%%evaldyn(letp (((f1,[x]),let((x, minus(x,1)),x)), apply(f1,[x])))





matching([], [], []).
matching([(X, Y)|Rest], [X|Xs], [Y|Ys]) :-
matching(Rest, Xs, Ys).

addtoend(Var,[],[Var]).
addtoend(Var,[First|Rest],[First|RestVar]) :-
addtoend(Var,Rest,RestVar).
member(X,[])
member(X,[X|_]).
member(X,[_|Rest]) :-
member(X,Rest).
nodupall(List,ValList) :-
nodup(List,[],Val),
reverse(Val,ValList).

nodup([],L,L).
nodup([X|Rest],Ltest,L) :-
(member(X,Ltest)
-> nodup(Rest,Ltest,L)
;  nodup(Rest,[X|Ltest],L)).


minmax([X,X],[Y|Rest]) :-
min(X,Y,[Y|Rest]).

min(Temp,Temp,[]).
min(Var,Temp,[X|Rest]) :-
(Temp>X
->  Var1 is X,
min(Var,Var1,Rest)
;   min(Var,Temp,Rest)).

max([X],X) :-!.
max([X|Xs],N) :- max(Xs,N),N>X.
max([X|Xs],X) :- max(Xs,Y),X>=Y.


mypred(Z, X, Y) :-
(Z == 3
-> X = 1,
Y=2
; X = 2,
Y=1
).
reverse([], []).
reverse([X|Xs], L) :- reverse(Xs, Ys), append(Ys, [X], L).


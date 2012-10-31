-module(point).
-export([new/2, distance/2, move/3]).

-include("point.hrl").

new(X, Y) ->
	#point{x=X, y=Y}.

distance(#point{x=X1, y=Y1}, #point{x=X2, y=Y2}) ->
	math:sqrt(sqr(X2-X1) + sqr(Y2-Y1)).

sqr(X) ->
	X*X.

move(#point{x=X, y=Y}, Dx, Dy) ->
	#point{x=(X+Dx), y=(Y+Dy)}.

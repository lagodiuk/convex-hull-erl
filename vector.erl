-module(vector).
-export([vector_from_points/2, pseudoscalar_product/2]).

-include("vector.hrl").
-include("point.hrl").

vector_from_points(#point{x=X1, y=Y1}, #point{x=X2, y=Y2}) ->
	#vector{x=(X2-X1), y=(Y2-Y1)}.

pseudoscalar_product(#vector{x=X1, y=Y1}, #vector{x=X2, y=Y2}) ->
	((X1*Y2) - (X2*Y1)).

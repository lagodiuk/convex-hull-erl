-module(convexhull_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("point.hrl").

triangle_test() ->
	Input = [point(0,0), point(1,2), point(2,0), point(1,1)],
	Expected = [point(1,2), point(2,0), point(0,0)],
	Actual = convexhull:build(Input),
	?assertMatch(Expected, Actual).

pentagon_test() ->
	Input = [point(1,3), point(2,5), point(2,4), point(2,1), point(3,4), point(3,2), point(4,5), point(5,3)],
	Expected = [point(1,3), point(2,5), point(4,5), point(5,3), point(2,1)],
	Actual = convexhull:build(Input),
	?assertMatch(Expected, Actual).

point(X, Y) ->
	#point{x=X, y=Y}.

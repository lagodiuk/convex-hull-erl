-module(convexhull_make).
-export([make/0]).

%% tip for executing tests after successful compilation is from Stackoverflow
%% http://stackoverflow.com/questions/1582818/what-tool-do-you-use-to-build-an-erlang-program/1583412#1583412
make() ->
	case make:all() of
		error ->
			error;
		_ ->
			convexhull_test:test()
	end.

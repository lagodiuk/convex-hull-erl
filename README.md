convex-hull-erl
===============

Implementation of [Graham's scan](http://en.wikipedia.org/wiki/Graham_scan) method for computing 2D convex hull in Erlang.

### usage ###

Example, from from erlang-shell:

```erlang
1> c(convexhull_make).
{ok,convexhull_make}
2> 
2> convexhull_make:make().
Recompile: convexhull
Recompile: convexhull_test
Recompile: point
Recompile: vector
  All 2 tests passed.
ok
3> 
3> rr(convexhull).
[point,vector]
4> 
4> ConvexHull = convexhull:build([point:new(0,0), point:new(1,1), point:new(2,0), point:new(1,2)]).
[#point{x = 1,y = 2},
 #point{x = 2,y = 0},
 #point{x = 0,y = 0}]
```

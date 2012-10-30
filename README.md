convex-hull-erl
===============

Implementation of [Graham's scan](http://en.wikipedia.org/wiki/Graham_scan) method for computing 2D convex hull in Erlang.

## Usage ##

Example, from from erlang-shell:

```erlang
1> c(point).
{ok,point}
2> 
2> c(vector).
{ok,vector}
3> 
3> c(convexhull).
{ok,convexhull}
4> 
4> rr(convexhull).
[point,vector]
5> 
5> Point = fun(X, Y) -> #point{x=X, y=Y} end.
#Fun<erl_eval.12.82930912>
6> 
6> Points = [Point(0,0), Point(1,1), Point(2,0), Point(1,2)].
[#point{x = 0,y = 0},
 #point{x = 1,y = 1},
 #point{x = 2,y = 0},
 #point{x = 1,y = 2}]
7> 
7> ConvexHull = convexhull:build(Points).
[#point{x = 1,y = 2},
 #point{x = 2,y = 0},
 #point{x = 0,y = 0}]
```

-module(test).
-export([render/0, render/1, render/2]).

render(         ) -> render([],  []).
render(Env      ) -> render(Env, []).

render(Env, Opts) ->
{ok, ["<!DOCTYPE html>
",
  jaderl_rt:'if'(Env, Opts, false, {binop,2,'==',{var,2,"v1",false},{var,2,"v2",false}}, fun(Env, Opts) ->
["xxx
"] end,
fun(Env, Opts) ->
["yyy
"] end),
""]}.
%--- END OF MODULE ---%
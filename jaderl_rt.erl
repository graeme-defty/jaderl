-module(jaderl_rt).
-include("jaderl.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([ get_var/3,
          filter/3,
          'case'/5,
          forloop/6,
          'if'/6,
          env/1,
          tester/0]).

%% -------- GET_VAR
% --- Handles the 'dot' notation for variables
% x.y => x is undefined => undefined
%             proplist  => proplists:get_value
%             module    => x:y/0
get_var(Name, Env, Escape)  ->
  Nameparts = string:tokens(Name,"."),
  Val = get_subvals(Env, Nameparts),
  Dispval = format(Val),
  case Escape of       %%%%% REAL ESCAPING NEEDED! %%%%%
    true  ->  Dispval;
    false ->  Dispval
  end.

% ---
get_subvals(Base,      [])    -> Base;
get_subvals([],  _)           -> "undefined";
get_subvals(undefined,  _)    -> "undefined";
get_subvals("undefined",_)    -> "undefined";
get_subvals(Base, [H|T])   when is_list(Base)  ->
  io:format("In get_subvals with list - ~p,~p~n",[Base,H]),
    case proplists:get_value(H, Base) of
      undefined   ->
        case proplists:get_value(list_to_atom(H), Base) of
          undefined   ->
            case io_lib:printable_list(Base) of
              true  ->  get_subvals(try_module(list_to_atom(Base), H), T);
              false ->  "undefined"
            end;
          Val         ->  get_subvals(Val, T)
        end;
      Val         ->  get_subvals(Val, T)
    end;

get_subvals(Base, [H|T])   when is_atom(Base)  ->
  get_subvals(try_module(Base, H), T);

get_subvals(Base, [H|T])   when is_tuple(Base)  ->
  get_subvals(try_module(element(1,Base), H), T).


try_module(Module, Func) when is_atom(Module) ->
  io:format("In try_module with atom - ~p,~p~n",[Module,Func]),
    case code:is_loaded(Module) of
	    {file,_} ->
            try_func(Module, Func);
      _ ->
 	      case code:load(Module) of
            {file, _}   ->  try_func(Module, Func);
            false       ->  undefined
        end
    end;
try_module(_Module,_Func)  -> undefined.

try_func(Module, Func) when is_list(Func) ->  try_func(Module, list_to_atom(Func));

try_func(Module, Func) ->
  io:format("In try_func with - ~p,~p~n",[Module,Func]),
  case lists:member({Func,0}, Module:module_info(exports)) of
    true  ->  apply(Module, Func, []);
    false ->  undefined
  end.



format(X) when is_integer(X)  ->  integer_to_list(X);
format(X)                     ->  X.

%% -------- FILTER
filter(reverse, _Opts, Text)  ->
  lists:reverse(Text).

%% -------- CASE
'case'(Env,_Opts, Name, Default, Whens)  ->
  Val = get_var(Name, Env, false),
  case Val of
    "undefined" ->  "undefined";
    _         ->  case_pick_when(Val, Default, Whens)
  end.

case_pick_when(_Val, Dflt, [])                      ->  Dflt;
case_pick_when(Val, _Dflt, [#'when'{value=HW_val, content=HW_body}|_TW]) 
                                when Val == HW_val  ->  HW_body;
case_pick_when(Val,  Dflt, [_HW| TW])               ->  case_pick_when(Val, Dflt, TW).

%% -------- FOR / EACH
forloop(Env, Opts, Key, Value, Var, Content)  ->
  Val = get_var(Var, Env, false),
  case get_var(Var, Env, false) of   % What to do depends on the type of data
    "undefined"           -> "";
    Val when is_list(Val) -> forloop_dolist(Env, Opts, Key, Value, Val, Content);
    _                     ->  ""
  end.

% Data is list - what to do depends on whether a key is passed
forloop_dolist(Env, Opts, Key, Value, Data, Content_func) ->
  case Key == "" of
    true  ->
      forloop_nokey(Env, Opts, Value, Data, Content_func);
    false ->
      forloop_withkey(Env, Opts, Key, Value, Data, Content_func)
  end.

% Fields dont include key - simple one-variable loop
forloop_nokey(Env, Opts, Value, Data, Content_func)  ->
  forloop_nokey(Env, Opts, Value, Data, Content_func, []).

forloop_nokey(_Env,_Opts,_Value, [],  _Content_func, Acc)  -> lists:reverse(Acc);
forloop_nokey(Env, Opts, Value, [H|T], Content_func, Acc)  ->
  forloop_nokey(Env, Opts, Value, T, Content_func,[Content_func([{Value,H}|Env], Opts)|Acc]).

% Fields include a key - do we have doubles
forloop_withkey(Env, Opts, Key, Value, Data, Content_func)  ->
  forloop_withkey(Env, Opts, Key, Value, Data, Content_func, []).

forloop_withkey(_Env,_Opts,_Key,_Value, [],  _Content_func, Acc)  -> lists:reverse(Acc);
forloop_withkey(Env, Opts, Key, Value, [H|T], Content_func, Acc)  ->
  case is_tuple(H) andalso size(H) == 2 of
    true  ->
      forloop_withkey(Env, Opts, Key, Value, T, Content_func,
                    [Content_func([{Key,element(1,H)},{Value,element(2,H)}|Env], Opts)|Acc]);
    false ->
      forloop_withkey(Env, Opts, Key, Value, T, Content_func,
                    [Content_func([{Key,""},{Value,H}|Env], Opts)|Acc])
  end.

%% -------- IF / UNLESS

'if'(Env, Opts, Reverse, Expr, Tfun, Ffun)  ->
  Rslt = eval(Env, Opts, Expr),
  case Rslt == Reverse of
    false ->  Tfun(Env, Opts);
    true  ->  Ffun(Env, Opts)
  end.

% --- basic expressions ---

eval(_Env,_Opts, X)  when  is_number(X)          ->  X;
eval(_Env,_Opts, X)  when  is_record(X, string)  ->  X#string.string;
eval( Env,_Opts, X)  when  is_record(X, var)     ->  get_var(X#var.name, Env, false);
eval( Env, Opts, X)  when  is_record(X, binop)   ->
    Lval = eval(Env, Opts, X#binop.lterm),
    Rval = eval(Env, Opts, X#binop.rterm),
    eval_op(X#binop.op, Lval, Rval);
eval(_Env,_Opts, X)  ->
    Msg = io_lib:format("Dont know how to evaluate ~p~n", [X]),
    io:format("~p~n",[Msg]),
    Msg.

eval_op('&&', Lval, Rval)  ->  bool_value(Lval) andalso bool_value(Rval);
eval_op('||', Lval, Rval)  ->  bool_value(Lval) orelse  bool_value(Rval);

eval_op(_,"undefined",_Rval)-> false;
eval_op(_,_Lval,"undefined")-> false;
eval_op('==', Lval, Rval)  ->  Lval == Rval;
eval_op('!=', Lval, Rval)  ->  Lval /= Rval;
eval_op('<',  Lval, Rval)  ->  Lval < Rval;
eval_op('<=', Lval, Rval)  ->  Lval =< Rval;
eval_op('>',  Lval, Rval)  ->  Lval > Rval;
eval_op('>=', Lval, Rval)  ->  Lval >= Rval.

bool_value(undefined)   ->  false;
bool_value("undefined") ->  false;
bool_value([])          ->  false;
bool_value(_)           ->  true.


  

% ===== UNIT TESTS =====

get_var_test_() ->
  [
    ?_assertEqual("undefined",  get_var("fld", [], false)),
    ?_assertEqual("undefined",  get_var("fl-", [{"fld", "val"}], false)),
    ?_assertEqual("val",        get_var("fld", [{"fld", "val"}  ], false)),
    ?_assertEqual("val",        get_var("fld.sub", [{"fld", [{"sub", "val"}]}  ], false)),
    ?_assertEqual("val",        get_var("fld.sub", [{fld, [{sub, "val"}]}  ], false)),
    ?_assertEqual("undefined",  get_var("fl-.sub", [{"fld", [{"sub", "val"}]}  ], false)),
    ?_assertEqual("undefined",  get_var("fld.su-", [{fld, [{sub, "val"}]}  ], false)),
    ?_assertEqual("test val",   get_var("mymod.tester", [{mymod, "jaderl_rt"}  ], false)),
    ?_assert(true)
  ].

%'case'(Env,_Opts, Name, Default, Whens)  ->
case_test_() ->
  [
    ?_assertEqual("undefined",  'case'(env(1), [], "varx", "xxx",
                                                    [])),
    ?_assertEqual("xxx",        'case'(env(1), [], "var1", "xxx",
                                                    [])),
    ?_assertEqual("matched",    'case'(env(1), [], "var1", "yyy",
                                                    [#'when'{ value="value-1",
                                                              content="matched"}])),
    ?_assert(true)
  ].

eval_test_()  ->
  [
  % basic terms
    ?_assertEqual(3,          eval(env(1), [], 3)),
    ?_assertEqual(3.1415,     eval(env(1), [], 3.1415)),
    ?_assertEqual("hello",    eval(env(1), [], #string{string="hello"})),
    ?_assertEqual("value-1",  eval(env(1), [], #var{name="var1"})),
  % comparators
    ?_assertEqual(true,       eval(env(1), [],
                              #binop{op='==',
                                      lterm=#var{name="var1"},
                                      rterm=#string{string="value-1"}})),
    ?_assertEqual(false,      eval(env(1), [],
                              #binop{op='==',
                                      lterm=#var{name="var1"},
                                      rterm=#string{string=23}})),

    ?_assert(true)
  ].


if_test_() ->
  [
    ?_assertEqual(true, 'if'(env(1), [], false,
                              #binop{op='==',lterm=#var{name="var1"},
                                            rterm=#string{string="value-1"}},
                              fun tfunc/2,
                              fun ffunc/2)),
    ?_assertEqual(false,'if'(env(1), [], true,
                              #binop{op='==',lterm=#var{name="var1"},
                                            rterm=#string{string="value-1"}},
                              fun tfunc/2,
                              fun ffunc/2)),
    ?_assertEqual(false,'if'(env(1), [], false,
                              #binop{op='==',lterm=#var{name="var1"},
                                            rterm=#string{string="value-x"}},
                              fun tfunc/2,
                              fun ffunc/2)),
    ?_assertEqual(true,'if'(env(1), [], true,
                              #binop{op='==',lterm=#var{name="var1"},
                                            rterm=#string{string="value-x"}},
                              fun tfunc/2,
                              fun ffunc/2)),
    ?_assertEqual(true,'if'(env(1), [], false,
                              #binop{op='>=',lterm=#var{name="var1"},
                                            rterm=#string{string="value-0"}},
                              fun tfunc/2,
                              fun ffunc/2)),
    ?_assert(true)
  ].

% --- helpers and data ---

env(1) ->  [{"var1", "value-1"}];
env(2) ->  [{"var1", [a,b,c,d]}].

%identity(X) ->  X.
tfunc(_,_)  ->  true.
ffunc(_,_)  ->  false.

tester() -> "test val".

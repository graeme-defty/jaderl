-module(jaderl).
-export([comp_file/2, compile/2, compile/3]).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% with reference to the docs at https://github.com/visionmedia/jade#readme ...
%%
%% Things that are implemented and working:
%%  -   tags
%%  -   nesting
%%  -   tag text on same line
%%  -   tag text using '|'
%%  -   tag text using '.'
%%  -   tags which accept *only* text don't need leading '|'
%%  -   attributes
%%  -   block expansion
%%  -   single line comments
%%  -   block comments
%%  -   conditional comments
%%  -   tag nesting
%%  -   inline html
%%  -   doctypes
%%  -   Filters
%%  -   Case
%%  -   Iteration
%%  -   Conditionals

%%
%% Things that are not yet implemented:
%%  -   Code
%%  -   Template Inheritance
%%  -   Block Append / prepend
%%  -   Includes
%%  -   Mixins
%%  -   
%%  -   

-define(IS_ALPHA(C),        ((C >= $a andalso C =< $z)
                      orelse (C >= $A andalso C =< $Z) 
                      orelse (C >= $0 andalso C =< $9))).

-define(IS_DIGIT(C),         (C >= $0 andalso C =< $9)).

-define(IS_ALPHANUMERIC(C), (?IS_ALPHA(C)
                      orelse ?IS_DIGIT(C))).

-define(IS_NAME_CHAR(C),     (?IS_ALPHANUMERIC(C)
                      orelse  C == $_ orelse  C == $-)).

-define(IS_VARNAME_CHAR(C),  (?IS_NAME_CHAR(C)
                      orelse  C == $.)).

-define(IS_ATTNAME_CHAR(C),   ((C >= $a andalso C =< $z)
                        orelse (C >= $A andalso C =< $Z) 
                        orelse (C >= $0 andalso C =< $9)
                        orelse  C == $_ orelse  C == $- orelse  C == $\:)).

-include("jaderl.hrl").

% --- parse a file and create an erlang module ---

compile(File, OutModule) ->
    compile(File, OutModule, []).

compile(File, OutModule, Options) ->
	{Indent, Src} = open_file(File),
    Erl = [preamble(atom_to_list(OutModule), File), gen(parse(Src),2), postscript()],
    {Mod, Bin} = dynamic_compile:from_string(binary_to_list(iolist_to_binary(Erl))),
    code:load_binary(Mod, [], Bin),
    case proplists:get_value(out_dir, Options) of
        undefined -> ok;
        OutDir -> 
            RootName = filename:rootname(File),
            BeamFile = filename:join(OutDir, RootName ++ ".beam"),
            file:write_file(BeamFile, Bin),
            ok
    end.

comp_file(Inf)  ->  comp_file(Inf, Inf).

comp_file(Inf, Outf) when is_atom(Inf)  ->  comp_file(atom_to_list(Inf), Outf);
comp_file(Inf, Outf) when is_atom(Outf) ->  comp_file(Inf, atom_to_list(Outf));
comp_file(Inf, Outf) ->
    Inf_name = Inf++".jade",
    Outf_name = Outf++".erl",
	{_Indent, Src} = open_file(Inf_name),
%    io:format("~p~n",[Src]),
    Erl = gen(parse(Src),2),
    io:format("~s",[Erl]),
    file:write_file(Outf_name, [preamble(Inf, Inf_name),Erl,postscript()]).


preamble(ModuleName, FileName)  ->
  [ "-module(",ModuleName,").\n",
    "-export([render/0, render/1, render/2, source/0, dependencies/0]).\n",
    "-export([translatable_strings/0]).\n",
    "\n",
    "source(              ) -> \"", FileName, "\".\n",
    "dependencies(        ) -> [].\n",
    "translatable_strings() -> [].\n",
    "render(              ) -> render([],  []).\n",
    "render(Env           ) -> render(Env, []).\n",
    "\n",
    "render(Env, Opts) ->\n{ok, "].

postscript()    ->
  [ "}.\n",
    "%--- END OF MODULE ---%"].

% --- Compile support functions ---
% Returns:
%	- the indent of the first line
%	- an array of strings
open_file(Filename)   ->
    case file:read_file(Filename) of
    {ok, SrcBin}	->
	    case string:tokens(binary_to_list(SrcBin), "\n") of
	    	[]		->	{0,[]};
			[H|T]	->	{Spaces, _} = count_spaces(H),
						{Spaces, [H|T]}
		end;
    {error, Reason}	->  
    	io:format("Error ~p reading file ~p~n", [Reason, Filename]),
    	{0, []}
	end.

% --- Runtime support functions ---
get_var(Var, Env, _Escape)   ->
    proplists:get_value(Var, Env).


% --- parse group of lines as a block ---

parse(Lines)    ->
    {AST, [], _New_lineno} = p_block(Lines,1,0,[]),
    AST.

% --- parse block line by line ---

p_block([], Lineno, _Indent, Acc)   ->  
    {lists:reverse(Acc),[],Lineno};

p_block([First|Rest],Lineno,Indent,Acc) -> 
    {Spaces, Content} = count_spaces(First),
    case Spaces < Indent of
        true    ->
            {lists:reverse(Acc), [First|Rest], Lineno};
        false   ->
            {Item, After_Item, New_Lineno} = p_line(Content,Rest,Lineno,Spaces),
            NewAcc = case Item of
                      nil ->    Acc;
                      _   ->    [Item|Acc]
                     end,
            p_block(After_Item, New_Lineno, Indent, NewAcc)
    end.

% ---
p_content_block([First|Rest],Lineno,Indent,Acc) -> 
    {Spaces, Content} = count_spaces(First),
    case Spaces =< Indent of
        true    ->  {[], [First|Rest], Lineno}; % no indented lines
        false   ->  Item = do_string(Content, Lineno),
                    p_content_block_rest(Rest, Lineno+1, Spaces, [Item | Acc])
    end.

p_content_block_rest([],Lineno,_Indent,Acc)  ->   {lists:reverse(Acc), [], Lineno};
p_content_block_rest([First|Rest],Lineno,Indent,Acc) -> 
    {Spaces, _Content} = count_spaces(First),
    case Spaces < Indent of
        true    ->  
            {lists:reverse(Acc), [First|Rest], Lineno};
        false   ->
%            {Item, After_Item, New_Lineno} = p_line(Content,Rest,Lineno,Spaces),
            Item = do_string(lists:nthtail(Indent,First),Lineno),
            p_content_block_rest(Rest, Lineno+1, Indent, [Item|Acc])
    end.


% --- parses a line ---
% Returns {Node, Rest, Lineno}
% - Node is the node for the construct parsed
% - Rest is the lines remaining after parsing
% - Lineno is the number of the first line of Rest

% DOCTYPE
p_line([$!,$!,$!,$ |T],Rest,Lineno,_Indent) ->  {{doctype,Lineno,T}, Rest, Lineno+1};
p_line([$!,$!,$!|T],Rest,Lineno,_Indent)    ->  {{doctype,Lineno,T}, Rest, Lineno+1};
p_line([$d,$o,$c,$t,$y,$p,$e,$ |T],Rest,Lineno,_Indent)   ->  {{doctype,Lineno,T}, Rest, Lineno+1};
p_line([$d,$o,$c,$t,$y,$p,$e|T],Rest,Lineno,_Indent)   ->  {{doctype,Lineno,T}, Rest, Lineno+1};

% CASE
p_line([$-,$c,$a,$s,$e |T],Rest,Lineno,Indent) ->
        {Name, _After_name} = get_name(skip_spaces(T)),
        {Default, Whens, NewRest, NewLineno} = get_whens(Rest,Lineno+1,Indent,[]),
        {{'case',Lineno,Name,Default,Whens}, NewRest, NewLineno};

% FOR / EACH
p_line([$-,$e,$a,$c,$h |T],Rest,Lineno,Indent) ->
  p_line([$-,$f,$o,$r |T],Rest,Lineno,Indent);
p_line([$-,$f,$o,$r |T],Rest,Lineno,Indent)    ->
    {Name1, After_name1} = get_name(skip_spaces(T)),
    AN1 = skip_spaces(After_name1),
    case hd(AN1) of
      $,  ->
        Key = Name1,
        {Value, After_name2} = get_name(skip_spaces(tl(AN1)));
      _   ->
        Key = "",
        {Value, After_name2} = {Name1, AN1}
    end,
    {Varname, After_varname} = get_name(skip_spaces(skip_in(skip_spaces(After_name2)))),
    {Content, NewRest, New_lineno} = get_when_content(After_varname, Rest, Lineno, Indent),
    For = #for{lineno=Lineno, key=Key, value=Value, varname=Varname, content=Content},
    {For, NewRest, New_lineno};

% IF / UNLESS
p_line([$-,$i,$f |T],Rest,Lineno,Indent) ->
    {Expr, AfterExpr, NewLineno1} = parse_expr(skip_spaces(T), Lineno),
    {Tleg, NewRest1, NewLineno2}  = get_when_content(AfterExpr, Rest, NewLineno1, Indent),
    {Fleg, NewRest2, NewLineno3}  = get_else(NewRest1, NewLineno2, Indent),
    If = #'if'{lineno=Lineno, reverse=false, expr=Expr, tleg=Tleg, fleg=Fleg },
    {If, NewRest2, NewLineno3};

p_line([$-,$u,$n,$l,$e,$s,$s |T],Rest,Lineno,Indent) ->
    {Expr, AfterExpr, NewLineno1} = parse_expr(skip_spaces(T), Lineno),
    {Tleg, NewRest1, NewLineno2}  = get_when_content(AfterExpr, Rest, NewLineno1, Indent),
    {Fleg, NewRest2, NewLineno3}  = get_else(NewRest1, NewLineno2, Indent),
    If = #'if'{lineno=Lineno, reverse=true, expr=Expr, tleg=Tleg, fleg=Fleg },
    {If, NewRest2, NewLineno3};
%    {Expr, AfterExpr} = get_expression(skip_spaces(T), Lineno),
%    {Tleg, NewRest1, NewLineno1} = get_when_content(AfterExpr, Rest, Lineno, Indent),
%    {Fleg, NewRest2, NewLineno2} = get_else(NewRest1, NewLineno1, Indent),
%    If = #'if'{lineno=Lineno, reverse=true, expr=Expr, tleg=Tleg, fleg=Fleg },
%    {If, NewRest2, NewLineno2};

% INCLUDE
p_line([$-,$i,$n,$c,$l,$u,$d,$e |T],Rest,Lineno,Indent)    ->
        {Name, _After_name} = get_varname(skip_spaces(T)),	% TODO - need a new 'get_filename/1'
        io:format("Including ~p~n",[Name]),
        Prefix = lists:duplicate(Indent, $\ ),
        {_, Lines} = open_file(Name),
        {nil, [Prefix ++ L || L <- Lines]++Rest, Lineno+1};

% TAG
p_line([$#|T],Rest,Lineno,Indent) ->
    p_tag_body('div', [$#|T], Rest, Lineno, Indent);

p_line([$.|T],Rest,Lineno,Indent) ->
    p_tag_body('div', [$.|T], Rest, Lineno, Indent);

p_line([H|T],Rest,Lineno,Indent) when ?IS_NAME_CHAR(H) ->
    {Tagname, After_tag} = get_tag([H|T]),
    p_tag_body(Tagname, After_tag, Rest, Lineno, Indent);

% CONTENT
p_line([$|,$ |T],Rest,Lineno,_Indent)   ->  {do_string(T, Lineno), Rest, Lineno+1};
p_line([$||T],Rest,Lineno,_Indent)      ->  {do_string(T, Lineno), Rest, Lineno+1};

% COMMENT
p_line([$/,$/,$i,$f,$ |T],Rest,Lineno,Indent)    ->
        {Content, NewRest, New_lineno} = p_block(Rest,Lineno+1,Indent+1,[]),
        {{comblock,Lineno,T,Content}, NewRest, New_lineno};
p_line([$/,$/,$-|T],Rest,Lineno,Indent)    ->  
    case string:strip(T) of
        []  ->  {_Content, NewRest, New_lineno} = p_block(Rest,Lineno+1,Indent+1,[]),
                {nil, NewRest, New_lineno};
        _   ->  {nil, Rest, Lineno+1}
    end;
p_line([$/,$/,$ |T],Rest,Lineno,Indent)    ->
    case string:strip(T) of
        []  ->  {Content, NewRest, New_lineno} = p_block(Rest,Lineno+1,Indent+1,[]),
                {{comblock,Lineno,[],Content}, NewRest, New_lineno};
        _   ->  {{comment,Lineno,T}, Rest, Lineno+1}
    end;
p_line([$/,$/|T],Rest,Lineno,Indent)       ->
    case string:strip(T) of
        []  ->  {Content, NewRest, New_lineno} = p_block(Rest,Lineno+1,Indent+1,[]),
                {{comblock,Lineno,[],Content}, NewRest, New_lineno};
        _   ->  {{comment,Lineno,T}, Rest, Lineno+1}
    end;

% FILTER
p_line([$: |T],Rest,Lineno,Indent)    ->
        {Name, _After_name} = get_name(T),
        {Content, NewRest, New_lineno} = p_content_block(Rest,Lineno+1,Indent,[]),
        {{filter,Lineno,Name,[],Content}, NewRest, New_lineno};

% UNKNOWN
p_line(First,Rest,Lineno,_Indent) ->
    io:format("Error: ~p, ~p, ~p~n",[First,Rest,Lineno]),
    {{err,Lineno,lists:flatten(io_lib:format("Dont know how to parse a ~s",[[First]]))}, Rest, Lineno+1}.

p_tag_body(Tagname, After_tag, Rest, Lineno, Indent)  ->
    {ID, After_id} = get_id(After_tag),
    Atts1 = case ID of
                []  ->  [];
                _   ->  [{"id",ID}]
           end,

    {Classes, After_classes} = get_classes(After_id),
    Atts2 = case Classes of
                []  ->  Atts1;
                _   ->  Atts1 ++ [{"class",Classes}]
            end,

    {Atts, After_atts, NewRest} = get_atts(After_classes, Atts2, Rest),

    {Content, After_Content, New_lineno} = 
        case After_atts of
          [$:,$ |T] ->
              {C,R,L} = p_line(skip_spaces(T),NewRest,Lineno,Indent),
              {[C],R,L};
          _         ->
              case string:strip([$.|After_atts]) == ".." orelse
                  Tagname == script orelse
                  Tagname == style of
                true  ->  p_content_block(NewRest,Lineno+1,Indent,[]);
                false ->
                  case After_atts of
                      []    ->  p_block(NewRest,Lineno+1,Indent+1,[]);
                      [$ ]  ->  p_block(NewRest,Lineno+1,Indent+1,do_string(" ",Lineno));
                      [$ |T]->  p_block(NewRest,Lineno+1,Indent+1,[do_string(T, Lineno)]);
                      S     ->  p_block(NewRest,Lineno+1,Indent+1,[do_string(S, Lineno)])
                  end
              end            
    end,
    Tag = {tag, Lineno, Tagname, Atts, Content},
    {Tag, After_Content, New_lineno}.


%--- assist functions ---

count_spaces(L) ->  count_spaces(L,0).
count_spaces([$ |T], C)    ->  count_spaces(T,C+1);
count_spaces(X,      C)    ->  {C, X}.

get_tag(S)          ->  
    {Tag_name, After_tag} = get_name(S),
    {list_to_atom(Tag_name), After_tag}.

get_name(S)         ->  get_name(S, []).
get_name([H|T], Acc) when ?IS_NAME_CHAR(H)
                    ->  get_name(T, [H|Acc]);
get_name(S, Acc)    ->  {lists:reverse(Acc), S}.

get_attname(S)      ->  get_attname(S, []).
get_attname([H|T], Acc) when ?IS_ATTNAME_CHAR(H)
                    ->  get_attname(T, [H|Acc]);
get_attname(S, Acc) ->  {lists:reverse(Acc), S}.

get_varname(S)         ->  get_varname(S, []).
get_varname([H|T], Acc) when ?IS_VARNAME_CHAR(H)
                    ->  get_varname(T, [H|Acc]);
get_varname(S, Acc)    ->  {lists:reverse(Acc), S}.

skip_spaces([$ |T]) ->  skip_spaces(T);
skip_spaces(S)      ->  S.

get_id([$#|T])  ->  get_name(T);
get_id(T)       ->  {[], T}.

get_quoted(S,Q)     ->  get_quoted(S,Q,[]).
get_quoted(S,Q,Acc) ->
    case S of
        []          ->  {lists:reverse(Acc),[]};
        [$\\,C|T]   ->  get_quoted(T,Q,[C|Acc]);
        [C|T]       ->  
            case lists:member(C,Q) of
                true    ->  {lists:reverse(Acc),T};
                false   ->  get_quoted(T,Q,[C|Acc])
            end
    end.

% -
get_classes(S)  ->  get_classes(S, []).

get_classes(S=[$.,$ |_], Acc)   ->  got_classes(S, Acc);
get_classes(S=[$.], Acc)        ->  got_classes(S, Acc);
get_classes([$.|T], Acc)        ->  {Class, After_class} = get_name(T),
                                    get_classes(After_class, [Class | Acc]);
get_classes(S, Acc)             ->  got_classes(S, Acc).

got_classes(T, [])  ->  {[], T};
got_classes(T, Acc) ->  {string:join(lists:reverse(Acc)," "), T}.

% -
get_atts(S, Acc, Rest) ->
    case S of
      [$(|T]    -> parse_atts(T,Acc,Rest); 
      _         -> {Acc, S, Rest}
    end.

% On entry: S points to the first char of the possible att
%           Acc contains the attributes so far
%           Rest contains the following lines of source
% Returns:  {Atts, After_atts, NewRest}
parse_atts(S,Acc,Rest)  ->
    case S of
        [$)|T]  -> {lists:reverse(Acc), skip_spaces(T), Rest};
        [$,|T]  -> parse_atts(skip_spaces(T), Acc, Rest);
        []      -> 
            case Rest of
                []  ->              {lists:reverse(Acc), [], []};
                [First|NewRest] ->  parse_atts(skip_spaces(First), Acc, NewRest)
            end;
        [C|_] when ?IS_ATTNAME_CHAR(C) ->  
            {Att, After_att} = parse_att(S, Rest),
            parse_atts(After_att, [Att|Acc], Rest);
        [_|T]   ->  
            parse_atts(T, Acc, Rest)
    end.

%Parse a single att 
% On entry: S points to the first char of the name
% Returns:  {Att, After_att} - where att is a proplist entry
parse_att(S, _Rest) ->
    {Name, After_name} = get_attname(S),
    After_name_xs = skip_spaces(After_name),
    case After_name_xs of
        [$=|T]  ->  {Val, After_val} = parse_att_val(skip_spaces(T)),
                    {{Name, Val},After_val};
        _       ->  {{Name,nil},After_name_xs}
    end.

parse_att_val(S)    ->
    case S of
        []      ->  "";
        [$'|T]  ->  get_quoted(T,"\'");
        [$"|T]  ->  get_quoted(T,"\"");
        _       ->  get_quoted(S," )")
    end.

% Takes a string and a lineno performs escaping and returns either a string item
% or a list of string and var items
do_string(S, L)                  ->  do_string(S, L, [], []).
do_string([], _L, [], [Item])    ->  Item;
do_string([], _L, [], Acc)       ->  lists:reverse(Acc);
do_string([], L, Sacc, Acc)      ->  do_string([], L, [], [do_string_string(L, Sacc)|Acc]);
do_string([$\\], L, Sacc, Acc)   ->  do_string([], L, [$\\|Sacc], Acc);
do_string([$\\,H|T], L,Sacc,Acc) ->  do_string(T, L, [H|Sacc], Acc);
do_string([$#,${], L, Sacc, Acc) ->  do_string([], L, [$#,${|Sacc], Acc);
do_string([$#,${|T], L,Sacc, Acc)->  Acc1 = [do_string_string(L, Sacc)|Acc], 
                                     {T2, Acc2} = do_string_var(T, L, [], Acc1, "true"),
                                     do_string(T2, L, [], Acc2);
do_string([$!,${], L, Sacc, Acc) ->  do_string([], L, [$#,${|Sacc], Acc);
do_string([$!,${|T], L,Sacc, Acc)->  Acc1 = [do_string_string(L, Sacc)|Acc], 
                                     {T2, Acc2} = do_string_var(T, L, [], Acc1, "false"),
                                     do_string(T2, L, [], Acc2);
do_string([H|T], L, Sacc, Acc)   ->  do_string(T, L, [H|Sacc], Acc).

do_string_string(L, S)  ->  {string, L, lists:reverse(S)}.

do_string_var(S, L, Sacc, Acc, Escape)  ->  
    Acc1 = case Sacc of
            []  ->  Acc;
            _   ->  [do_string_string(L, Sacc) | Acc]
           end,
    {Name, NewS} = get_varname(S),
    Acc2 = [ {var,L,Name,Escape} | Acc1 ],
    {tl(NewS), Acc2}.


% Parses the 'when' clauses of a 'case' construct
% Returns - {Default, Whens, NewRest, New_lineno}
%             where - each When in the list of Whens is a when record
%             and   - Default is the ocntent for the default case
get_whens([First|Rest],Lineno,Indent,Acc) ->
    {Spaces, Content} = count_spaces(First),
    case Spaces =< Indent of
        true    ->
            {[],lists:reverse(Acc), [First|Rest], Lineno};
        false   ->
            case Content of

              [$-,$w,$h,$e,$n |T] ->
                {When, NewRest, NewLineno} = get_a_when(T,Rest,Lineno,Spaces),
                NewAcc = case When of
                          nil ->    Acc;
                          _   ->    [When|Acc]
                         end,
                get_whens(NewRest, NewLineno, Indent, NewAcc);

              [$-,$d,$e,$f,$a,$u,$l,$t |T]  ->
                {Default, NewRest, NewLineno} = get_default(T,Rest,Lineno,Spaces),
                {Default,lists:reverse(Acc), NewRest, NewLineno};

              _ -> {[],lists:reverse(Acc), [First|Rest], Lineno}

            end
    end.


get_a_when(T, Rest, Lineno, Indent)    ->
   {Value, After_val} = parse_att_val(skip_spaces(T)),
   {Content, NewRest, New_lineno} = get_when_content(After_val,Rest,Lineno,Indent),
    When = #'when'{lineno=Lineno, value=Value, content=Content},
    {When, NewRest, New_lineno}.


% Parses the 'default' clause of a 'case' construct (if present)
% Returns - {Default, NewRest, New_lineno}
%             where Default is the content of the default clause

get_default(T,Rest,Lineno,Indent)    ->
    {Content, NewRest, New_lineno} = get_when_content(T,Rest,Lineno,Indent),
    {Content, NewRest, New_lineno}.

% Common function to parse the content of a 'when' or a 'default' clause
% of a 'case' structure
% Returns - {Content, NewRest, New_lineno}

get_when_content(T,Rest,Lineno,Indent)  ->
  case T of
    [$:,$ |BlockX] ->
        {C,R,L} = p_line(skip_spaces(BlockX),Rest,Lineno,Indent),
        {[C],R,L};
    _         ->
        case string:strip([$.|T]) == ".." of
          true  ->  p_content_block(Rest,Lineno+1,Indent,[]);
          false ->
            case T of
                []        ->  p_block(Rest,Lineno+1,Indent+1,[]);
                [$ ]      ->  p_block(Rest,Lineno+1,Indent+1,do_string(" ",Lineno));
                [$ |NewT] ->  p_block(Rest,Lineno+1,Indent+1,[do_string(NewT, Lineno)]);
                S         ->  p_block(Rest,Lineno+1,Indent+1,[do_string(S, Lineno)])
            end
        end            
  end.

skip_in([$i,$n | T])->  T;
skip_in(S)          ->  S.


% Parses an expression for an 'if' or 'case' construct
% Returns - {Expression, NewRest, New_lineno}
get_expression(T, Lineno) ->
  {Lterm, After_lterm} = get_term(T),
  {Op, After_op} = get_op(skip_spaces(After_lterm)),
  {Rterm, After_rterm} = get_term(skip_spaces(After_op)),
  {#'expr'{op=Op, lineno=Lineno, lterm=Lterm, rterm=Rterm}, After_rterm}.

get_op([$=,$= |T]) ->  {'==', T};
get_op([$!,$= |T]) ->  {'!=', T};
get_op([$<,$= |T]) ->  {'<=', T};
get_op([$<    |T]) ->  {'<',  T};
get_op([$>,$= |T]) ->  {'>=', T};
get_op([$>    |T]) ->  {'>',  T};
get_op([$i,$n |T]) ->  {'in', T};
get_op(S)         ->  {'==', S}.


% Parses a term in an expression, which can be:
%   - a number (integer or float with optional leading + or -
%   - a string quoted by " or ' with / as escape char
%   - a variable name
% Returns the term and the remaining tail of the string
%   {Term, Tail}

get_term(S=[H|_]) when ?IS_DIGIT(H) 
                orelse H == $- 
                orelse H == $+  ->  get_number(S);
get_term([H|T]) when   H == $'
                orelse H == $"  ->  {S, After} = get_quoted(T, [H]),
                                    {#string{lineno=0,string=S}, After};
get_term(S)                     ->  {Name, After} = get_attname(S),
                                    {#var{lineno=0,name=Name,escape=false}, After}.

parse_expr(S, Lineno) ->
  parse_bool_expr(S, Lineno).


% Parses a boolean expression, which can be:
%   - && - logical 'and' or
%   - || - logical 'or'
% Returns the term, the remaining tail of the string and the current lineno
%   {Term, Tail, Lineno}

parse_bool_expr(S, Lineno) ->
  {Lterm, After_lterm, Newlineno} = parse_rel_expr(skip_spaces(S), Lineno),
  case parse_boolop(skip_spaces(After_lterm)) of
    {nil, T}  ->  {Lterm, T, Newlineno};
    {Op, T}   ->  {Rterm, Tail, Lineno} = parse_rel_expr(T, Lineno),
                  {#binop{lineno=Newlineno,
                          op=Op,
                          lterm=Lterm,
                          rterm=Rterm}, Tail, Newlineno}
  end.


% Parses a relational expression, which can be:
%   - == - equals
%   - != - not equals
%   - etc
% Returns the term, the remaining tail of the string and the current lineno
%   {Term, Tail, Lineno}

parse_rel_expr(S, Lineno) ->
  {Lterm, After_lterm, Newlineno} = parse_basic_expr(skip_spaces(S), Lineno),
  case parse_relop(skip_spaces(After_lterm)) of
    {nil, T}  ->  {Lterm, T, Newlineno};
    {Op, T}   ->  {Rterm, Tail, Lineno} = parse_rel_expr(T, Lineno),
                  {#binop{lineno=Newlineno,
                          op=Op,
                          lterm=Lterm,
                          rterm=Rterm}, Tail, Newlineno}
  end.


% Parses a basic expression, which can be:
%   - a number (integer or float with optional leading + or -
%   - a string quoted by " or ' with / as escape char
%   - a variable name
% Returns the term and the remaining tail of the string
%   {Term, Tail}

parse_basic_expr(S=[H|_], Lineno)
                when ?IS_DIGIT(H)
                orelse H == $- 
                orelse H == $+  ->  {Number, After} = get_number(S),
                                    {Number, After, Lineno};
parse_basic_expr([H|T], Lineno) 
                when   H == $'
                orelse H == $"  ->  {S, After} = get_quoted(T, [H]),
                                    {#string{lineno=Lineno,string=S}, After, Lineno};
parse_basic_expr(S, Lineno)     ->  {Name, After} = get_attname(S),
                                    {#var{lineno=Lineno,name=Name,escape=false}, After, Lineno}.


parse_relop([$=,$= |T]) ->  {'==', T};
parse_relop([$!,$= |T]) ->  {'!=', T};
parse_relop([$<,$= |T]) ->  {'<=', T};
parse_relop([$<    |T]) ->  {'<',  T};
parse_relop([$>,$= |T]) ->  {'>=', T};
parse_relop([$>    |T]) ->  {'>',  T};
parse_relop([$i,$n |T]) ->  {'in', T};
parse_relop(S)          ->  {nil, S}.


parse_boolop([$&,$& |T]) ->  {'&&', T};
parse_boolop([$|,$| |T]) ->  {'||', T};
parse_boolop(S)          ->  {nil, S}.



% ---
get_number([$-|S])  ->  get_int_part(S, 0, -1);
get_number([$+|S])  ->  get_int_part(S, 0,  1);
get_number(S)       ->  get_int_part(S, 0,  1).

get_int_part([H|T], Acc, Sgn)  when ?IS_DIGIT(H) ->  get_int_part(T, Acc*10 +H -$0, Sgn);
get_int_part([H|T], Acc, Sgn)  when H == $.      ->  get_dec_part(T, Acc, 10, Sgn);
get_int_part(S,     Acc, Sgn)                    ->  {Acc*Sgn, S}.

get_dec_part([H|T], Acc, Div, Sgn)
                          when ?IS_DIGIT(H) ->  get_dec_part(T, Acc + (H-$0)/Div, Div*10, Sgn);
get_dec_part(S,     Acc,_Div, Sgn)               ->  {Acc*Sgn, S}.



% ---

% Parses the 'else' clauses of an 'if' or 'unless' construct
% Returns - {ElseBody, NewRest, New_lineno}
get_else([First|Rest],Lineno,Indent) ->
    {Spaces, Content} = count_spaces(First),
    case Spaces < Indent of
        true    ->
            {[], [First|Rest], Lineno};
        false   ->
            case Content of
              [$-,$e,$l,$s,$e|T]  ->  get_when_content(T,Rest,Lineno,Spaces);
              _                   ->  {[], [First|Rest], Lineno, Spaces} 
            end
    end.



% === unit tests ===
skip_spaces_test_() ->
  [
    ?_assertEqual("abc", skip_spaces("abc")) ,
    ?_assertEqual("def", skip_spaces("   def")) ,
    ?_assertEqual("", skip_spaces("   ")) ,
    ?_assertEqual("", skip_spaces("")) ,
    ?_assert(true)
  ].



count_spaces_test_() ->
  [
    ?_assertEqual({0,"abc"}, count_spaces("abc")) ,
    ?_assertEqual({3,"abc"}, count_spaces("   abc")) ,
    ?_assertEqual({5,""},    count_spaces("     ")) ,
    ?_assert(true)
  ].

get_tag_test_() ->
  [
    ?_assertEqual({abc,"   def"}, get_tag("abc   def")) ,
    ?_assert(true)
  ].

get_number_test_() ->
  [
    ?_assertEqual({123, " abc"},  get_number("123 abc")) ,
    ?_assertEqual({123, "xyz"},   get_number("+123xyz")) ,
    ?_assertEqual({-321, ""},     get_number("-321")) ,
    ?_assertEqual({1.54, ""},     get_number("1.54")) ,
    ?_assertEqual({-4.56, ""},    get_number("-4.56")) ,
    ?_assert(true)
  ].

get_term_test_() ->
  [
    ?_assertEqual({-3.14, " abc"},get_term("-3.14 abc")) ,
    ?_assertEqual({#string{lineno=0,string="abc"}, " xyz"}, get_term("\"abc\" xyz")) ,
    ?_assertEqual({#string{lineno=0,string="abc"}, ""},     get_term("\"abc\"")) ,
    ?_assertEqual({#string{lineno=0,string="abc"}, ""},     get_term("\"abc")) ,
    ?_assertEqual({#var{lineno=0,name="abc",escape=false}, " xyz"}, get_term("abc xyz")) ,
    ?_assert(true)
  ].

parse_basic_expr_test_() ->
  [
    ?_assertEqual({-3.14, " abc", 23},
                    parse_basic_expr("-3.14 abc", 23)) ,

    ?_assertEqual({#string{lineno=23,string="abc"}, " xyz", 23},
                    parse_basic_expr("\"abc\" xyz", 23)) ,

    ?_assertEqual({#string{lineno=23,string="abc"}, "", 23},
                    parse_basic_expr("\"abc\"", 23)) ,

    ?_assertEqual({#string{lineno=23,string="abc"}, "", 23},
                    parse_basic_expr("\"abc", 23)) ,

    ?_assertEqual({#var{lineno=23,name="abc",escape=false}, " xyz", 23},
                    parse_basic_expr("abc xyz", 23)) ,
    
    ?_assert(true)
  ].

parse_rel_expr_test_() ->
  [
    ?_assertEqual({#binop{lineno=23, op='==', 
                          lterm = -3.14,
                          rterm = #var{lineno=23,
                                        name="abc",
                                        escape=false}},
                   "def", 23},
                    parse_rel_expr("-3.14 == abc def", 23)) ,
    
    ?_assert(true)
  ].

parse_bool_expr_test_() ->
  [
    ?_assertEqual({#binop{lineno=23, op='&&',
                          lterm = #binop{lineno=23, op='<=', 
                                          lterm = -3.14,
                                          rterm = #var{lineno=23,
                                                        name="abc",
                                                        escape=false}},
                          rterm = #binop{lineno=23, op='>=', 
                                          lterm = #var{lineno=23,
                                                        name="def",
                                                        escape=false},
                                          rterm = #string{lineno=23,
                                                          string="xyz"}}},
                   "stu", 23},
                    parse_bool_expr("-3.14 <= abc && def >= \"xyz\"  stu", 23)) ,

    ?_assert(true)
  ].

-define(PARSETEST(X), ?_assertEqual(data(X,ast), parse(data(X,src)))).

p_block_test_() ->
  [
    ?PARSETEST(b1),
    ?PARSETEST(b2),
    ?PARSETEST(b3),
    ?_assert(true)
  ].

p_tag_test_() ->
  [
    ?_assertEqual(  data(t1,ast), parse(data(t1,src))),
    ?_assertEqual(  data(t2,ast), parse(data(t2,src))),
    ?_assertEqual(  data(t3,ast), parse(data(t3,src))),
    ?_assertEqual(  data(t4,ast), parse(data(t4,src))),
    ?_assertEqual(  data(t5,ast), parse(data(t5,src))),
    ?_assertEqual(  data(t6,ast), parse(data(t6,src))),
    ?_assertEqual(  data(t7,ast), parse(data(t7,src))),
    ?_assertEqual(  data(t8,ast), parse(data(t8,src))),
    ?_assertEqual(  data(t9,ast), parse(data(t9,src))),
    ?_assert(true)
  ].

p_content_test_() ->
  [
    ?_assertEqual(  data(c1,ast), parse(data(c1,src))),
    ?_assertEqual(  data(c2,ast), parse(data(c2,src))),
    ?_assertEqual(  data(c3,ast), parse(data(c3,src))),
    ?_assertEqual(  data(c4,ast), parse(data(c4,src))),
    ?_assertEqual(  data(c5,ast), parse(data(c5,src))),
    ?_assertEqual(  data(c6,ast), parse(data(c6,src))),
    ?_assertEqual(  data(c7,ast), parse(data(c7,src))),
    ?_assertEqual(  data(c8,ast), parse(data(c8,src))),
    ?_assert(true)
  ].

p_interpolation_test_() ->
  [
    ?_assertEqual(  data(i1,ast), parse(data(i1,src))),
    ?_assertEqual(  data(i2,ast), parse(data(i2,src))),
    ?_assertEqual(  data(i3,ast), parse(data(i3,src))),
    ?_assertEqual(  data(i4,ast), parse(data(i4,src))),
    ?_assertEqual(  data(i5,ast), parse(data(i5,src))),
    ?_assert(true)
  ].

p_comment_test_() ->
  [
    ?_assertEqual(  data(m1,ast), parse(data(m1,src))),
    ?_assertEqual(  data(m2,ast), parse(data(m2,src))),
    ?_assertEqual(  data(m3,ast), parse(data(m3,src))),
    ?_assertEqual(  data(m4,ast), parse(data(m4,src))),
    ?_assertEqual(  data(m5,ast), parse(data(m5,src))),
    ?_assert(true)
  ].

p_doctype_test_() ->
  [
    ?_assertEqual(  data(d1,ast), parse(data(d1,src))),
    ?_assertEqual(  data(d2,ast), parse(data(d2,src))),
    ?_assertEqual(  data(d3,ast), parse(data(d3,src))),
    ?_assert(true)
  ].

p_filter_test_() ->
  [
    ?_assertEqual(  data(f1,ast), parse(data(f1,src))),
    ?_assertEqual(  data(f2,ast), parse(data(f2,src))),
    ?_assert(true)
  ].

p_case_test_() ->
  [
    ?_assertEqual(  data(ca1,ast), parse(data(ca1,src))),
    ?_assertEqual(  data(ca2,ast), parse(data(ca2,src))),
    ?_assert(true)
  ].

p_for_test_() ->
  [
    ?_assertEqual(  data(e1,ast), parse(data(e1,src))),
    ?_assertEqual(  data(e2,ast), parse(data(e2,src))),
    ?_assert(true)
  ].

p_if_test_() ->
  [
    ?PARSETEST(u1),
    ?PARSETEST(u2),
    ?PARSETEST(u3),
    ?_assert(true)
  ].



% HTML Syntax Tree definition
%
% {comment, Lineno, Text}
% {comblock, Lineno, Iftext, Content}
% {doctype, Lineno, Text}
% {tag, Lineno, Tagname, Attlist, ContentList}
% {string, Lineno, String}
% {var, Lineno, Name, Escape}
% {filter, Lineno, Name, Opts, Content}
% {case, Lineno, VarName, Default, [<when1>,...]}
%     where <whenn> = {when, Value, Lineno, Content}
% {'when', Lineno, Value, Content=[]}).
% {for, Lineno, Value, Key, Varname, Content=[]}).
% {'if', Lineno, Reverse, Expr, Tleg, Fleg}).
%      where Expr =  {'expr',   {lineno, op, lterm, rterm}).

% =======================================================

%
% ------- generate HTML from HST ------
%
gen(HST, Step)          ->  ["[\"", lists:reverse(gen(HST, Step, 0, []), "\"]")].

gen([], _Step, _Level, Acc)    ->  Acc;
gen([H|T], Step, Level, Acc)   ->  
gen(T, Step, Level, gen(H, Step, Level, Acc));

%--- Doctype
gen({doctype,_Lineno,S}, Step, Level, Acc)    ->
    [[prefix(Step,Level), doctype_text(S), "\n"] | Acc];

%--- tag
gen({tag, _Lineno, Tagname, Atts, Content}, Step, Level, Acc) ->
    [[prefix(Step,Level), gen_close_tag(Tagname)] |
        gen(Content, Step, Level+1,
            [ [prefix(Step,Level), gen_open_tag(Tagname, Atts)] | Acc])];

%--- text content
gen({string,_Lineno,S}, Step, Level, Acc)    ->
    [[prefix(Step,Level), S, $\n] | Acc];

%---variable
gen({var,_Lineno,Name,Escape}, Step, Level, Acc)    ->
    [ ["\","++prefix(Step,Level)++"jaderl_rt:get_var(\""++Name++"\", Env, "++Escape++"),\n\""] | Acc];

%---comment
gen({comment,_Lineno,S}, Step, Level, Acc)    ->
    [[prefix(Step,Level), "<!-- ", S, " -->\n"] | Acc];

% --- (conditional) comment block
gen({comblock,_Lineno,Iftext,Content}, Step, Level, Acc)    ->
    Opentag = case Iftext of
                []  ->  "<!--\n";
                _   ->  "<--[if " ++ Iftext ++ "]\n"
              end,
    [[prefix(Step,Level), "-->\n"] | 
        gen(Content, Step, Level+1, 
            [ [prefix(Step,Level), Opentag] | Acc])];

%---filter
gen({filter,_Lineno,Name,Opts,Content}, Step, Level, Acc)    ->
    [ ["\",\n",
       prefix(Step,Level),
       "jaderl_rt:filter(",
       Name, ", ",
       io_lib:format("~p",[Opts]),
       ", [\"", gen(Content, Step, Level+1, []), "\"]),\n\""]
    | Acc];

%---case
gen({'case',_Lineno,Name,Default,Whens}, Step, Level, Acc)    ->
    [ ["\",\n",
       prefix(Step,Level+1),
       "jaderl_rt:'case'(Env, Opts, \"", Name, "\", \"", gen(Default,Step,Level+2,[]), "\", [\n",
       string:join(gen_whens(Whens, Step, Level+2),",\n"),
       "]),\"\n" ]
    | Acc];

%---for / each
gen({for,_Lineno,Value, Key, Varname, Content}, Step, Level, Acc)    ->
    [ ["\",\n",
       prefix(Step,Level+1),
       "jaderl_rt:forloop(Env, Opts, \"",
                          Key, "\", \"",
                          Value, "\", \"",
                          Varname, "\", fun(Env, Opts) ->\n",
                          gen(Content,Step)," end),\n\"" ]
      | Acc];

%---if / unless
gen({'if',_Lineno, Reverse, Expr, Tleg, Fleg}, Step, Level, Acc)    ->
    [ ["\",\n",
       prefix(Step,Level+1),
       "jaderl_rt:'if'(Env, Opts, ",
                          io_lib:format("~s",[Reverse]), ", ",
                          io_lib:format("~p",[Expr]), ", fun(Env, Opts) ->\n",
                          gen(Tleg,Step)," end,\n",
                           "fun(Env, Opts) ->\n",
                          gen(Fleg,Step)," end),\n\"" ]
      | Acc].


%--- returns the appropriate number of spaces for Level with a step of Step
prefix(Step, Level) ->  lists:duplicate(Step*Level, $ ).

%--- Generate the pieces for a tag
gen_open_tag(Tagname, Atts) ->  [<<"<">>, binify(Tagname), gen_tag_atts(Atts), <<">\n">>].

gen_close_tag(Tagname)      ->  [<<"</">>, binify(Tagname), <<">\n">>].

gen_tag_atts(Atts)  ->
    [ [$ , binify(Key), <<"=\\\"">>, binify(Val), <<"\\\"">>] || {Key,Val} <- Atts ].

%--- Generate the pieces for a case statement
gen_whens([HW|TW], Step, Level) ->
  gen_whens([HW|TW], Step, Level, []).

gen_whens([]     ,_Step,_Level, Acc)  -> lists:reverse(Acc);
gen_whens([HW|TW], Step, Level, Acc)  ->
  gen_whens(TW, Step, Level,
%              [ ["\"",
              [ [prefix(Step,Level),
                io_lib:format("{~p, ",[HW#'when'.value]),
                gen(HW#'when'.content,Step),
                "}"]
                | Acc]).


% ---
comp({string,S})                    ->  binify(S);

comp(Term)       when is_list(Term) ->  [comp(Item) || Item <- Term];

comp({var,V})                       ->  [<<"proplists:get_value(">>,binify(V),<<")">>];

comp({apply,M,F})                   ->  [binify(M),<<":">>,binify(F),<<"()">>].


% --- support functions ---
binify(X)   when is_binary(X)   ->  X;
binify(X)   when is_list(X)     ->  list_to_binary(X);
binify(X)   when is_atom(X)     ->  list_to_binary(atom_to_list(X)).

doctype_text(S) ->
    case doctype_text_set(S) of
        undefined   ->  S;
        Str         ->  Str
    end.

doctype_text_set(S) ->
   case proplists:get_value(string:strip(S), [
      {""            , "<!DOCTYPE html>"},
      {"5"           , "<!DOCTYPE html>"},
      {"default"     , "<!DOCTYPE html>"},
      {"xml"         , "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"},
      {"transitional", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"},
      {"strict"      , "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"},
      {"frameset"    , "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"},
      {"1.1"         , "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"},
      {"basic"       , "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\" \"http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd\">"},
      {"mobile"      , "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.2//EN\" \"http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd\">"}]) of
    undefined   ->  "<!DOCTYPE " ++ S ++ ">";
    Rslt        ->  Rslt
  end.



% --- unit tests ---

-define(GENTEST(X), ?_assertEqual(data(X, erl), pp(gen(data(X, ast), 2)))).

block_test_() ->
  [
    ?GENTEST(b1),
    ?_assertEqual(data(b1, erl), pp(gen(data(b1, ast), 2))),
    ?_assertEqual(data(b2, erl), pp(gen(data(b2, ast), 2))),
    ?_assertEqual(data(b3, erl), pp(gen(data(b3, ast), 2))),
    ?_assert(true)
  ].

tag_test_() ->
  [
    ?_assertEqual(data(t1, erl), pp(gen(data(t1, ast), 2))),
    ?_assertEqual(data(t2, erl), pp(gen(data(t2, ast), 2))),
    ?_assertEqual(data(t3, erl), pp(gen(data(t3, ast), 2))),
    ?_assertEqual(data(t4, erl), pp(gen(data(t4, ast), 2))),
    ?_assertEqual(data(t5, erl), pp(gen(data(t5, ast), 2))),
    ?_assertEqual(data(t6, erl), pp(gen(data(t6, ast), 2))),
    ?_assertEqual(data(t7, erl), pp(gen(data(t7, ast), 2))),
    ?_assertEqual(data(t8, erl), pp(gen(data(t8, ast), 2))),
    ?_assertEqual(data(t9, erl), pp(gen(data(t9, ast), 2))),
    ?_assert(true)
  ].

content_test_() ->
  [
    ?_assertEqual(data(c1, erl), pp(gen(data(c1, ast), 2))),
    ?_assertEqual(data(c2, erl), pp(gen(data(c2, ast), 2))),
    ?_assertEqual(data(c3, erl), pp(gen(data(c3, ast), 2))),
    ?_assertEqual(data(c4, erl), pp(gen(data(c4, ast), 2))),
    ?_assertEqual(data(c5, erl), pp(gen(data(c5, ast), 2))),
    ?_assertEqual(data(c6, erl), pp(gen(data(c6, ast), 2))),
    ?_assertEqual(data(c7, erl), pp(gen(data(c7, ast), 2))),
    ?_assertEqual(data(c8, erl), pp(gen(data(c8, ast), 2))),
    ?_assert(true)
  ].


interpolation_test_() ->
  [
    ?_assertEqual(data(i1, erl), pp(gen(data(i1, ast), 2))),
    ?_assertEqual(data(i2, erl), pp(gen(data(i2, ast), 2))),
    ?_assertEqual(data(i3, erl), pp(gen(data(i3, ast), 2))),
    ?_assertEqual(data(i4, erl), pp(gen(data(i4, ast), 2))),
    ?_assertEqual(data(i5, erl), pp(gen(data(i5, ast), 2))),
    ?_assert(true)
  ].

comment_test_() ->
  [
    ?_assertEqual(data(m1, erl), pp(gen(data(m1, ast), 2))),
    ?_assertEqual(data(m2, erl), pp(gen(data(m2, ast), 2))),
    ?_assertEqual(data(m3, erl), pp(gen(data(m3, ast), 2))),
    ?_assertEqual(data(m4, erl), pp(gen(data(m4, ast), 2))),
    ?_assert(true)
  ].

doctype_test_() ->
  [
    ?_assertEqual(data(d1, erl), pp(gen(data(d1, ast), 2))),
    ?_assertEqual(data(d2, erl), pp(gen(data(d2, ast), 2))),
    ?_assertEqual(data(d3, erl), pp(gen(data(d3, ast), 2))),
    ?_assert(true)
  ].

filter_test_() ->
  [
    ?_assertEqual(data(f1, erl), pp(gen(data(f1, ast), 2))),
%    ?_assertEqual(data(f2, erl), pp(gen(data(f2, ast), 2))),
    ?_assert(true)
  ].

case_test_() ->
  [
    ?_assertEqual(data(ca1, erl), pp(gen(data(ca1, ast), 2))),
    ?_assertEqual(data(ca2, erl), pp(gen(data(ca2, ast), 2))),
    ?_assert(true)
  ].

for_test_() ->
  [
    ?_assertEqual(data(e1, erl), pp(gen(data(e1, ast), 2))),
    ?_assertEqual(data(e2, erl), pp(gen(data(e2, ast), 2))),
    ?_assert(true)
  ].

if_test_() ->
  [
    ?GENTEST(u1),
    ?GENTEST(u2),
    ?GENTEST(u3),
    ?_assert(true)
  ].


% --- encoder for testing ---
%encode([])          ->  "[\"\n\"]";
%encode(S)           ->  encode_prefix(encode(S, [])).
%
%encode_prefix([H|T])->  [[$[,$"|H]|T].
%
%encode([],    Acc)  ->  lists:reverse([[$",$]]|Acc]);
%
%encode([H|T], Acc)  ->  encode(T, [H|Acc]).


% --- unit tests ---
%encode_test_() ->
%  [
%    ?_assertEqual(pp(gen(data(b1, ast), 2)), encode(data(b1, html))),
%    ?_assertEqual(pp(gen(data(t1, ast), 2)), encode(data(t1, html))),
%    ?_assert(true)
%  ].

comp_test_() ->
  [
    ?_assertEqual(<<"abc">>, comp({string,<<"abc">>})),
    ?_assertEqual(<<"abc">>, comp({string,"abc"})),
    ?_assertEqual([<<"abc">>,<<"def">>], comp([{string,"abc"},{string,"def"}])),
    ?_assertEqual([<<"proplists:get_value(">>,<<"varname">>,<<")">>], comp({var,"varname"})),
    ?_assertEqual([<<"module">>,<<":">>,<<"function">>,<<"()">>], comp({apply,"module","function"})),
    ?_assert(true)
  ].



% === TEST DATA ===
% Block Tests
data(b1, src) -> ["html","head","tail"];
data(b1, ast) -> [{tag,1,html,[],[]},{tag,2,head,[],[]},{tag,3,tail,[],[]}];
data(b1, erl) -> ["[\"<html>","</html>",
                  "<head>","</head>",
                  "<tail>","</tail>","\"]"];

data(b2, src) -> ["html","head"," tail"];
data(b2, ast) -> [{tag,1,html,[],[]},{tag,2,head,[],[{tag,3,tail,[],[]}]}];
data(b2, erl) -> ["[\"<html>","</html>",
                 "<head>",
                 "  <tail>","  </tail>",
                 "</head>","\"]"];

data(b3, src) -> ["li: p","  | text"];
data(b3, ast) -> [{tag,1,li,[],[{tag,1,p,[],[{string,2,"text"}]}]}];
data(b3, erl) -> ["[\"<li>","  <p>","    text","  </p>","</li>","\"]"];



% Tag Tests
data(t1, src) -> ["div#container"];
data(t1, ast) -> [{tag,1,'div',[{"id","container"}],[]}];
data(t1, erl) -> ["[\"<div id=\\\"container\\\">","</div>","\"]"];
data(t1, html)-> ["<div id=\\\"container\\\">","</div>"];

data(t2, src) -> ["div.user-details"];
data(t2, ast) -> [{tag,1,'div',[{"class","user-details"}],[]}];
data(t2, erl) -> ["[\"<div class=\\\"user-details\\\">","</div>","\"]"];

data(t3, src) -> ["div#foo.bar.baz"];
data(t3, ast) -> [{tag,1,'div',[{"id","foo"},{"class","bar baz"}],[]}];
data(t3, erl) -> ["[\"<div id=\\\"foo\\\" class=\\\"bar baz\\\">","</div>","\"]"];

data(t4, src) -> ["#foo",".bar"];
data(t4, ast) -> [{tag,1,'div',[{"id","foo"}],[]},{tag,2,'div',[{"class","bar"}],[]}];
data(t4, erl) -> ["[\"<div id=\\\"foo\\\">","</div>","<div class=\\\"bar\\\">","</div>","\"]"];

data(t5, src) -> ["#foo.bar.baz"];
data(t5, ast) -> [{tag,1,'div',[{"id","foo"},{"class","bar baz"}],[]}];
data(t5, erl) -> ["[\"<div id=\\\"foo\\\" class=\\\"bar baz\\\">","</div>","\"]"];

data(t6, src) -> ["p(attr=val)"];
data(t6, ast) -> [{tag,1,p,[{"attr","val"}],[]}];
data(t6, erl) -> ["[\"<p attr=\\\"val\\\">","</p>","\"]"];

data(t7, src) -> ["p(attr='val')"];
data(t7, ast) -> [{tag,1,p,[{"attr","val"}],[]}];
data(t7, erl) -> ["[\"<p attr=\\\"val\\\">","</p>","\"]"];

data(t8, src) -> ["p(attr='val')"];
data(t8, ast) -> [{tag,1,p,[{"attr","val"}],[]}];
data(t8, erl) -> ["[\"<p attr=\\\"val\\\">","</p>","\"]"];

data(t9, src) -> [".x(attr=val xmlns:attr2=\"val2\")"];
data(t9, ast) -> [{tag,1,'div',[{"class","x"},{"attr","val"},{"xmlns:attr2","val2"}],[]}];
data(t9, erl) -> ["[\"<div class=\\\"x\\\" attr=\\\"val\\\" xmlns:attr2=\\\"val2\\\">","</div>","\"]"];

% Content Tests
data(c1, src) -> ["p Wahoo!"];
data(c1, ast) -> [{tag,1,p,[],[{string,1,"Wahoo!"}]}];
data(c1, erl) -> ["[\"<p>","  Wahoo!","</p>","\"]"];

data(c2, src) -> ["p"," | Wahoo!"," |  to you!"," p"];
data(c2, ast) -> [{tag,1,p,[],[{string,2,"Wahoo!"},{string,3," to you!"},{tag,4,p,[],[]}]}];
data(c2, erl) -> ["[\"<p>","  Wahoo!","   to you!","  <p>","  </p>","</p>","\"]"];

data(c3, src) -> ["p ."," foo"];
data(c3, ast) -> [{tag,1,p,[],[{string,1,"."},{tag,2,foo,[],[]}]}];
data(c3, erl) -> ["[\"<p>","  .","  <foo>","  </foo>","</p>","\"]"];

data(c4, src) -> ["p."," foo"];
data(c4, ast) -> [{tag,1,p,[],[{string,2,"foo"}]}];
data(c4, erl) -> ["[\"<p>","  foo","</p>","\"]"];

data(c5, src) -> ["p."," foo", "  bar"];
data(c5, ast) -> [{tag,1,p,[],[{string,2,"foo"},{string,3," bar"}]}];
data(c5, erl) -> ["[\"<p>","  foo","   bar","</p>","\"]"];

data(c6, src) -> ["p."," foo\\\\bar"];
data(c6, ast) -> [{tag,1,p,[],[{string,2,"foo\\bar"}]}];
data(c6, erl) -> ["[\"<p>","  foo\\bar","</p>","\"]"];

data(c7, src) -> ["script","  bollocks"];
data(c7, ast) -> [{tag,1,script,[],[{string,2,"bollocks"}]}];
data(c7, erl) -> ["[\"<script>","  bollocks","</script>","\"]"];

data(c8, src) -> ["p.","  <h1>Title</h1>"];
data(c8, ast) -> [{tag,1,p,[],[{string,2,"<h1>Title</h1>"}]}];
data(c8, erl) -> ["[\"<p>","  <h1>Title</h1>","</p>","\"]"];

% Interpolation Tests
data(i1, src) -> ["p Hello #{name.first}!"];
data(i1, ast) -> [{tag,1,p,[],[[{string,1,"Hello "},{var,1,"name.first","true"},{string,1,"!"}]]}];
data(i1, erl) -> ["[\"<p>","  Hello ","\",  jaderl_rt:get_var(\"name.first\", Env, true),","\"  !","</p>","\"]"];

data(i2, src) -> ["p Hello \\#{name.first}!"];
data(i2, ast) -> [{tag,1,p,[],[{string,1,"Hello #{name.first}!"}]}];
data(i2, erl) -> ["[\"<p>","  Hello #{name.first}!","</p>","\"]"];

data(i3, src) -> ["p"," | g'day #{mate}!"];
data(i3, ast) -> [{tag,1,p,[],[[{string,2,"g'day "},{var,2,"mate","true"},{string,2,"!"}]]}];
data(i3, erl) -> ["[\"<p>","  g'day ","\",  jaderl_rt:get_var(\"mate\", Env, true),","\"  !","</p>","\"]"];

data(i4, src) -> ["p."," g'day #{cobber}!"];
data(i4, ast) -> [{tag,1,p,[],[[{string,2,"g'day "},{var,2,"cobber","true"},{string,2,"!"}]]}];
data(i4, erl) -> ["[\"<p>","  g'day ","\",  jaderl_rt:get_var(\"cobber\", Env, true),","\"  !","</p>","\"]"];

data(i5, src) -> ["p Hello !{name.first}!"];
data(i5, ast) -> [{tag,1,p,[],[[{string,1,"Hello "},{var,1,"name.first","false"},{string,1,"!"}]]}];
data(i5, erl) -> ["[\"<p>","  Hello ","\",  jaderl_rt:get_var(\"name.first\", Env, false),","\"  !","</p>","\"]"];

% Comment Tests
data(m1, src) -> ["  // A comment!"];
data(m1, ast) -> [{comment,1,"A comment!"}];
data(m1, erl) -> ["[\"<!-- A comment! -->","\"]"];

data(m2, src) -> ["p","  //- A comment!"];
data(m2, ast) -> [{tag,1,p,[],[]}];
data(m2, erl) -> ["[\"<p>","</p>","\"]"];

data(m3, src) -> ["body","  //","    p"];
data(m3, ast) -> [{tag,1,body,[],[{comblock,2,[],[{tag,3,p,[],[]}]}]}];
data(m3, erl) -> ["[\"<body>","  <!--","    <p>","    </p>","  -->","</body>","\"]"];

data(m4, src) -> ["body","  //-","    p"];
data(m4, ast) -> [{tag,1,body,[],[]}];
data(m4, erl) -> ["[\"<body>","</body>","\"]"];

data(m5, src) -> ["body","  //if IE lt 8","    p"];
data(m5, ast) -> [{tag,1,body,[],[{comblock,2,"IE lt 8",[{tag,3,p,[],[]}]}]}];
data(m5, erl) -> ["[\"<body>","  <!--[if IE lt 8]","    <p>","    </p>","  -->","</body>","\"]"];


% DOCTYPE Tests
data(d1, src) -> ["!!!"];
data(d1, ast) -> [{doctype,1,""}];
data(d1, erl) -> ["[\"<!DOCTYPE html>","\"]"];

data(d2, src) -> ["doctype"];
data(d2, ast) -> [{doctype,1,""}];
data(d2, erl) -> ["[\"<!DOCTYPE html>","\"]"];

data(d3, src) -> ["doctype html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\""];
data(d3, ast) -> [{doctype,1,"html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\""}];
data(d3, erl) -> ["[\"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Basic 1.1//EN\">","\"]"];

% Filter Tests
data(f1, src) -> ["p"," :markup","  sometext"];
data(f1, ast) -> [#tag{lineno=1, tagname=p, attlist=[],
                       content=[#filter{lineno=2, name="markup", opts=[],
                                        content=[#string{lineno=3, string="sometext"}]}]}];
data(f1, erl) -> ["[\"<p>", "\",",
                  "  jaderl_rt:filter(markup, [], [\"    sometext", "\"]),",
                  "\"</p>", "\"]"];

data(f2, src) -> ["p"," :markup","  text with a #{variable}"];
data(f2, ast) -> [#tag{lineno=1, tagname=p, attlist=[],
                        content=[#filter{lineno=2, name="markup", opts=[],
                                          content=[[#string{lineno=3,
                                                            string="text with a "},
                                                    #var{lineno=3,
                                                          name="variable",
                                                          escape="true"}]]}]}];
data(f2, erl) -> ["[\"<p>", "\",",
                  "  jaderl_rt:filter(markup, [], [\"  test with a ",
                  "jaderl_rt:get_var(\"variable\", Env, false)",
                   "\"]),",
                  "\"</p>", "\"]"];

% Case Tests
data(ca1, src) -> ["p start",
                   "-case varname",
                   " -when val1 it's val1",
                   " -when val2 it's val2",
                   " -default my default",
                   "p all done"];

data(ca1, ast) -> [#'tag'{lineno=1,tagname=p,attlist=[],
                          content=[#string{lineno=1,string="start"}]},

                   #'case'{lineno=2,
                           varname="varname",
                           default=[#string{lineno=5,
                                            string="my default"}],
                           whens=[#'when'{lineno=3,
                                          value="val1",
                                          content=[#string{lineno=3,
                                                           string="it's val1"}]},
                                  #'when'{lineno=4,
                                          value="val2",
                                          content=[#string{lineno=4,
                                                           string="it's val2"}]}]},

                   #'tag'{lineno=6,tagname=p,attlist=[],
                                   content=[#string{lineno=6,string="all done"}]} ];

data(ca1, erl) -> [ "[\"<p>", "  start", "</p>", 
                    "\",",
                    "  jaderl_rt:'case'(Env, Opts, \"varname\", \"    my default",
                    "\", [",
                    "    {\"val1\", [\"it's val1","\"]},",
                    "    {\"val2\", [\"it's val2","\"]}]),\"",
                    "<p>", "  all done","</p>","\"]"];


data(ca2, src) -> ["-case varname", " -when val1","  p val1",
                                    " -when val2","  p val2",
                    "p all done"];
data(ca2, ast) -> [#'case'{lineno=1,
                            varname="varname",
                            default=[],
                            whens=[#'when'{lineno=2,
                                           value="val1",
                                           content=[#tag{lineno=3,
                                                         tagname=p,
                                                         attlist=[],
                                                         content=[#string{lineno=3,
                                                                          string="val1"}]}]},
                                   #'when'{lineno=4,
                                           value="val2",
                                           content=[#tag{lineno=5,
                                                         tagname=p,
                                                         attlist=[],
                                                         content=[#string{lineno=5,
                                                                          string="val2"}]}]} ]},
                  {tag,6,p,[],[{string,6,"all done"}]}];

data(ca2, erl) -> ["[\"\",",
                    "  jaderl_rt:'case'(Env, Opts, \"varname\", \"\", [",
                    "    {\"val1\", [\"<p>","  val1","</p>","\"]},",
                    "    {\"val2\", [\"<p>","  val2","</p>","\"]}]),\"",
                    "<p>", "  all done","</p>","\"]"];


% For/Each Tests
data(e1, src) -> ["-each item in items","  li item"];
data(e1, ast) -> [#for{lineno=1, key="", value="item", varname="items",
                       content=[#tag{lineno=2,
                                     tagname=li,
                                     attlist=[],
                                     content=[#string{lineno=2,string="item"}]}]}];
data(e1, erl) -> ["[\"\",",
                  "  jaderl_rt:forloop(Env, Opts, \"\", \"item\", \"items\", fun(Env, Opts) ->",
                  "[\"<li>","  item","</li>","\"] end),",
                  "\"\"]"];

data(e2, src) -> ["-each key, val in items","  li item"];
data(e2, ast) -> [#for{lineno=1, key="key", value="val", varname="items",
                       content=[#tag{lineno=2,
                                     tagname=li,
                                     attlist=[],
                                     content=[#string{lineno=2,string="item"}]}]}];
data(e2, erl) -> ["[\"\",",
                  "  jaderl_rt:forloop(Env, Opts, \"key\", \"val\", \"items\", fun(Env, Opts) ->",
                  "[\"<li>",
                  "  item",
                  "</li>",
                  "\"] end),",
                  "\"\"]"];

% If/Unless Tests
data(u1, src) -> ["-if v1 == v2 xxx", "-else yyy"];
data(u1, ast) -> [#'if'{lineno=1,
                        reverse=false,
                        expr=#'binop'{lineno=1,
                                     op='==',
                                     lterm=#var{lineno=1,name="v1",escape=false},
                                     rterm=#var{lineno=1,name="v2",escape=false}},
                        tleg=[#string{lineno=1,string="xxx"}],
                        fleg=[#string{lineno=2,string="yyy"}]}];
data(u1, erl) -> ["[\"\",",
                  "  jaderl_rt:'if'(Env, Opts, false, {binop,1,'==',{var,1,\"v1\",false},{var,1,\"v2\",false}}, fun(Env, Opts) ->",
                  "[\"xxx","\"] end,",
                  "fun(Env, Opts) ->",
                  "[\"yyy","\"] end),",
                  "\"\"]"];

data(u2, src) -> ["-unless v1 == v2."," xxx", "-else."," yyy"];
data(u2, ast) -> [#'if'{lineno=1,
                        reverse=true,
                        expr=#'binop'{lineno=1,
                                     op='==',
                                     lterm=#var{lineno=1,name="v1",escape=false},
                                     rterm=#var{lineno=1,name="v2",escape=false}},
                        tleg=[#string{lineno=2,string="xxx"}],
                        fleg=[#string{lineno=4,string="yyy"}]}];
data(u2, erl) -> ["[\"\",",
                  "  jaderl_rt:'if'(Env, Opts, true, {binop,1,'==',{var,1,\"v1\",false},{var,1,\"v2\",false}}, fun(Env, Opts) ->",
                  "[\"xxx","\"] end,",
                  "fun(Env, Opts) ->",
                  "[\"yyy","\"] end),",
                  "\"\"]"];

data(u3, src) -> ["-if v1 > \"small\" && v2 < 99999 ."," xxx", "-else"," | yyy"];
data(u3, ast) -> [#'if'{lineno=1,
                        reverse=false,
                        expr=#'binop'{lineno=1,
                                     op='&&',
                                      lterm=#'binop'{lineno=1,
                                                   op='>',
                                                   lterm=#var{lineno=1,
                                                              name="v1",escape=false},
                                                   rterm=#string{lineno=1,
                                                                 string="small"}},
                                      rterm=#'binop'{lineno=1,
                                                   op='<',
                                                   lterm=#var{lineno=1,
                                                              name="v2",escape=false},
                                                   rterm=99999}},
                        tleg=[#string{lineno=2,string="xxx"}],
                        fleg=[#string{lineno=4,string="yyy"}]}];
data(u3, erl) -> ["[\"\",",
                  "  jaderl_rt:'if'(Env, Opts, false, {binop,1,'&&',",
                  "       {binop,1,'>',{var,1,\"v1\",false},{string,1,\"small\"}},",
                  "       {binop,1,'<',{var,1,\"v2\",false},99999}}, fun(Env, Opts) ->",
                  "[\"xxx","\"] end,",
                  "fun(Env, Opts) ->",
                  "[\"yyy","\"] end),",
                  "\"\"]"];

data(999, x) -> ok.    


%--- Test Support Functions ---
pp(X) -> string:tokens(binary_to_list(iolist_to_binary(X)),"\n").


show(Module) ->             show(Module, [], []).
show(Module, Env) ->        show(Module, Env, []).
show(Module, Env, Opts) ->
  {ok,Output} = Module:render(Env, Opts),
  [io:format("~s~n",[Line]) || Line <- pp(Output)],
  ok.
  



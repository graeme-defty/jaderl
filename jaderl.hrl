% HTML Syntax Tree node definitions
%
-record(comment,  {lineno=0, text=[]}).
-record(comblock, {lineno=0, iftext=true, content=[]}).
-record(doctype,  {lineno=0, text=[]}).
-record(tag,      {lineno=0, tagname, attlist=[], content=[]}).
-record(string,   {lineno=0, string=[]}).
-record(var,      {lineno=0, name, escape=true}).
-record(filter,   {lineno=0, name, opts, content=[]}).
-record('case',   {lineno=0, varname, default, whens=[]}).
-record('when',   {lineno=0, value, content=[]}).
-record(for,      {lineno=0, value, key, varname, content=[]}).
-record('if',     {lineno=0, reverse, expr, tleg, fleg}).
-record(expr,     {lineno=0, op, lterm, rterm}).
-record(binop,    {lineno=0, op, lterm, rterm}).



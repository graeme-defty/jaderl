jaderl
======

An implementation of the Jade templating language in Erlang 

Current Status
--------------

With reference to the docs at [https://github.com/visionmedia/jade#readme](https://github.com/visionmedia/jade#readme) ...

 Things that are implemented and working:
  -   tags
  -   nesting
  -   tag text on same line
  -   tag text using '|'
  -   tag text using '.'
  -   tags which accept *only* text don't need leading '|'
  -   attributes
  -   block expansion
  -   single line comments
  -   block comments
  -   conditional comments
  -   tag nesting
  -   inline html
  -   doctypes
  -   Case
  -   Filters
  -   Iteration
  -   Conditionals

Note:
Standard 'dot syntax' is supported, so sub-variables can be references as "var.sub".  This includes the ability to pass a tuple and lookup a module with name matching element(1... and call the appropriate function. (This is intended to be used to call functions in a model, but I have not tested it for that yet so it probably does not quite work, though it works as I have described it here) I have a flag for escaping values, but escaping is not implemented yet.



 Things that are not yet implemented:
  -   Code
  -   Template Inheritance
  -   Block Append / prepend
  -   Includes
  -   Mixins

Notes
-----
1. "- Code" means embedding JavaScript in the template, and I am not expecting to provide that at any point, unless there is a really good case for it

2. Where functions are required in the generated template I have created in-line anonymous functions rather than named functions - I may go back on this as i think named functions would make the generated template more readable (though I am not sure how much this matters)

3. For some strange reason I did not generate embedded functions to evaluate conditionals, but rather pass the expression tree into the generated template where it is "interpreted" by functions in the runtime library. Going the other way would probably produce slightly faster rendering, though I do not know whether the difference would be significant.

4. I have passed an options variable through the code, but have not had occasion to use it yet.

5. As I mentioned below, Jaderl is structured in two phases as discussed earlier in this thread, and I would like to propose HST (the Html Syntax Tree) as a possible candidate for a standard intermediate form. I have no doubt it needs work, but it is in the jaderl.hrl file, and I am happy to document it further as a start-point for discussion. (Note that at the moment the HST is an intermediate form passed between two functions in jaderl.erl.)


"Non-standard" items
--------------------
1. "Code" is not implemented, but I do intend to include the ability to execute an erlang function which would be expected to return HTML (this is somewhat a replacement for the custom tags that Django supports - likely syntax  "-call module function, params" )

2. Jaderl requires the use of "-" as the initial character for .  I like the way it makes these elemnts stand out from tags, and it ensures that no potential tags are "reserved words".  (Does this mean that Jaderl could be used to generate arbitrary



Most of the code is fairly well covered by unit tests, but where I have misunderstood, my tests will be testing in accordance with my misunderstanding.

Feedback on any of this very much appreciated!

Getting Started
===============

1. write a page in jade (see test.jade as an example
2. run jaderl:comp_file(test). This will create the generated template for your page. This is a plain erlang source file and will be named test.erl.
3. compile test.erl (creating test.beam ;-) )
4. run test:render(Env) - where env is a proplist of variables to be passed into the render operation. Env = [{"v1","val"}, {"v2","val"}] will give "xxx" in the generated html. Env = [{"v1","val"}, {"v2","vax"}] will give "yyy".

 



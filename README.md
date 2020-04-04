# Python-Error-Messages

This project provides enhanced error messages for Python.  It compiles to a JavaScript-library that can readily be
imported in a web page and used together with, e.g. [Skulpt](http://skulpt.org/).  The library only checks a Python
program for syntax errors and is primarily aimed at novice programmers and students.

It has grown out of a [PhD thesis by Tobias Kohn](https://tobiaskohn.ch/index.php/research/research-tigerjython/) and 
is part of the [TigerJython programming envrionment](http://jython.tobiaskohn.ch/).  There is also an 
[online version of TigerJython](https://webtigerjython.ethz.ch/), where this JavaScript-library is currently used.

Let us know if you find the library useful and include it in your own project (jython at tobiaskohn.ch).

In order to make the project a self-contained JavaScript-library, additional resources like translations of the error
messages are directly inlined into the code.


## Usage

### ES Module

To be used in larger projects, the parser is offered as a 
[JavaScript ES module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules).
The file can be found in [release/tigerpython-parser.mjs](release/tigerpython-parser.mjs), along a standalone
JavaScript file (see below).  The accompanying [index.html](release/index.html) demonstrates how the module could
be loaded and used in a simple HTML.  However, make sure that your server delivers the module with MIME type
`text/javascript`.


### Standalone JavaScript File

In order to use the parser for error checking in your project, load the respective 
[JavaScript-file](release/tigerpython-parser.js), set your preferences in the `TPyParser` object and then check your
code.  You can find a simple example in [doc/index.html](doc/index.html).
```JavaScript
TPyParser.rejectDeadCode = true;
TPyParser.setLanguage("en");
var err = TPyParser.checkSyntax(my_code);
if (err !== null) {
  var error_line = err.line;
  var error_msg = err.msg;
  // Display the error...
}
```


### The TPyParser Object

The `TPyParser` object provides two methods for checking syntax:
- **`TPyParser.checkSyntax(source: string): ErrorInfo`**  Takes the entire source code and returns either `null` or the
  first error found in the Python program.
- **`TPyParser.findAllErrors(source: string): Array[ErrorInfo]`**  Takes the entire source code and returns a list of 
  all errors found in the Python program.
  
A third method returns the AST:
- **`TPyParser.parse(source: string): object`**  Takes the entire source code and returns the AST as (generic) objects.
  Each object has a field `kind` that corresponds to the class in Python.  While the AST is as close to CPython as 
  possible, there are some minor differences, which stem mainly from the fact that the parser supports several different
  versions of Python, or provides slightly more information in some cases.
  Note that this is feature is currently under development and not thoroughly tested, yet.

Available options:
- **`evalMode: boolean`**  Set to `true` when the given code is from an interactive console / shell.  The parser would 
  normally reject simple expressions like `3 + 4` as not having side effects.  However, in the context of a shell, this
  is perfectly legal and normal.
- **`newDivision: boolean`**  (Python 2 only) Python 3 distinguishes between "true" and "integer" division 
  (`/` vs `//`) whereas Python 2 does not (by default).  Set this flag to `true` if you use this "new division" in
  Python 2.
- **`PythonVersion: int`**  An integer value that is either 2 or 3 and indicates the used Python version.
- **`rejectDeadCode: bool`**  If this flag is set to `true`, dead code will be rejected as erroneous (e.g. code after
  a `return` or `break` statement).
- **`repeatStatement: bool`**  _TigerJython_ allows the use of `repeat` as a keyword for simple loops.  Set this flag
  to `true` to mark `repeat` as a keyword.
- **`sagePower: bool`**  _Sage_ uses the `^` operator as a power operator rather than _xor_ (which becomes `^^`).  Set
  this flag to `true` to mark `^` as power operator and enable `^^` and `^^=` etc. as valid operators.
- **`strictCode: bool`**  If set to `true`, the parser will be very strict and report issues that are usually not
  considered errors in Python, but typical student mistakes.
- **`warningAsErrors: bool`**  If set to `true`, warnings are also reported as errors.
- **`setLanguage(s: string)`**  Change the language for the error message, where the language `s` is a two-letter
  identifier like `"en"`, `"de"`, or `"fr"`.
- **`setErrorMessage(code: string, msg: string)`**  Occasionally, you might want to change just a few of the error
  messages in a given language to adapt it to your system, but you probably do not want to recompile the entire parser
  for that.  In such a case, use `setErrorMessage()` to override the builtin error message and replace it by something
  new.  You can find all available error codes in 
  [errors.ErrorCode.scala](tpParser/shared/src/main/scala/tigerpython/parser/errors/ErrorCode.scala).
  
  Example usage:  
  `setErrorMessage("MISSING_SPACE", "Missing whitespace.")`


## Compilation

The entire project is written in [Scala 2.12](https://scala-lang.org/) / [Scala.js](http://www.scala-js.org/) and uses
[sbt](https://www.scala-sbt.org/).

When `sbt` is installed, go the the project's root directory and use `sbt fastOptJS` or `sbt fullOptJS` to compile the
project (`scala.js` supports two compilation modes: fast compilation during development and optimised compilation for
production code).  The output (JavaScript-files) can then be found in `./tpParser/js/target/scala-2.12/`.

The JS-linker is currently configured to output a
[JavaScript ES module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules).  The line responsible
for this is in [build.sbt](build.sbt): `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }`.  Remove
or comment this line in order to get a classic JavaScript file instead of an ES module.


## Contribution

The entire parser was written by [Tobias Kohn](https://tobiaskohn.ch/).  The authors who contributed translations for 
the error messages are noted in the [respective files](tpParser/shared/src/main/scala/tigerpython/parser/errormessages).

Please let us know if you would like to add another language for error messages - we are more than happy to include 
new languages.


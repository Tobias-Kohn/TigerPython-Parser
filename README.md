# Python-Error-Messages

This project provides enhanced error messages for Python.  It compiles to a JavaScript-library that can readily be
imported in a web page and used together with, e.g. [Skulpt](http://skulpt.org/), 
[Pyodide](https://pyodide.org/en/stable/) or any other Python-interpreter, really (however, note that its main
support is for Python until 3.6 and selected features from 3.7+ – see below).  The library only checks a Python program 
for syntax errors and is primarily aimed at novice programmers and students.

It has grown out of a [PhD thesis by Tobias Kohn](https://tobiaskohn.ch/index.php/research/research-tigerjython/) and 
is part of the [TigerJython programming envrionment](http://jython.tobiaskohn.ch/).  There is also an 
[online version of TigerJython](https://webtigerjython.ethz.ch/), where this JavaScript-library is currently used.

Let us know if you find the library useful and include it in your own project (jython at tobiaskohn.ch).

In order to make the project a self-contained JavaScript-library, additional resources like translations of the error
messages are directly inlined into the code.


## Usage

### NPM Package

You can point to this GitHub repository as an NPM dependency.  Add the following line to your `"dependencies"` in 
your `package.json`:

    "tigerpython-parser": "git+https://github.com/Tobias-Kohn/TigerPython-Parser.git",

### ES Module

To be used in larger projects, the parser can be compiled to a 
[JavaScript ES module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules).
The file can be found in [release/tigerpython-parser.mjs](release/tigerpython-parser.mjs), along with a standalone
JavaScript file (see below).  The accompanying [index.html](release/index.html) demonstrates how the module could
be loaded and used in a simple HTML.  However, make sure your server delivers the module with MIME type
`text/javascript`.

Note: when compiling to an ES module, the tests currently fail.  It seems node.js is complaining about using `export`
instead of `module.export` in the generated module.


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
- **`enableTigerPythonModifications()`**  This will enable TigerPython-specific modifications such as adding fields for
  `first` and `last` to the `list`-type.  This operation cannot be undone due to the kind of modifications, which is
  why this is not exposed as a boolean flag, but activated through this method.
- **`evalMode: boolean`**  Set to `true` when the given code is from an interactive console / shell.  The parser would 
  normally reject simple expressions like `3 + 4` as not having side effects.  However, in the context of a shell, this
  is perfectly legal and normal.
- **`newDivision: boolean`**  (Python 2 only) Python 3 distinguishes between "true" and "integer" division 
  (`/` vs `//`) whereas Python 2 does not (by default).  Set this flag to `true` if you use this "new division" in
  Python 2.
- **`pythonVersion: int`**  An integer value that is either 2 or 3 and indicates the used Python version (default 3).
- **`rejectDeadCode: bool`**  If this flag is set to `true`, dead code will be rejected as erroneous (e.g. code after
  a `return` or `break` statement).
- **`repeatStatement: bool`**  _TigerJython_ allows the use of `repeat` as a keyword for simple loops.  Set this flag
  to `true` to mark `repeat` as a keyword.
- **`sagePower: bool`**  _Sage_ uses the `^` operator as a power operator rather than _xor_ (which becomes `^^`).  Set
  this flag to `true` to mark `^` as power operator and enable `^^` and `^^=` etc. as valid operators.
- **`strictCode: bool`**  If set to `true`, the parser will be very strict and report issues that are usually not
  considered errors in Python, but typical student mistakes.
- **`warningAsErrors: bool`**  If set to `true`, warnings are also reported as errors.
- **`getLanguage(): string`**  Get the two-letter language identifier for the error messages currently in use.
- **`getLanguages(): string[]`**  Get the list two-letter language identifiers supported by TigerPython for the error 
  messages.
- **`setLanguage(s: string)`**  Change the language for the error messages, where the language `s` is a two-letter
  identifier like `"en"`, `"de"`, or `"fr"`.
- **`setErrorMessage(code: string, msg: string)`**  Occasionally, you might want to change just a few of the error
  messages in a given language to adapt it to your system, but you probably do not want to recompile the entire parser
  for that.  In such a case, use `setErrorMessage()` to override the builtin error message and replace it by something
  new.  You can find all available error codes in 
  [errors.ErrorCode.scala](tpParser/shared/src/main/scala/tigerpython/parser/errors/ErrorCode.scala).
  
  Example usage:  
  `setErrorMessage("MISSING_SPACE", "Missing whitespace.")`


### Auto-Completion

You can use the parser to get suggestions for auto-completion.  Use 
`TPyParser.autoComplete(source: string, pos: int, filter: bool)` for that purpose, which will return a 
(possibly empty) array of strings.  `source` is the program text and `pos` is the absolute offset of the current 
positions for which to retrieve the suggestions, counting from the beginning of the text.

The flag `filter` determines whether the resulting suggestions shall be filtered according to the position of the
caret.  Take, for instance, `math.a|(` (with `|` denoting the caret) and run the auto-completer.  If `filter` is set
to `false` (the default), it will return all possible suggestions for `math.`, ignoring the `a`.  If `filter` is set
to `true`, however, it will only return a list with `acos, asin, atan, ...`, that is those names starting with `a`.

Use `TPyParser.defineModule(moduleName: string, moduleBody: string, "pyi")` in order to add your own modules that can 
then be 'imported' when the auto-completer analyses your program code.  You need to give the module's name and the 
source code, which must be in the PYI-format (see, e.g., [typeshed](https://github.com/python/typeshed)).  For 
performance reasons, this uses a small and fast parser that does not deal gracefully with errors.  Hence, make sure 
the PYI-code that you pass in here is correct (this feature is not intended to be exposed to the end-user, but rather 
to allow you to support additional predefined modules).

There is also a method `getQualifiedName(source: string, pos: int): string` that returns the fully qualified name for an
identifier at the specified position in the source code.  That is, for instance `sin` is expanded to `math.sin`.  If
there is no name at the given position the returned value is `null`.


## Supported Python Versions

The parser was originally written so support Python 2.7 and Python 3.6.  With Python 3.9, the grammar has significantly
changed (including the AST nodes), as the old LL(1)-parser was replaced by a Pegen-parser.  The structure of this 
parser and the generated AST therefore deviates quite a bit from the ones used in Python 3.9+.  Nonetheless, we try to
support new syntactic elements or changes to the syntax, but cannot guarantee full compatibility.  Concerning newer
features, the current state is as follows:

- Assignment expressions (aka the 'Walrus' operator `:=`) are supported and will generate errors when the assignment
  expression is used as a statement, if the target is anything other than a name or if they are chained;
- f-Strings are partially supported in that the parser will accept f-strings (even nested ones according to PEP 701), 
  but it cannot yet handle errors that end in unterminated strings.  At the moment, it will just treat the entire
  string literal as a single token;
- Positional only-arguments are, likewise, accepted through the slash in the arguments list.  However, this is currently
  not reflected in the AST and no further checks are performed;
- Pattern matching is supported.  This one is a bit tricky because of the subtle mechanisms at play for discerning 
  whether `match` and `case` have to be considered keywords or just normal identifiers.


## Compilation

The entire project is written in [Scala 2.12](https://scala-lang.org/) / [Scala.js](http://www.scala-js.org/) and uses
[sbt](https://www.scala-sbt.org/).

When `sbt` is installed, go to the project's root directory and use `sbt makeRelease`.  This will create both the
JavaScript standalone-file and the module and place them inside the `/release` folder.


### Old instructions

When `sbt` is installed, go to the project's root directory and use `sbt fastOptJS` or `sbt fullOptJS` to compile the
project (`scala.js` supports two compilation modes: fast compilation during development and optimised compilation for
production code).  The output (JavaScript-files) can then be found in `./tpParser/js/target/scala-2.12/`.

The JS-linker was previsouly configured to output a
[JavaScript ES module](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules).  The line responsible
for this is in [build.sbt](build.sbt): `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }`.  Remove
the comments on this line in order to get an ES module instead of classic JavaScript file (however, `sbt` currently 
crashes because of an `export` vs. `module.export` error). 


### Tests

We include a number of test cases for programs with and without errors in them.  Programs in the folder
`/test/programs/correct` are correct Python programs that should run without any error.  Those in the folder
`/test/programs/erroneous` are Python programs that have an error.  These are stored as txt-files to avoid some
overzealous IDEs from reporting the (deliberate) errors in them.  The first two lines are comments indicating the
line (with the third actual line in the file starting as `1`) and the expected error code.

If a test with an incorrect program fails, you will find some additional information in the console, but usually
higher up than all the `[info]`s on successful or failed tests.  That additional information tells you what error
TigerPython think there actually is, where it seems the error, but also which location in the parser is responsible
for reporting that error.


## Contribution

The parser was initially written by [Tobias Kohn](https://tobiaskohn.ch/).  Further contributions by:
- [Neil Brown](https://www.twistedsquare.com/)

The authors who contributed translations for the error messages are noted in the [respective files](tpParser/shared/src/main/scala/tigerpython/parser/errormessages).

Please let us know if you would like to add another language for error messages - we are more than happy to include 
new languages.


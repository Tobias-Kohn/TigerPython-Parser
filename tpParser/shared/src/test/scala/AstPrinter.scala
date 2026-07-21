/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.printer

import tigerpython.parser.ast.AstNode._
import tigerpython.parser.ast.{AstNode, UnOp, ValueType}

/**
  * Converts an `AstNode` back into Python source text. This is not a formatter: output
  * is not meant to resemble the original source, only to re-parse to an equivalent tree
  * (used by the round-trip regression tests). Parentheses are inserted conservatively:
  * since the AST has no node representing "parenthesized expression", adding an extra,
  * unneeded pair of parens can never change the resulting tree, so ambiguous cases lean
  * towards over-parenthesizing rather than towards precise precedence-table minimalism.
  */
object AstPrinter {

  private final val INDENT = "    "

  def unparse(node: AstNode): String =
    node match {
      case Suite(_, stmts) => printStatements(stmts, 0).mkString("\n")
      case stmt: Statement => printStatement(stmt, 0).mkString("\n")
      case expr: Expression => printExpr(expr, 0)
      case other => throw new IllegalArgumentException("AstPrinter: cannot unparse node " + other)
    }

  private def ind(level: Int): String = INDENT * level

  private def wrap(s: String, ownPrec: Int, minPrec: Int): String =
    if (ownPrec < minPrec) "(" + s + ")" else s

  // Some "empty" syntactic forms (e.g. `case Point():`) leave an array field
  // `null` rather than an empty array.
  private def safeArr[T: scala.reflect.ClassTag](a: Array[T]): Array[T] =
    if (a != null) a else Array.empty[T]

  // ---------------------------------------------------------------- statements

  private def printStatements(stmts: Array[Statement], indent: Int): Vector[String] = {
    // A `null` entry in a statement list is a recovery artifact for a line so broken
    // that no placeholder statement (not even `pass`) could be built for it; there is
    // no Python spelling for "nothing, not even a no-op", so it is simply omitted.
    val nonNull = if (stmts == null) Array.empty[Statement] else stmts.filter(_ != null)
    if (nonNull.isEmpty)
      Vector(ind(indent) + "pass")
    else
      nonNull.iterator.flatMap(s => printStatement(s, indent)).toVector
  }

  private def printBody(body: Statement, indent: Int): Vector[String] =
    body match {
      // A `null` body is TigerPython's own `MISSING_BODY` recovery outcome for a
      // header line with no indented block following it - printing nothing (rather
      // than substituting `pass`) reproduces that exact, TigerPython-parseable
      // incomplete shape, and reparsing it deterministically yields another `null`
      // body again (verified directly: stable at top level, nested, and whether or
      // not another statement follows at the same indent).
      case null => Vector()
      case Suite(_, stmts) => printStatements(stmts, indent)
      case other => printStatement(other, indent)
    }

  // Decorators are attached in reverse source order, so print them in reverse too.
  private def decoratorLines(dec: Decoratable, indent: Int): Vector[String] =
    dec.decoratorList.reverseIterator.map(d => ind(indent) + "@" + printExpr(d, 0)).toVector

  private def printStatement(stmt: Statement, indent: Int): Vector[String] =
    stmt match {
      case Suite(_, stmts) =>
        printStatements(stmts, indent)
      case Nothing(_) =>
        Vector()
      case Pass(_) =>
        Vector(ind(indent) + "pass")
      case Break(_) =>
        Vector(ind(indent) + "break")
      case Continue(_) =>
        Vector(ind(indent) + "continue")
      case Assert(_, test, msg) =>
        val m = if (msg != null) ", " + printExpr(msg, 0) else ""
        Vector(ind(indent) + "assert " + printExpr(test, Precedence.OR) + m)
      case Assignment(_, targets, value) =>
        val lhs = targets.map(t => printExpr(t, 0) + " = ").mkString
        Vector(ind(indent) + lhs + printExpr(value, 0))
      case AugAssign(_, target, op, value) =>
        Vector(ind(indent) + printExpr(target, 0) + " " + op.toString + " " + printExpr(value, 0))
      case Delete(_, targets) =>
        Vector(ind(indent) + "del " + targets.map(t => printExpr(t, 0)).mkString(", "))
      case Return(_, value) =>
        Vector(ind(indent) + "return" + (if (value != null) " " + printExpr(value, 0) else ""))
      case ExprStatement(_, expr) =>
        Vector(ind(indent) + printExpr(expr, 0))
      case Global(_, names) =>
        Vector(ind(indent) + "global " + names.map(_.name).mkString(", "))
      case NonLocal(_, names) =>
        Vector(ind(indent) + "nonlocal " + names.map(_.name).mkString(", "))
      case Import(_, names) =>
        Vector(ind(indent) + "import " + names.map(printAlias).mkString(", "))
      case ImportFrom(_, module, names) =>
        val m = if (module != null) module.name else ""
        Vector(ind(indent) + "from " + m + " import " + names.map(printAlias).mkString(", "))
      case ImportStar(_, module) =>
        Vector(ind(indent) + "from " + module.name + " import *")
      case ImportFuture(_, names) =>
        Vector(ind(indent) + "from __future__ import " + names.mkString(", "))
      case Raise3(_, ex, cause) =>
        val e = if (ex != null) " " + printExpr(ex, 0) else ""
        val c = if (cause != null) " from " + printExpr(cause, 0) else ""
        Vector(ind(indent) + "raise" + e + c)
      case Raise2(_, exType, inst, tBack) =>
        val parts = Vector(exType, inst, tBack).takeWhile(_ != null).map(e => printExpr(e, 0))
        Vector(ind(indent) + "raise" + (if (parts.nonEmpty) " " + parts.mkString(", ") else ""))
      case Exec(_, expr, globals, locals) =>
        val g = if (globals != null) " in " + printExpr(globals, 0) + (if (locals != null) ", " + printExpr(locals, 0) else "") else ""
        Vector(ind(indent) + "exec " + printExpr(expr, 0) + g)
      case Print(_, dest, values, newline) =>
        val d = if (dest != null) ">>" + printExpr(dest, 0) + (if (values.nonEmpty) ", " else "") else ""
        val vs = values.map(v => printExpr(v, 0)).mkString(", ")
        val trailer = if (!newline) "," else ""
        Vector(ind(indent) + "print " + d + vs + trailer)
      case i: If =>
        val head = ind(indent) + "if " + printExpr(i.test, Precedence.OR) + ":"
        val bodyLines = printBody(i.body, indent + 1)
        val elseLines = if (i.elseBody != null) (ind(indent) + "else:") +: printBody(i.elseBody, indent + 1) else Vector()
        (head +: bodyLines) ++ elseLines
      case w: While =>
        val head = ind(indent) + "while " + printExpr(w.test, Precedence.OR) + ":"
        val bodyLines = printBody(w.body, indent + 1)
        val elseLines = if (w.elseBody != null) (ind(indent) + "else:") +: printBody(w.elseBody, indent + 1) else Vector()
        (head +: bodyLines) ++ elseLines
      case f: For =>
        val asyncPrefix = if (f.isAsync) "async " else ""
        val head = ind(indent) + asyncPrefix + "for " + printExpr(f.target, 0) + " in " + printExpr(f.iter, Precedence.OR) + ":"
        val bodyLines = printBody(f.body, indent + 1)
        val elseLines = if (f.elseBody != null) (ind(indent) + "else:") +: printBody(f.elseBody, indent + 1) else Vector()
        (head +: bodyLines) ++ elseLines
      case fd: FunctionDef =>
        val asyncPrefix = if (fd.isAsync) "async " else ""
        val ret = if (fd.returns != null) " -> " + printExpr(fd.returns, 0) else ""
        val head = ind(indent) + asyncPrefix + "def " + fd.getName + "(" + printParamList(fd.params) + ")" + ret + ":"
        decoratorLines(fd, indent) ++ (head +: printBody(fd.body, indent + 1))
      case cd: ClassDef =>
        val baseParts = safeArr(cd.bases).map(b => printExpr(b, 0)) ++ safeArr(cd.keywords).map(k => k.name + "=" + printExpr(k.value, 0))
        val basesStr = if (baseParts.nonEmpty) "(" + baseParts.mkString(", ") + ")" else ""
        val head = ind(indent) + "class " + cd.getName + basesStr + ":"
        decoratorLines(cd, indent) ++ (head +: printBody(cd.body, indent + 1))
      case w: With =>
        val asyncPrefix = if (w.isAsync) "async " else ""
        val ctx = printExpr(w.context, 0) + (if (w.opt_vars != null) " as " + printExpr(w.opt_vars, 0) else "")
        val head = ind(indent) + asyncPrefix + "with " + ctx + ":"
        head +: printBody(w.body, indent + 1)
      case t: Try =>
        val head = ind(indent) + "try:"
        val bodyLines = printBody(t.body, indent + 1)
        val handlerLines = t.handlers.toVector.flatMap(h => printExceptHandler(h, indent))
        val elseLines = if (t.elseBody != null) (ind(indent) + "else:") +: printBody(t.elseBody, indent + 1) else Vector()
        val finalLines = if (t.finalBody != null) (ind(indent) + "finally:") +: printBody(t.finalBody, indent + 1) else Vector()
        (head +: bodyLines) ++ handlerLines ++ elseLines ++ finalLines
      case m: Match =>
        val head = ind(indent) + "match " + printExpr(m.subject, 0) + ":"
        head +: m.cases.toVector.flatMap(c => printMatchCase(c, indent + 1))
      case _: MatchCase =>
        // `Match.cases` entries also show up as stray sibling statements in the
        // enclosing Suite (a parse-time artifact); printing them here as well
        // would duplicate the case clauses already printed by the `Match` case
        // above, so a standalone `MatchCase` is skipped.
        Vector()
      case eh: ExceptHandler =>
        printExceptHandler(eh, indent)
      case other =>
        throw new IllegalArgumentException("AstPrinter: cannot unparse statement " + other)
    }

  private def printExceptHandler(h: ExceptHandler, indent: Int): Vector[String] = {
    val ex =
      if (h.exType != null)
        " " + printExpr(h.exType, 0) + (if (h.name != null) " as " + printExpr(h.name, 0) else "")
      else
        ""
    (ind(indent) + "except" + ex + ":") +: printBody(h.body, indent + 1)
  }

  private def printMatchCase(c: MatchCase, indent: Int): Vector[String] = {
    val guard = if (c.guard != null) " if " + printExpr(c.guard, 0) else ""
    (ind(indent) + "case " + printPattern(c.pattern) + guard + ":") +: printBody(c.body, indent + 1)
  }

  private def printAlias(a: Alias): String =
    if (a.asName != null) a.name.name + " as " + a.asName.name else a.name.name

  // ---------------------------------------------------------------- parameters

  private def printParamList(p: Parameters): String = {
    val parts = collection.mutable.ArrayBuffer[String]()
    val offset = p.args.length - p.defaults.length

    def defaultFor(i: Int): Expression =
      if (i >= offset) {
        val d = p.defaults(i - offset)
        if (d != null) d._1 else null
      } else
        null

    def printParam(param: Parameter, default: Expression): String =
      param match {
        case np: NameParameter =>
          val ann = if (np.annotation != null) ": " + printExpr(np.annotation, 0) else ""
          val df = if (default != null) (if (np.annotation != null) " = " else "=") + printExpr(default, 0) else ""
          np.name + ann + df
        case tp: TupleParameter =>
          "(" + tp.tuple.names.map(_.name).mkString(", ") + ")"
      }

    val posOnly = p.maxPositionalOnlyArgCount
    val posMax = p.maxPositionalArgCount

    for (i <- 0 until posOnly)
      parts += printParam(p.args(i), defaultFor(i))
    if (posOnly > 0)
      parts += "/"
    for (i <- posOnly until posMax)
      parts += printParam(p.args(i), defaultFor(i))
    if (p.varArgs != null)
      parts += "*" + p.varArgs.name + (if (p.varArgs.annotation != null) ": " + printExpr(p.varArgs.annotation, 0) else "")
    else if (posMax < p.args.length)
      parts += "*"
    for (i <- posMax until p.args.length)
      parts += printParam(p.args(i), defaultFor(i))
    if (p.kwArgs != null)
      parts += "**" + p.kwArgs.name + (if (p.kwArgs.annotation != null) ": " + printExpr(p.kwArgs.annotation, 0) else "")

    parts.mkString(", ")
  }

  // ---------------------------------------------------------------- expressions

  private def printExpr(expr: Expression, minPrec: Int): String =
    expr match {
      case null =>
        // A `null` element inside an otherwise-populated expression list (e.g. a
        // `Call`'s args) is a recovery artifact for a missing expression, much like
        // `EmptyExpression` - printed as `None` for the same reason (see there).
        "None"
      case Name(_, name) =>
        name
      case BooleanValue(_, v) =>
        if (v) "True" else "False"
      case Ellipsis(_) =>
        "..."
      case StringValue(_, _, value, isUnicode) =>
        (if (isUnicode) "u" else "") + quoteString(value)
      case v: Value =>
        v.valueType match {
          case ValueType.NONE => "None"
          case _ => if (v.value != null) v.value else "0"
        }
      case EmptyExpression(_) =>
        "None"
      case NameTuple(_, names) =>
        "(" + names.map(_.name).mkString(", ") + ")"
      case Starred(_, e) =>
        "*" + printExpr(e, Precedence.UNARY)
      case a: Attribute =>
        printExpr(a.base, Precedence.ATOM) + "." + a.attr.name
      case s: Subscript =>
        printExpr(s.base, Precedence.ATOM) + "[" + printSlice(s.slice) + "]"
      case c: Call =>
        printCall(c)
      case tup: Tuple =>
        if (tup.elements.isEmpty) "()"
        else if (tup.elements.length == 1) "(" + printExpr(tup.elements.head, 0) + ",)"
        else "(" + tup.elements.map(e => printExpr(e, 0)).mkString(", ") + ")"
      case l: List =>
        "[" + l.elements.map(e => printExpr(e, 0)).mkString(", ") + "]"
      case Set(_, elements) =>
        "{" + elements.map(e => printExpr(e, 0)).mkString(", ") + "}"
      case Dict(_, _, keys, values) =>
        printDict(keys, values)
      case ListComp(_, _, elements, gens) =>
        "[" + printExpr(elements, 0) + printGenerators(gens) + "]"
      case SetComp(_, elements, gens) =>
        "{" + printExpr(elements, 0) + printGenerators(gens) + "}"
      case DictComp(_, _, key, value, gens) =>
        "{" + printExpr(key, 0) + ": " + printExpr(value, 0) + printGenerators(gens) + "}"
      case Generator(_, element, gens) =>
        "(" + printExpr(element, 0) + printGenerators(gens) + ")"
      case IfExpr(_, test, body, elseBody) =>
        val s = printExpr(body, Precedence.IF_EXPR + 1) + " if " + printExpr(test, Precedence.IF_EXPR + 1) +
          " else " + printExpr(elseBody, Precedence.IF_EXPR)
        wrap(s, Precedence.IF_EXPR, minPrec)
      case Lambda(_, args, body) =>
        val paramStr = printParamList(args)
        val s = "lambda" + (if (paramStr.nonEmpty) " " + paramStr else "") + ": " + printExpr(body, Precedence.LAMBDA)
        wrap(s, Precedence.LAMBDA, minPrec)
      case NamedExpr(_, target, value) =>
        "(" + target.name + " := " + printExpr(value, 0) + ")"
      case Yield(_, e) =>
        "(yield" + (if (e != null) " " + printExpr(e, 0) else "") + ")"
      case YieldFrom(_, source) =>
        "(yield from " + printExpr(source, 0) + ")"
      case Await(_, e) =>
        wrap("await " + printExpr(e, Precedence.AWAIT), Precedence.AWAIT, minPrec)
      case UnaryOp(_, op, e) =>
        val prec = Precedence.ofUnOp(op)
        wrap(op.toString + " " + printExpr(e, prec), prec, minPrec)
      case BinaryOp(_, tigerpython.parser.ast.BinOp.POW, left, right) =>
        val s = printExpr(left, Precedence.POWER + 1) + " ** " + printExpr(right, Precedence.POWER)
        wrap(s, Precedence.POWER, minPrec)
      case BinaryOp(_, op, left, right) =>
        val prec = Precedence.ofBinOp(op)
        val s = printExpr(left, prec) + " " + op.toString + " " + printExpr(right, prec + 1)
        wrap(s, prec, minPrec)
      case cmp: Compare =>
        val prec = Precedence.COMPARISON
        val sb = new StringBuilder(printExpr(cmp.left, prec + 1))
        for ((op, rhs) <- cmp.comparators)
          sb.append(' ').append(op.toString).append(' ').append(printExpr(rhs, prec + 1))
        wrap(sb.toString(), prec, minPrec)
      case other =>
        throw new IllegalArgumentException("AstPrinter: cannot unparse expression " + other)
    }

  private def printCall(c: Call): String = {
    val parts = collection.mutable.ArrayBuffer[String]()
    for (a <- c.args) parts += printExpr(a, 0)
    if (c.starArg != null) parts += "*" + printExpr(c.starArg, 0)
    for (k <- c.keywords) parts += k.name + "=" + printExpr(k.value, 0)
    if (c.kwArg != null) parts += "**" + printExpr(c.kwArg, 0)
    printExpr(c.function, Precedence.ATOM) + "(" + parts.mkString(", ") + ")"
  }

  private def printDict(keys: Array[Expression], values: Array[Expression]): String = {
    val parts =
      for (i <- keys.indices) yield {
        val k = keys(i)
        val v = values(i)
        k match {
          case value: Value if value.valueType == ValueType.NONE && value.value == null =>
            "**" + printExpr(v, 0)
          case _ =>
            printExpr(k, 0) + ": " + printExpr(v, 0)
        }
      }
    "{" + parts.mkString(", ") + "}"
  }

  private def printGenerators(gens: Array[Comprehension]): String =
    gens.iterator.map(printComprehension).mkString

  private def printComprehension(c: Comprehension): String = {
    val ifs = c.ifs.iterator.map(cond => " if " + printExpr(cond, Precedence.OR)).mkString
    " for " + printExpr(c.target, 0) + " in " + printExpr(c.iter, Precedence.OR) + ifs
  }

  private def printSlice(slice: Slice): String =
    slice match {
      case Index(_, value) =>
        printExpr(value, 0)
      case SliceRange(_, lower, upper, step) =>
        val l = if (lower != null) printExpr(lower, 0) else ""
        val u = if (upper != null) printExpr(upper, 0) else ""
        val s = if (step != null) ":" + printExpr(step, 0) else ""
        l + ":" + u + s
      case MultiSlice(_, elements) =>
        elements.map(printSlice).mkString(", ")
      case other =>
        throw new IllegalArgumentException("AstPrinter: cannot unparse slice " + other)
    }

  private def printPattern(p: Pattern): String =
    p match {
      case MatchValue(value) =>
        printExpr(value, 0)
      case MatchSingleton(value) =>
        printExpr(value, 0)
      case MatchSequence(_, patterns) =>
        "[" + safeArr(patterns).map(printPattern).mkString(", ") + "]"
      case MatchMapping(_, keys, patterns, rest) =>
        val entries = safeArr(keys).zip(safeArr(patterns)).map { case (k, pat) => printExpr(k, 0) + ": " + printPattern(pat) }
        val all = if (rest != null) entries :+ ("**" + rest.name) else entries
        "{" + all.mkString(", ") + "}"
      case MatchClass(cls, patterns, keywords) =>
        val posParts = safeArr(patterns).map(printPattern)
        val kwParts = safeArr(keywords).map { case (name, pat) => name.name + "=" + printPattern(pat) }
        printExpr(cls, Precedence.ATOM) + "(" + (posParts ++ kwParts).mkString(", ") + ")"
      case MatchStar(_, name) =>
        "*" + (if (name != null) name.name else "_")
      case MatchAs(_, pattern, name) =>
        (pattern, name) match {
          case (null, null) => "_"
          case (null, n) => n.name
          case (pat, null) => printPattern(pat)
          case (pat, n) => printPattern(pat) + " as " + n.name
        }
      case MatchOr(_, patterns) =>
        safeArr(patterns).map(printPattern).mkString(" | ")
      case other =>
        throw new IllegalArgumentException("AstPrinter: cannot unparse pattern " + other)
    }

  private def quoteString(value: String): String = {
    val useDouble = value.contains("'") && !value.contains("\"")
    val quote = if (useDouble) '"' else '\''
    val sb = new StringBuilder
    sb.append(quote)
    for (c <- value)
      c match {
        case '\\' => sb.append("\\\\")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case ch if ch == quote => sb.append('\\').append(ch)
        case ch => sb.append(ch)
      }
    sb.append(quote)
    sb.toString()
  }
}

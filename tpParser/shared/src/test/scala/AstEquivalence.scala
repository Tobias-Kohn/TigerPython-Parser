/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.printer

import tigerpython.parser.ast.AstNode._
import tigerpython.parser.ast.{AstNode, ValueType}

/**
  * Structural equality between two AST trees that ignores source positions
  * (`pos`/`endPos`/etc.) but does check "extra" mutable state that lives outside
  * case-class constructor parameters (`decoratorList`, `expr_context`) and would
  * therefore be silently skipped by the default, compiler-generated `equals`.
  *
  * Deliberately not implemented via overriding `equals` on the AST classes: a
  * case class always gets a concrete, compiler-synthesized `equals` defined
  * directly on the class itself, which shadows anything a mixed-in trait tries to
  * contribute — so trait-level overrides silently do nothing — and hand-rolling
  * `equals` per node class would both lose the free structural comparison of
  * constructor fields and risk changing `==`/`hashCode` semantics for any other
  * code in the project that already relies on default case-class equality.
  *
  * On mismatch, returns a path describing where the trees first diverge (e.g.
  * `root.body[2].value: BinaryOp op mismatch: ADD vs SUB`) rather than a bare
  * `false`, since a silent boolean is close to undebuggable for trees this size.
  */
object AstEquivalence {

  def equivalent(a: AstNode, b: AstNode): Either[String, Unit] =
    check(a, b, "root")

  // ---------------------------------------------------------------- generic helpers

  private def eq[T](x: T, y: T, name: String, path: String): Either[String, Unit] =
    if (x == y) Right(()) else Left(s"$path.$name: $x != $y")

  private def node(x: AstNode, y: AstNode, name: String, path: String): Either[String, Unit] =
    check(x, y, s"$path.$name")

  // A single-statement `Suite` carries no meaning beyond that one statement, but the
  // *same* semantic content can come out of the parser wrapped in a `Suite` (e.g. an
  // explicit `else:\n    if ...:` block) or as a bare, unwrapped statement (e.g.
  // `elif ...:` sugar, represented as a bare nested `If` with no `Suite`) - both are
  // unwrapped to their single inner statement, recursively, before comparing, so this
  // wrapper-shape difference alone doesn't count as a mismatch. Used only for
  // statement "body" fields (function/class/if/while/for/try/with/except bodies),
  // not for expression bodies like `IfExpr`/`Lambda`, where a `Suite` can't appear.
  private def unwrapSingleton(s: Statement): Statement =
    s match {
      case Suite(_, stmts) if stmts != null && stmts.length == 1 => unwrapSingleton(stmts(0))
      case other => other
    }

  private def bodyNode(x: Statement, y: Statement, name: String, path: String): Either[String, Unit] =
    node(unwrapSingleton(x), unwrapSingleton(y), name, path)

  // `EmptyExpression` is a recovery placeholder for a missing expression that has no
  // valid Python spelling of its own; a bare `null` expression (e.g. a missing element
  // in an otherwise-populated `Call` args list) is the same idea. The unparser renders
  // both as `None`, so all three are treated as equivalent here too.
  private def isNoneLike(e: Expression): Boolean =
    e == null || e.isInstanceOf[EmptyExpression] || (e match { case v: Value => v.valueType == ValueType.NONE; case _ => false })

  private def arr[T: scala.reflect.ClassTag](xs: Array[T], ys: Array[T], name: String, path: String)
                     (cmp: (T, T, String) => Either[String, Unit]): Either[String, Unit] = {
    // A `null` array and an empty array both mean "no elements" in this AST
    // (some "empty" syntactic forms, e.g. `case Point():`, leave the field
    // `null` rather than `Array()`), so they're treated as equivalent here.
    val xsSafe = if (xs != null) xs else Array.empty[T]
    val ysSafe = if (ys != null) ys else Array.empty[T]
    if (xsSafe.length != ysSafe.length)
      Left(s"$path.$name: length ${xsSafe.length} != ${ysSafe.length}")
    else
      xsSafe.indices.foldLeft[Either[String, Unit]](Right(())) { (acc, i) =>
        acc.flatMap(_ => cmp(xsSafe(i), ysSafe(i), s"$path.$name[$i]"))
      }
  }

  private def nodeArr[T <: AstNode: scala.reflect.ClassTag](xs: Array[T], ys: Array[T], name: String, path: String): Either[String, Unit] =
    arr(xs, ys, name, path)((x, y, p) => check(x, y, p))

  private def seq(steps: Either[String, Unit]*): Either[String, Unit] =
    steps.find(_.isLeft).getOrElse(Right(()))

  // ---------------------------------------------------------------- cross-cutting trait checks

  private def checkDecoratable(a: AstNode, b: AstNode, path: String): Either[String, Unit] =
    (a, b) match {
      case (da: Decoratable, db: Decoratable) =>
        nodeArr(da.decoratorList, db.decoratorList, "decoratorList", path)
      case _ =>
        Right(())
    }

  private def checkContextExpression(a: AstNode, b: AstNode, path: String): Either[String, Unit] =
    (a, b) match {
      case (ca: ContextExpression, cb: ContextExpression) =>
        eq(ca.expr_context, cb.expr_context, "expr_context", path)
      case _ =>
        Right(())
    }

  // ---------------------------------------------------------------- dispatcher

  // A bare `null` on one side is accepted here as a stand-in for a `None`-like
  // Expression on the other (the `isNoneLike` case below). This is needed because
  // `AstPrinter` can't reprint a missing expression as nothing: e.g. recovery for
  // `print("x" screws=+nails)` gives `Call(args: [StringValue("x"), null, ...])`,
  // where the missing second argument is a bare `null` with no position or partial
  // content to reconstruct from - so the printer renders it as the literal `None`,
  // which then reparses to a real `Value(pos, NONE)` node, not `null` again. Without
  // this check, that `null` (original) vs `Value(NONE)` (reprinted) pair would be
  // reported as a mismatch even though the printer did the only thing it could.
  private def noneLikeOrNull(n: AstNode): Boolean =
    n == null || (n.isInstanceOf[Expression] && isNoneLike(n.asInstanceOf[Expression]))

  def check(a: AstNode, b: AstNode, path: String): Either[String, Unit] =
    if (a == null && b == null)
      Right(())
    else if (noneLikeOrNull(a) && noneLikeOrNull(b))
      Right(())
    else if (a == null || b == null)
      Left(s"$path: null mismatch ($a vs $b)")
    else
      seq(
        checkDecoratable(a, b, path),
        checkContextExpression(a, b, path),
        checkCore(a, b, path)
      )

  private def typeMismatch(a: AstNode, b: AstNode, path: String): Either[String, Unit] =
    Left(s"$path: node type mismatch: ${a.getClass.getSimpleName} vs ${b.getClass.getSimpleName}")

  private def checkCore(a: AstNode, b: AstNode, path: String): Either[String, Unit] =
    a match {
      // ---------------- statements
      case Suite(_, stmtsA) =>
        b match { case Suite(_, stmtsB) => nodeArr(stmtsA, stmtsB, "statements", path); case _ => typeMismatch(a, b, path) }
      case Nothing(_) =>
        if (b.isInstanceOf[Nothing]) Right(()) else typeMismatch(a, b, path)
      case Pass(_) =>
        if (b.isInstanceOf[Pass]) Right(()) else typeMismatch(a, b, path)
      case Break(_) =>
        if (b.isInstanceOf[Break]) Right(()) else typeMismatch(a, b, path)
      case Continue(_) =>
        if (b.isInstanceOf[Continue]) Right(()) else typeMismatch(a, b, path)
      case Assert(_, testA, msgA) =>
        b match {
          case Assert(_, testB, msgB) => seq(node(testA, testB, "test", path), node(msgA, msgB, "msg", path))
          case _ => typeMismatch(a, b, path)
        }
      case Assignment(_, targetsA, valueA) =>
        b match {
          case Assignment(_, targetsB, valueB) => seq(nodeArr(targetsA, targetsB, "targets", path), node(valueA, valueB, "value", path))
          case _ => typeMismatch(a, b, path)
        }
      case AugAssign(_, targetA, opA, valueA) =>
        b match {
          case AugAssign(_, targetB, opB, valueB) =>
            seq(node(targetA, targetB, "target", path), eq(opA, opB, "op", path), node(valueA, valueB, "value", path))
          case _ => typeMismatch(a, b, path)
        }
      case Delete(_, targetsA) =>
        b match { case Delete(_, targetsB) => nodeArr(targetsA, targetsB, "targets", path); case _ => typeMismatch(a, b, path) }
      case Exec(_, exprA, globalsA, localsA) =>
        b match {
          case Exec(_, exprB, globalsB, localsB) =>
            seq(node(exprA, exprB, "expr", path), node(globalsA, globalsB, "globals", path), node(localsA, localsB, "locals", path))
          case _ => typeMismatch(a, b, path)
        }
      case ExprStatement(_, exprA) =>
        b match { case ExprStatement(_, exprB) => node(exprA, exprB, "expression", path); case _ => typeMismatch(a, b, path) }
      case For(_, _, targetA, iterA, bodyA, elseBodyA, isAsyncA) =>
        b match {
          case For(_, _, targetB, iterB, bodyB, elseBodyB, isAsyncB) =>
            seq(node(targetA, targetB, "target", path), node(iterA, iterB, "iter", path),
              bodyNode(bodyA, bodyB, "body", path), bodyNode(elseBodyA, elseBodyB, "elseBody", path),
              eq(isAsyncA, isAsyncB, "isAsync", path))
          case _ => typeMismatch(a, b, path)
        }
      case FunctionDef(_, _, nameA, paramsA, bodyA, returnsA, isAsyncA) =>
        b match {
          case FunctionDef(_, _, nameB, paramsB, bodyB, returnsB, isAsyncB) =>
            seq(node(nameA, nameB, "name", path), node(paramsA, paramsB, "params", path),
              bodyNode(bodyA, bodyB, "body", path), node(returnsA, returnsB, "returns", path),
              eq(isAsyncA, isAsyncB, "isAsync", path))
          case _ => typeMismatch(a, b, path)
        }
      case cdA: ClassDef =>
        b match {
          case cdB: ClassDef =>
            seq(node(cdA.name, cdB.name, "name", path), nodeArr(cdA.bases, cdB.bases, "bases", path),
              arr(cdA.keywords, cdB.keywords, "keywords", path)(checkKeyword),
              bodyNode(cdA.body, cdB.body, "body", path))
          case _ => typeMismatch(a, b, path)
        }
      case Global(_, namesA) =>
        b match { case Global(_, namesB) => nodeArr(namesA, namesB, "names", path); case _ => typeMismatch(a, b, path) }
      case If(_, _, testA, bodyA, elseBodyA) =>
        b match {
          case If(_, _, testB, bodyB, elseBodyB) =>
            seq(node(testA, testB, "test", path), bodyNode(bodyA, bodyB, "body", path), bodyNode(elseBodyA, elseBodyB, "elseBody", path))
          case _ => typeMismatch(a, b, path)
        }
      case Import(_, namesA) =>
        b match { case Import(_, namesB) => nodeArr(namesA, namesB, "names", path); case _ => typeMismatch(a, b, path) }
      case ImportFrom(_, moduleA, namesA) =>
        b match {
          case ImportFrom(_, moduleB, namesB) => seq(node(moduleA, moduleB, "module", path), nodeArr(namesA, namesB, "names", path))
          case _ => typeMismatch(a, b, path)
        }
      case ImportFuture(_, namesA) =>
        b match { case ImportFuture(_, namesB) => eq(namesA.toSeq, namesB.toSeq, "names", path); case _ => typeMismatch(a, b, path) }
      case ImportStar(_, moduleA) =>
        b match { case ImportStar(_, moduleB) => node(moduleA, moduleB, "module", path); case _ => typeMismatch(a, b, path) }
      case NonLocal(_, namesA) =>
        b match { case NonLocal(_, namesB) => nodeArr(namesA, namesB, "names", path); case _ => typeMismatch(a, b, path) }
      case Print(_, destA, valuesA, newlineA) =>
        b match {
          case Print(_, destB, valuesB, newlineB) =>
            seq(node(destA, destB, "dest", path), nodeArr(valuesA, valuesB, "values", path), eq(newlineA, newlineB, "newline", path))
          case _ => typeMismatch(a, b, path)
        }
      case Raise2(_, exTypeA, instA, tBackA) =>
        b match {
          case Raise2(_, exTypeB, instB, tBackB) =>
            seq(node(exTypeA, exTypeB, "exType", path), node(instA, instB, "inst", path), node(tBackA, tBackB, "tBack", path))
          case _ => typeMismatch(a, b, path)
        }
      case Raise3(_, exA, causeA) =>
        b match { case Raise3(_, exB, causeB) => seq(node(exA, exB, "ex", path), node(causeA, causeB, "cause", path)); case _ => typeMismatch(a, b, path) }
      case Return(_, valueA) =>
        b match { case Return(_, valueB) => node(valueA, valueB, "value", path); case _ => typeMismatch(a, b, path) }
      case Try(_, bodyA, handlersA, elseBodyA, finalBodyA) =>
        b match {
          case Try(_, bodyB, handlersB, elseBodyB, finalBodyB) =>
            seq(bodyNode(bodyA, bodyB, "body", path), nodeArr(handlersA, handlersB, "handlers", path),
              bodyNode(elseBodyA, elseBodyB, "elseBody", path), bodyNode(finalBodyA, finalBodyB, "finalBody", path))
          case _ => typeMismatch(a, b, path)
        }
      case While(_, _, testA, bodyA, elseBodyA) =>
        b match {
          case While(_, _, testB, bodyB, elseBodyB) =>
            seq(node(testA, testB, "test", path), bodyNode(bodyA, bodyB, "body", path), bodyNode(elseBodyA, elseBodyB, "elseBody", path))
          case _ => typeMismatch(a, b, path)
        }
      case With(_, _, contextA, optVarsA, bodyA, isAsyncA) =>
        b match {
          case With(_, _, contextB, optVarsB, bodyB, isAsyncB) =>
            seq(node(contextA, contextB, "context", path), node(optVarsA, optVarsB, "opt_vars", path),
              bodyNode(bodyA, bodyB, "body", path), eq(isAsyncA, isAsyncB, "isAsync", path))
          case _ => typeMismatch(a, b, path)
        }
      case Match(_, _, subjectA, casesA) =>
        b match { case Match(_, _, subjectB, casesB) => seq(node(subjectA, subjectB, "subject", path), nodeArr(casesA, casesB, "cases", path)); case _ => typeMismatch(a, b, path) }
      case MatchCase(_, _, patternA, guardA, bodyA) =>
        b match {
          case MatchCase(_, _, patternB, guardB, bodyB) =>
            seq(node(patternA, patternB, "pattern", path), node(guardA, guardB, "guard", path), bodyNode(bodyA, bodyB, "body", path))
          case _ => typeMismatch(a, b, path)
        }
      case ExceptHandler(_, exTypeA, nameA, bodyA) =>
        b match {
          case ExceptHandler(_, exTypeB, nameB, bodyB) =>
            seq(node(exTypeA, exTypeB, "exType", path), node(nameA, nameB, "name", path), bodyNode(bodyA, bodyB, "body", path))
          case _ => typeMismatch(a, b, path)
        }

      // ---------------- simple expressions
      case EmptyExpression(_) =>
        if (b.isInstanceOf[EmptyExpression]) Right(()) else typeMismatch(a, b, path)
      case Alias(_, nameA, asNameA) =>
        b match { case Alias(_, nameB, asNameB) => seq(node(nameA, nameB, "name", path), node(asNameA, asNameB, "asName", path)); case _ => typeMismatch(a, b, path) }
      case Ellipsis(_) =>
        if (b.isInstanceOf[Ellipsis]) Right(()) else typeMismatch(a, b, path)
      case Name(_, nameA) =>
        b match { case Name(_, nameB) => eq(nameA, nameB, "name", path); case _ => typeMismatch(a, b, path) }
      case NameTuple(_, namesA) =>
        b match { case NameTuple(_, namesB) => nodeArr(namesA, namesB, "names", path); case _ => typeMismatch(a, b, path) }
      case BooleanValue(_, valueA) =>
        b match { case BooleanValue(_, valueB) => eq(valueA, valueB, "value", path); case _ => typeMismatch(a, b, path) }
      case StringValue(_, _, valueA, isUnicodeA) =>
        b match {
          case StringValue(_, _, valueB, isUnicodeB) => seq(eq(valueA, valueB, "value", path), eq(isUnicodeA, isUnicodeB, "isUnicode", path))
          case _ => typeMismatch(a, b, path)
        }
      case va: Value =>
        b match {
          case vb: Value => seq(eq(va.valueType, vb.valueType, "valueType", path), eq(va.value, vb.value, "value", path))
          case _ => typeMismatch(a, b, path)
        }

      // ---------------- compound expressions
      case Attribute(_, _, baseA, attrA) =>
        b match { case Attribute(_, _, baseB, attrB) => seq(node(baseA, baseB, "base", path), node(attrA, attrB, "attr", path)); case _ => typeMismatch(a, b, path) }
      case Await(_, exprA) =>
        b match { case Await(_, exprB) => node(exprA, exprB, "expr", path); case _ => typeMismatch(a, b, path) }
      case BinaryOp(_, opA, leftA, rightA) =>
        b match {
          case BinaryOp(_, opB, leftB, rightB) => seq(eq(opA, opB, "op", path), node(leftA, leftB, "left", path), node(rightA, rightB, "right", path))
          case _ => typeMismatch(a, b, path)
        }
      case Call(_, _, functionA, argsA, keywordsA, starArgA, kwArgA) =>
        b match {
          case Call(_, _, functionB, argsB, keywordsB, starArgB, kwArgB) =>
            seq(node(functionA, functionB, "function", path), nodeArr(argsA, argsB, "args", path),
              arr(keywordsA, keywordsB, "keywords", path)(checkKeyword),
              node(starArgA, starArgB, "starArg", path), node(kwArgA, kwArgB, "kwArg", path))
          case _ => typeMismatch(a, b, path)
        }
      case cmpA: Compare =>
        b match {
          case cmpB: Compare =>
            seq(node(cmpA.left, cmpB.left, "left", path),
              arr(cmpA.comparators, cmpB.comparators, "comparators", path) { (x, y, p) =>
                seq(eq(x._1, y._1, "op", p), check(x._2, y._2, p + ".expr"))
              })
          case _ => typeMismatch(a, b, path)
        }
      case Dict(_, _, keysA, valuesA) =>
        b match {
          case Dict(_, _, keysB, valuesB) => seq(nodeArr(keysA, keysB, "keys", path), nodeArr(valuesA, valuesB, "values", path))
          case _ => typeMismatch(a, b, path)
        }
      case DictComp(_, _, keyA, valueA, gensA) =>
        b match {
          case DictComp(_, _, keyB, valueB, gensB) =>
            seq(node(keyA, keyB, "key", path), node(valueA, valueB, "value", path), arr(gensA, gensB, "generators", path)(checkComprehension))
          case _ => typeMismatch(a, b, path)
        }
      case Generator(_, elementA, gensA) =>
        b match {
          case Generator(_, elementB, gensB) => seq(node(elementA, elementB, "element", path), arr(gensA, gensB, "generators", path)(checkComprehension))
          case _ => typeMismatch(a, b, path)
        }
      case IfExpr(_, testA, bodyA, elseBodyA) =>
        b match {
          case IfExpr(_, testB, bodyB, elseBodyB) =>
            seq(node(testA, testB, "test", path), node(bodyA, bodyB, "body", path), node(elseBodyA, elseBodyB, "elseBody", path))
          case _ => typeMismatch(a, b, path)
        }
      case Lambda(_, argsA, bodyA) =>
        b match { case Lambda(_, argsB, bodyB) => seq(node(argsA, argsB, "args", path), node(bodyA, bodyB, "body", path)); case _ => typeMismatch(a, b, path) }
      case listA: List =>
        b match { case listB: List => nodeArr(listA.elements, listB.elements, "elements", path); case _ => typeMismatch(a, b, path) }
      case ListComp(_, _, elementsA, gensA) =>
        b match {
          case ListComp(_, _, elementsB, gensB) => seq(node(elementsA, elementsB, "elements", path), arr(gensA, gensB, "generators", path)(checkComprehension))
          case _ => typeMismatch(a, b, path)
        }
      case NamedExpr(_, targetA, valueA) =>
        b match { case NamedExpr(_, targetB, valueB) => seq(node(targetA, targetB, "target", path), node(valueA, valueB, "value", path)); case _ => typeMismatch(a, b, path) }
      case setA: Set =>
        b match { case setB: Set => nodeArr(setA.elements, setB.elements, "elements", path); case _ => typeMismatch(a, b, path) }
      case SetComp(_, elementsA, gensA) =>
        b match {
          case SetComp(_, elementsB, gensB) => seq(node(elementsA, elementsB, "elements", path), arr(gensA, gensB, "generators", path)(checkComprehension))
          case _ => typeMismatch(a, b, path)
        }
      case Starred(_, exprA) =>
        b match { case Starred(_, exprB) => node(exprA, exprB, "expr", path); case _ => typeMismatch(a, b, path) }
      case Subscript(_, _, baseA, sliceA) =>
        b match { case Subscript(_, _, baseB, sliceB) => seq(node(baseA, baseB, "base", path), node(sliceA, sliceB, "slice", path)); case _ => typeMismatch(a, b, path) }
      case tupA: Tuple =>
        b match { case tupB: Tuple => nodeArr(tupA.elements, tupB.elements, "elements", path); case _ => typeMismatch(a, b, path) }
      case UnaryOp(_, opA, exprA) =>
        b match { case UnaryOp(_, opB, exprB) => seq(eq(opA, opB, "op", path), node(exprA, exprB, "expr", path)); case _ => typeMismatch(a, b, path) }
      case Yield(_, exprA) =>
        b match { case Yield(_, exprB) => node(exprA, exprB, "expr", path); case _ => typeMismatch(a, b, path) }
      case YieldFrom(_, sourceA) =>
        b match { case YieldFrom(_, sourceB) => node(sourceA, sourceB, "source", path); case _ => typeMismatch(a, b, path) }

      // ---------------- slices
      case Index(_, valueA) =>
        b match { case Index(_, valueB) => node(valueA, valueB, "value", path); case _ => typeMismatch(a, b, path) }
      case MultiSlice(_, elementsA) =>
        b match { case MultiSlice(_, elementsB) => nodeArr(elementsA, elementsB, "elements", path); case _ => typeMismatch(a, b, path) }
      case SliceRange(_, lowerA, upperA, stepA) =>
        b match {
          case SliceRange(_, lowerB, upperB, stepB) => seq(node(lowerA, lowerB, "lower", path), node(upperA, upperB, "upper", path), node(stepA, stepB, "step", path))
          case _ => typeMismatch(a, b, path)
        }

      // ---------------- patterns
      case MatchValue(valueA) =>
        b match { case MatchValue(valueB) => node(valueA, valueB, "value", path); case _ => typeMismatch(a, b, path) }
      case MatchSingleton(valueA) =>
        b match { case MatchSingleton(valueB) => node(valueA, valueB, "value", path); case _ => typeMismatch(a, b, path) }
      case MatchSequence(_, patternsA) =>
        b match { case MatchSequence(_, patternsB) => nodeArr(patternsA, patternsB, "patterns", path); case _ => typeMismatch(a, b, path) }
      case MatchMapping(_, keysA, patternsA, restA) =>
        b match {
          case MatchMapping(_, keysB, patternsB, restB) =>
            seq(nodeArr(keysA, keysB, "keys", path), nodeArr(patternsA, patternsB, "patterns", path), node(restA, restB, "rest", path))
          case _ => typeMismatch(a, b, path)
        }
      case MatchClass(clsA, patternsA, keywordsA) =>
        b match {
          case MatchClass(clsB, patternsB, keywordsB) =>
            seq(node(clsA, clsB, "cls", path), nodeArr(patternsA, patternsB, "patterns", path),
              arr(keywordsA, keywordsB, "keywords", path) { (x, y, p) => seq(node(x._1, y._1, "name", p), node(x._2, y._2, "pattern", p)) })
          case _ => typeMismatch(a, b, path)
        }
      case MatchStar(_, nameA) =>
        b match { case MatchStar(_, nameB) => node(nameA, nameB, "name", path); case _ => typeMismatch(a, b, path) }
      case MatchAs(_, patternA, nameA) =>
        b match { case MatchAs(_, patternB, nameB) => seq(node(patternA, patternB, "pattern", path), node(nameA, nameB, "name", path)); case _ => typeMismatch(a, b, path) }
      case MatchOr(_, patternsA) =>
        b match { case MatchOr(_, patternsB) => nodeArr(patternsA, patternsB, "patterns", path); case _ => typeMismatch(a, b, path) }

      // ---------------- parameters
      case NameParameter(_, nameA, annotationA) =>
        b match { case NameParameter(_, nameB, annotationB) => seq(eq(nameA, nameB, "name", path), node(annotationA, annotationB, "annotation", path)); case _ => typeMismatch(a, b, path) }
      case TupleParameter(_, tupleA) =>
        b match { case TupleParameter(_, tupleB) => node(tupleA, tupleB, "tuple", path); case _ => typeMismatch(a, b, path) }
      case pa: Parameters =>
        b match {
          case pb: Parameters =>
            seq(nodeArr(pa.args, pb.args, "args", path),
              arr(pa.defaults, pb.defaults, "defaults", path) { (x, y, p) =>
                if (x == null && y == null) Right(())
                else if (x == null || y == null) Left(s"$p: null mismatch")
                else check(x._1, y._1, p)
              },
              eq(pa.maxPositionalOnlyArgCount, pb.maxPositionalOnlyArgCount, "maxPositionalOnlyArgCount", path),
              eq(pa.maxPositionalArgCount, pb.maxPositionalArgCount, "maxPositionalArgCount", path),
              node(pa.varArgs, pb.varArgs, "varArgs", path),
              node(pa.kwArgs, pb.kwArgs, "kwArgs", path))
          case _ => typeMismatch(a, b, path)
        }
      case ca: Comprehension =>
        b match { case cb: Comprehension => checkComprehension(ca, cb, path); case _ => typeMismatch(a, b, path) }

      case other =>
        Left(s"$path: AstEquivalence does not yet handle node type ${other.getClass.getSimpleName}")
    }

  private def checkKeyword(x: Keyword, y: Keyword, path: String): Either[String, Unit] =
    seq(eq(x.name, y.name, "name", path), check(x.value, y.value, path + ".value"))

  private def checkComprehension(x: Comprehension, y: Comprehension, path: String): Either[String, Unit] =
    seq(node(x.target, y.target, "target", path), node(x.iter, y.iter, "iter", path), nodeArr(x.ifs, y.ifs, "ifs", path))
}

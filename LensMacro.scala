package org.encalmo.lens

import scala.quoted.{Quotes, *}

object LensMacro {

  inline def createLens[R, V, X, L <: Lens[R, X]](
      name: String,
      get: R => V,
      set: (R, V) => R
  ): L =
    ${ LensMacro.createLensImpl[R, V, X, L]('name, 'get, 'set) }

  def createLensImpl[R: Type, V: Type, X: Type, L <: Lens[R, X]: Type](
      name: Expr[String],
      get: Expr[R => V],
      set: Expr[(R, V) => R]
  )(using
      Quotes
  ): Expr[L] = {
    import quotes.reflect.*

    val copyMethod = TypeRepr.of[V].typeSymbol.methodMember("copy").head
    val fields = TypeRepr.of[V].typeSymbol.caseFields
    val fieldSymbol = fields
      .find(s => s.name == name.valueOrAbort)
      .getOrElse(
        report.errorAndAbort(
          s"Expected field '$name' to be present on ${TypeRepr.of[V].typeSymbol}, but got none"
        )
      )
    val className: String = Symbol.freshName(TypeRepr.of[L].typeSymbol.name)
    val parents = List(TypeTree.of[L])

    def decls(cls: Symbol): List[Symbol] =
      List(
        Symbol.newMethod(
          cls,
          "get",
          MethodType(List("root"))(_ => List(TypeRepr.of[R]), _ => TypeRepr.of[X]),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        ),
        Symbol.newMethod(
          cls,
          "set",
          MethodType(List("root", "value"))(_ => List(TypeRepr.of[R], TypeRepr.of[X]), _ => TypeRepr.of[R]),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        ),
        Symbol.newMethod(
          cls,
          "update",
          MethodType(List("root", "update"))(
            _ => List(TypeRepr.of[R], TypeRepr.of[Function1[X, X]]),
            _ => TypeRepr.of[R]
          ),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        )
      )

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)

    val funcSetSym = cls.declaredMethod("set").head
    val funcSetDef = DefDef(
      funcSetSym,
      {
        case List(List(root @ Ident("root"), value @ Ident("value"))) =>
          val target = Expr.betaReduce('{ $get(${ root.asExprOf[R] }) }).asTerm
          val args = fields.map(s =>
            if s.name == name.valueOrAbort
            then NamedArg(s.name, value)
            else NamedArg(s.name, target.select(s))
          )
          val updated = target.select(copyMethod).appliedToArgs(args)
          Some(Expr.betaReduce('{ $set(${ root.asExprOf[R] }, ${ updated.asExprOf[V] }) }).asTerm)

        case other =>
          report.errorAndAbort(s"Expected method 'set' to have two parameters: 'root' and 'value', but got $other")
      }
    )

    val funcGetSym = cls.declaredMethod("get").head
    val funcGetDef = DefDef(
      funcGetSym,
      {
        case List(List(root @ Ident("root"))) =>
          val target = Expr.betaReduce('{ $get(${ root.asExprOf[R] }) }).asTerm
          Some(target.select(fieldSymbol))

        case other =>
          report.errorAndAbort(s"Expected method 'get' to have parameter: 'root', but got $other")
      }
    )

    val funcUpdateSym = cls.declaredMethod("update").head
    val funcUpdateDef = DefDef(
      funcUpdateSym,
      {
        case List(List(root @ Ident("root"), update @ Ident("update"))) =>
          val target = Expr.betaReduce('{ $get(${ root.asExprOf[R] }) }).asTerm
          val applyMethod = TypeRepr.of[Function1[X, X]].typeSymbol.methodMember("apply").head
          val value = update.select(applyMethod).appliedTo(target.select(fieldSymbol))
          val args = fields.map(s =>
            if s.name == name.valueOrAbort
            then NamedArg(s.name, value)
            else NamedArg(s.name, target.select(s))
          )
          val updated = target.select(copyMethod).appliedToArgs(args)
          Some(Expr.betaReduce('{ $set(${ root.asExprOf[R] }, ${ updated.asExprOf[V] }) }).asTerm)

        case other =>
          report.errorAndAbort(s"Expected method 'update' to have two parameters: 'root' and 'update', but got $other")
      }
    )

    val clsDef = ClassDef(cls, parents, body = List(funcGetDef, funcSetDef, funcUpdateDef))

    val newCls =
      Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[L])

    Block(List(clsDef), newCls).asExprOf[L]
  }

  inline def createOptionalLens[R, V, X, L <: Lens[R, Option[X]]](
      name: String,
      get: R => Option[V],
      set: (R, Option[V]) => R
  ): L =
    ${ LensMacro.createOptionalLensImpl[R, V, X, L]('name, 'get, 'set) }

  def createOptionalLensImpl[R: Type, V: Type, X: Type, L <: Lens[R, Option[X]]: Type](
      name: Expr[String],
      get: Expr[R => Option[V]],
      set: Expr[(R, Option[V]) => R]
  )(using
      Quotes
  ): Expr[L] = {
    import quotes.reflect.*

    val copyMethod = TypeRepr.of[V].typeSymbol.methodMember("copy").head
    val fields = TypeRepr.of[V].typeSymbol.caseFields
    val fieldSymbol = fields
      .find(s => s.name == name.valueOrAbort)
      .getOrElse(
        report.errorAndAbort(
          s"Expected field '$name' to be present on ${TypeRepr.of[V].typeSymbol}, but got none"
        )
      )
    val className: String = Symbol.freshName(TypeRepr.of[L].typeSymbol.name)
    val parents = List(TypeTree.of[L])

    def decls(cls: Symbol): List[Symbol] =
      List(
        Symbol.newMethod(
          cls,
          "get",
          MethodType(List("root"))(_ => List(TypeRepr.of[R]), _ => TypeRepr.of[Option[X]]),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        ),
        Symbol.newMethod(
          cls,
          "set",
          MethodType(List("root", "value"))(_ => List(TypeRepr.of[R], TypeRepr.of[Option[X]]), _ => TypeRepr.of[R]),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        ),
        Symbol.newMethod(
          cls,
          "update",
          MethodType(List("root", "update"))(
            _ => List(TypeRepr.of[R], TypeRepr.of[Function1[Option[X], Option[X]]]),
            _ => TypeRepr.of[R]
          ),
          Flags.Override & Flags.Inline,
          Symbol.noSymbol
        )
      )

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents = parents.map(_.tpe), decls, selfType = None)

    val funcSetSym = cls.declaredMethod("set").head
    val funcSetDef = DefDef(
      funcSetSym,
      {
        case List(List(root @ Ident("root"), value @ Ident("value"))) =>
          Some(
            Expr
              .betaReduce('{
                $get(${ root.asExprOf[R] }).match {
                  case None => ${ root.asExprOf[R] }
                  case Some(t) =>
                    ${
                      val target = '{ t }.asTerm
                      val args = fields.map(s =>
                        if s.name == name.valueOrAbort
                        then NamedArg(s.name, value)
                        else NamedArg(s.name, target.select(s))
                      )
                      val updated = target.select(copyMethod).appliedToArgs(args)
                      Expr.betaReduce('{ $set(${ root.asExprOf[R] }, Some(${ updated.asExprOf[V] })) })
                    }
                }
              })
              .asTerm
          )

        case other =>
          report.errorAndAbort(s"Expected method 'set' to have two parameters: 'root' and 'value', but got $other")
      }
    )

    val funcGetSym = cls.declaredMethod("get").head
    val funcGetDef = DefDef(
      funcGetSym,
      {
        case List(List(root @ Ident("root"))) =>
          Some(
            Expr
              .betaReduce('{
                $get(${ root.asExprOf[R] }).match {
                  case None => None
                  case Some(t) =>
                    ${
                      val target = '{ t }.asTerm
                      '{ Some(${ target.select(fieldSymbol).asExprOf[X] }) }
                    }
                }
              })
              .asTerm
          )

        case other =>
          report.errorAndAbort(s"Expected method 'get' to have parameter: 'root', but got $other")
      }
    )

    val funcUpdateSym = cls.declaredMethod("update").head
    val funcUpdateDef = DefDef(
      funcUpdateSym,
      {
        case List(List(root @ Ident("root"), update @ Ident("update"))) =>
          Some(
            Expr
              .betaReduce('{
                $get(${ root.asExprOf[R] }).match {
                  case None => ${ root.asExprOf[R] }
                  case Some(t) =>
                    ${
                      val target = '{ t }.asTerm
                      val applyMethod =
                        TypeRepr.of[Function1[Option[X], Option[X]]].typeSymbol.methodMember("apply").head
                      val value =
                        update
                          .select(applyMethod)
                          .appliedTo('{ Some(${ target.select(fieldSymbol).asExprOf[X] }) }.asTerm)
                      val args = fields.map(s =>
                        if s.name == name.valueOrAbort
                        then NamedArg(s.name, value)
                        else NamedArg(s.name, target.select(s))
                      )
                      val updated = target.select(copyMethod).appliedToArgs(args)
                      Expr.betaReduce('{ $set(${ root.asExprOf[R] }, Some(${ updated.asExprOf[V] })) })
                    }
                }
              })
              .asTerm
          )

        case other =>
          report.errorAndAbort(
            s"Expected method 'update' to have two parameters: 'root' and 'update', but got $other"
          )
      }
    )

    val clsDef = ClassDef(cls, parents, body = List(funcGetDef, funcSetDef, funcUpdateDef))

    val newCls =
      Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[L])

    Block(List(clsDef), newCls).asExprOf[L]
  }
}

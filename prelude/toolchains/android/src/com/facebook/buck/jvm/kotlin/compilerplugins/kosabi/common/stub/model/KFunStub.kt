/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

package com.facebook.kotlin.compilerplugins.kosabi.common.stub.model

private typealias NamedArg = Pair<String, Type>

open class KFunStub(val name: String, val namedArgs: List<NamedArg>) {
  constructor(name: String, args: Int) : this(name, List(args) { "p$it" to KType.ANY_NULLABLE })

  var ret: Type = KType.UNIT
  var throws: List<KType> = emptyList()
  var typeVariableList: List<KTypeVariableName> = emptyList()
  // Only members in named objects and companion objects can be declared as static
  // Top level functions are always static
  var static: Boolean = false
  var private: Boolean = false
  var abstract: Boolean = false
  var isConstructor: Boolean = false
  var isOperator: Boolean = false
  var isFinal: Boolean = false
  var annotations: List<KAnnotation> = emptyList()
  // Boolean: isSuper, super else this
  // List<NamedArg>: constructor parameters name and value
  var constructorSuperDelegate: Pair<Boolean, List<NamedArg>> = Pair(false, emptyList())

  companion object {
    fun withTypedArgs(name: String, args: List<Type>) =
        KFunStub(name, args.mapIndexed { i, e -> "p$i" to e })
  }
}

class KCtorStub(namedArgs: List<NamedArg>) : KFunStub("<init>", namedArgs) {
  // Note
  // It's not correct to pass Any? as an annotation argument
  constructor(args: Int) : this(List(args) { "p$it" to KType.ANY_NULLABLE })
}

class KTypeVariableName(
    val name: String,
    val bounds: List<Type>,
    override val pkg: List<String> = emptyList(),
    override val generics: List<Type> = emptyList(),
    override val nullable: Boolean = false,
) : Type {
  override val names: List<String> = listOf(name)
}

class KAnnotation(val pkg: String, val name: String, val attributesText: String)

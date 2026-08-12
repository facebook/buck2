/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

@file:SuppressWarnings("PackageLocationMismatch")
@file:Suppress("OPT_IN_USAGE_ERROR")
@file:OptIn(
    com.facebook.DeprecatedForRemovalCompilerApiCompat::class,
    com.facebook.DirectDeclarationsAccessCompat::class,
)

package com.facebook

import com.facebook.buck.jvm.kotlin.compilerplugins.common.isStub
import org.jetbrains.kotlin.backend.common.extensions.IrGenerationExtension
import org.jetbrains.kotlin.backend.common.extensions.IrPluginContext
import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.ir.IrBuiltIns
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.IrStatement
import org.jetbrains.kotlin.ir.declarations.*
import org.jetbrains.kotlin.ir.expressions.*
import org.jetbrains.kotlin.ir.expressions.impl.IrConstImpl
import org.jetbrains.kotlin.ir.expressions.impl.IrReturnImpl
import org.jetbrains.kotlin.ir.symbols.IrReturnTargetSymbol
import org.jetbrains.kotlin.ir.symbols.UnsafeDuringIrConstructionAPI
import org.jetbrains.kotlin.ir.types.*
import org.jetbrains.kotlin.ir.util.kotlinFqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.psi.KtFile

internal class NonAbiDeclarationsStrippingIrExtension(private val sourceFiles: List<KtFile>) :
    IrGenerationExtension {

  private fun shouldStripAnnotation(annotation: IrConstructorCall): Boolean {
    val annotationClass = annotation.symbol.owner.parent as? IrClass ?: return false
    val annotationFqName = annotationClass.kotlinFqName.asString()

    // Keep @Throws annotation in IR to generate bytecode throws clause for Java interop.
    // The JVM backend (FunctionCodegen.getThrownExceptions) reads @Throws from IR annotations
    // to generate the method's throws clause. If we strip it here, Java code cannot catch
    // checked exceptions because the throws clause won't be in bytecode.
    // Note: K2 JVM backend writes @Throws to both throws clause AND RuntimeInvisibleAnnotations.
    // We strip it from RuntimeInvisibleAnnotations via bytecode post-processing (see
    // ThrowsAnnotationStripper) to match K1 behavior and prevent Safe Kotlin errors.
    if (annotationFqName == "kotlin.jvm.Throws" || annotationFqName == "kotlin.Throws") {
      return false
    }

    // Strip SOURCE retention annotations (not needed in ABI)
    for (retentionAnnotation in annotationClass.annotations) {
      val retentionClass = retentionAnnotation.symbol.owner.parent as? IrClass ?: continue
      if (retentionClass.kotlinFqName.asString() == "kotlin.annotation.Retention") {
        if (retentionAnnotation.valueArgumentsCount > 0) {
          val arg = retentionAnnotation.getValueArgument(0)
          if (arg is IrGetEnumValue && arg.symbol.owner.name.asString() == "SOURCE") {
            return true
          }
        }
      }
    }
    return false
  }

  // Check if an annotation contains error types in its arguments.
  // Error types occur when K2 cannot resolve constants from source-only ABI dependencies.
  // These annotations cause crashes during constant evaluation, so we strip them.
  private fun hasErrorType(annotation: IrConstructorCall): Boolean {
    for (i in 0 until annotation.valueArgumentsCount) {
      val arg = annotation.getValueArgument(i)
      if (arg != null && containsErrorType(arg)) {
        return true
      }
    }
    return false
  }

  // Recursively check if an IR expression contains error types
  private fun containsErrorType(expression: IrExpression): Boolean {
    // Check if the expression's type is an error type
    if (expression.type is IrErrorType) {
      return true
    }

    // For class references (like IOException::class in @Throws), check if the referenced type
    // is an error type. This handles cases where @Throws has unresolved exception classes.
    if (expression is IrClassReference && expression.classType is IrErrorType) {
      return true
    }

    // For varargs (like @Throws(E1::class, E2::class)), check all elements
    if (expression is IrVararg) {
      for (element in expression.elements) {
        if (element is IrExpression && containsErrorType(element)) {
          return true
        }
      }
    }

    // Recursively check children
    var hasError = false
    expression.acceptChildren(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            if (element is IrExpression && element.type is IrErrorType) {
              hasError = true
            }
            // Also check class references in children
            if (element is IrClassReference && element.classType is IrErrorType) {
              hasError = true
            }
            element.acceptChildren(this, null)
          }
        },
        null,
    )
    return hasError
  }

  private fun stripSourceRetentionAnnotations(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            declaration.annotations =
                declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            super.visitClass(declaration)
          }

          override fun visitSimpleFunction(declaration: IrSimpleFunction) {
            declaration.annotations =
                declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            declaration.valueParameters.forEach { param ->
              param.annotations =
                  param.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            }
            super.visitSimpleFunction(declaration)
          }

          override fun visitField(declaration: IrField) {
            declaration.annotations =
                declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            super.visitField(declaration)
          }

          override fun visitProperty(declaration: IrProperty) {
            declaration.annotations =
                declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            super.visitProperty(declaration)
          }

          override fun visitConstructor(declaration: IrConstructor) {
            declaration.annotations =
                declaration.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            declaration.valueParameters.forEach { param ->
              param.annotations =
                  param.annotations.filter { !shouldStripAnnotation(it) && !hasErrorType(it) }
            }
            super.visitConstructor(declaration)
          }
        },
        null,
    )
  }

  @OptIn(UnsafeDuringIrConstructionAPI::class)
  override fun generate(moduleFragment: IrModuleFragment, pluginContext: IrPluginContext) {
    // Filter out files generated from stubs, similar to K1 implementation
    val stubs = sourceFiles.filter { it.isStub() }
    val stubPaths: Set<String> = stubs.map { it.viewProvider.virtualFile.path }.toSet()

    // Remove IR files that were generated from stubs
    moduleFragment.files.removeAll { irFile -> stubPaths.contains(irFile.fileEntry.name) }

    // Remove IR files for plugin-generated declarations (e.g., "__GENERATED DECLARATIONS__")
    // These files contain stub declarations generated by our
    // MissingConstantDeclarationGenerationExtension
    // for FIR resolution of missing constants and transitive dependencies. They're temporary
    // artifacts
    // used during FIR analysis and should not be included in the final bytecode.
    // Note: This filtering preserves legitimate plugin-generated methods like Parcelize's
    // describeContents()/writeToParcel() which are generated directly in the class.
    moduleFragment.files.removeAll { irFile ->
      irFile.fileEntry.name.contains("__GENERATED DECLARATIONS__") ||
          irFile.fileEntry.name.contains("GENERATED_DECLARATIONS")
    }

    // Strip SOURCE retention annotations from all declarations.
    // SOURCE retention annotations (like @IntDef from androidx.annotation) should not appear
    // in bytecode, but K2 preserves them. K1 Kosabi naturally omits them because source stubs
    // don't carry meta-annotations. We strip them here at the IR level where we can inspect
    // the annotation class's @Retention policy.
    stripSourceRetentionAnnotations(moduleFragment)

    moduleFragment.transform(
        NonAbiDeclarationsStrippingIrVisitor(pluginContext.irFactory, pluginContext.irBuiltIns),
        null,
    )
  }
}

@OptIn(UnsafeDuringIrConstructionAPI::class)
internal class NonAbiDeclarationsStrippingIrVisitor(
    private val irFactory: IrFactory,
    private val irBuiltins: IrBuiltIns,
) : IrElementTransformerVoidCompat() {

  override fun visitFile(declaration: IrFile): IrFile {
    declaration.removeNonPublicApi()
    return super.visitFile(declaration)
  }

  override fun visitDeclaration(declaration: IrDeclarationBase): IrStatement {
    if (declaration is IrDeclarationContainer) {
      declaration.removeNonPublicApi()
    }
    return super.visitDeclaration(declaration)
  }

  override fun visitClass(declaration: IrClass): IrStatement {
    // Strip PRIVATE supertypes from the class's implemented interfaces.
    // Internal supertypes are kept because source-only ABI is consumed within the same
    // module, where internal types are accessible. Stripping them would cause Java consumers
    // to see "incompatible types" errors when a public class implements an internal interface.

    // First, collect the supertypes that will be stripped (private only)
    val strippedSupertypes =
        declaration.superTypes.filter { superType ->
          val superClass = superType.classOrNull?.owner ?: return@filter false
          isClassPrivate(superClass)
        }

    // Strip the private supertypes
    declaration.superTypes =
        declaration.superTypes.filter { superType ->
          val superClass = superType.classOrNull?.owner ?: return@filter true
          !isClassPrivate(superClass)
        }

    // For each stripped supertype that was an interface, convert fake override methods
    // that implement that interface to real methods with stub bodies.
    // This is needed because the JVM backend skips generating bytecode for fake overrides.
    if (strippedSupertypes.isNotEmpty()) {
      convertFakeOverridesFromStrippedSupertypes(declaration, strippedSupertypes)
    }

    // Also convert fake override methods that override non-public API methods.
    // This handles cases like enum entry classes where the fake override methods
    // override methods from the parent enum class that in turn implements a private interface.
    convertPublicFakeOverridesOfNonPublicMethods(declaration)

    return super.visitClass(declaration)
  }

  // Convert public fake override methods that ultimately override non-public methods.
  // This handles enum entry classes where getFlavor/getName are fake overrides that
  // override the parent enum class's abstract methods (which themselves implement
  // a stripped private interface).
  private fun convertPublicFakeOverridesOfNonPublicMethods(irClass: IrClass) {
    for (decl in irClass.declarations) {
      if (decl !is IrSimpleFunction) continue
      if (!decl.isFakeOverride) continue
      if (!decl.visibility.isPublicAPI) continue

      // Check if any of the overridden symbols is from a class that implements a non-public
      // interface
      val shouldMaterialize =
          decl.overriddenSymbols.any { overriddenSymbol ->
            val overridden = overriddenSymbol.owner
            val overriddenParent = overridden.parent as? IrClass
            overriddenParent != null && !isClassPubliclyAccessible(overriddenParent)
          }

      if (shouldMaterialize) {
        materializeFakeOverride(decl)
      }
    }
  }

  // Convert fake override methods that came from stripped supertypes to real methods.
  // The JVM backend skips generating bytecode for fake overrides (isFakeOverride = true),
  // so when we strip a private interface from a public class, we need to convert the
  // fake override methods that implemented that interface to real methods with stub bodies.
  private fun convertFakeOverridesFromStrippedSupertypes(
      irClass: IrClass,
      strippedSupertypes: List<IrType>,
  ) {
    // Collect method signatures from stripped interfaces
    val strippedInterfaceSignatures = mutableSetOf<MethodSignature>()
    for (superType in strippedSupertypes) {
      val superClass = superType.classOrNull?.owner ?: continue
      if (superClass.kind != ClassKind.INTERFACE) continue
      for (decl in superClass.declarations) {
        if (decl is IrSimpleFunction && !decl.isFakeOverride) {
          strippedInterfaceSignatures.add(decl.methodSignature())
        }
      }
    }

    if (strippedInterfaceSignatures.isEmpty()) return

    // Materialize fake override methods matching stripped interface signatures
    materializeFakeOverridesMatching(irClass, strippedInterfaceSignatures)

    // For enum classes, also process enum entry classes
    // Enum entry classes (correspondingClass) inherit methods from the enum class, and when
    // a private interface is stripped from the enum class, those methods become orphaned
    // fake overrides in the enum entry classes too.
    if (irClass.kind == ClassKind.ENUM_CLASS) {
      for (enumEntry in irClass.declarations.filterIsInstance<IrEnumEntry>()) {
        val entryClass = enumEntry.correspondingClass ?: continue
        materializeFakeOverridesMatching(entryClass, strippedInterfaceSignatures)
      }
    }
  }

  // Convert a fake override to a real method with a stub body.
  private fun materializeFakeOverride(decl: IrSimpleFunction) {
    decl.isFakeOverride = false
    decl.modality = Modality.FINAL
    decl.origin = IrDeclarationOrigin.DEFINED
    if (decl.body == null) {
      decl.body =
          generateDefaultBody(decl.returnType, decl.symbol) ?: irFactory.createBlockBody(-1, -1)
    }
  }

  // Find and materialize public fake override methods whose signature matches
  // a method from a stripped interface.
  private fun materializeFakeOverridesMatching(
      irClass: IrClass,
      signatures: Set<MethodSignature>,
  ) {
    for (decl in irClass.declarations) {
      if (decl !is IrSimpleFunction) continue
      if (!decl.isFakeOverride) continue
      if (!decl.visibility.isPublicAPI) continue
      if (decl.methodSignature() in signatures) {
        materializeFakeOverride(decl)
      }
    }
  }

  // Check if a class is publicly accessible (it and all its containing classes are public)
  private fun isClassPubliclyAccessible(irClass: IrClass): Boolean {
    var current: IrClass? = irClass
    while (current != null) {
      if (!current.visibility.isPublicAPI) {
        return false
      }
      // Get the containing class, if any
      current = current.parent as? IrClass
    }
    return true
  }

  // Check if a class or any of its containing classes is private/local (not internal or public).
  // Internal classes are accessible within the same module (source-only ABI scope).
  private fun isClassPrivate(irClass: IrClass): Boolean {
    var current: IrClass? = irClass
    while (current != null) {
      val visibility = current.visibility
      if (
          visibility == DescriptorVisibilities.PRIVATE || visibility == DescriptorVisibilities.LOCAL
      ) {
        return true
      }
      current = current.parent as? IrClass
    }
    return false
  }

  override fun visitField(declaration: IrField): IrStatement {
    // For fields with initializers containing function calls,
    // replace the initializer with a default constant value.
    // This handles cases like: const val X = (10 * TimeConstants.MS_PER_SECOND).toInt()
    // where the initializer contains a function call that can't be evaluated at compile time
    // in source-only ABI mode.
    // We specifically check for IrCall to avoid replacing valid expressions like unary minus
    // (-1).
    val initializer = declaration.initializer
    if (initializer != null) {
      val expression = initializer.expression
      if (containsFunctionCalls(expression)) {
        // Replace with a default constant value based on the field type
        val defaultExpressionBody = generateDefaultExpressionBody(declaration.type)
        if (defaultExpressionBody != null) {
          declaration.initializer = defaultExpressionBody
        }
      }
    }
    return super.visitField(declaration)
  }

  // Check if an expression tree contains any function calls
  private fun containsFunctionCalls(expression: IrExpression): Boolean {
    if (expression is IrCall) return true
    var hasCall = false
    expression.acceptChildren(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            if (element is IrCall) hasCall = true
            element.acceptChildren(this, null)
          }
        },
        null,
    )
    return hasCall
  }

  // we shouldn't generate default values for constants becuase the values are getting inlined
  // TODO: fix it: T235115614
  private fun generateDefaultValue(type: IrType): IrExpression? {
    val constructedType = type as? IrSimpleType ?: return null

    // For primitive types, create a default constant value
    // The code handles properties like val DEFAULT_SHADOW_COLOR: Int = Color.argb(128, 0, 0, 0)
    // If we don't set a default value, compiler crashes because it expects a ConstExpression
    var defaultValue: IrExpression? =
        when {
          constructedType == irBuiltins.intType -> IrConstImpl.int(-1, -1, irBuiltins.intType, 0)
          constructedType == irBuiltins.booleanType ->
              IrConstImpl.boolean(-1, -1, irBuiltins.booleanType, false)
          constructedType == irBuiltins.stringType ->
              IrConstImpl.string(-1, -1, irBuiltins.stringType, "")
          constructedType == irBuiltins.doubleType ->
              IrConstImpl.double(-1, -1, irBuiltins.doubleType, 0.0)
          constructedType == irBuiltins.floatType ->
              IrConstImpl.float(-1, -1, irBuiltins.floatType, 0.0f)
          constructedType == irBuiltins.longType ->
              IrConstImpl.long(-1, -1, irBuiltins.longType, 0L)
          constructedType == irBuiltins.charType ->
              IrConstImpl.char(-1, -1, irBuiltins.charType, '\u0000')
          constructedType == irBuiltins.byteType -> IrConstImpl.byte(-1, -1, irBuiltins.byteType, 0)
          constructedType == irBuiltins.shortType ->
              IrConstImpl.short(-1, -1, irBuiltins.shortType, 0)
          else ->
              IrConstImpl.constNull(
                  -1,
                  -1,
                  constructedType.makeNullable(),
              ) // If we can't handle this type, return null
        }

    return defaultValue
  }

  private fun IrDeclarationContainer.removeNonPublicApi() {
    // For inline/value classes, we need to keep the backing field even if it's private
    val inlineClassBackingFieldName =
        (this as? IrClass)?.inlineClassRepresentation?.underlyingPropertyName

    this.declarations.removeAll { declaration ->
      // Keep synthetic declarations (generated by compiler)
      if (declaration.origin.isSynthetic) return@removeAll false
      // Keep constructors (needed for instantiation)
      if (declaration is IrConstructor) return@removeAll false
      // Keep companion objects (may contain public members)
      if ((declaration as? IrClass)?.isCompanion == true) return@removeAll false

      // Keep nested/inner classes even if private. They generate separate .class files
      // referenced via the InnerClasses attribute, constant pool entries, and Kotlin
      // @Metadata. Stripping the class but keeping these references causes javac to fail
      // with "class file not found" errors. K1 Kosabi keeps them as stubs — their private
      // members are stripped recursively and method bodies are stubbed by the visitor.
      // Note: `this is IrClass` ensures this only applies to nested classes (parent is a
      // class). Top-level private classes (parent is IrFile) are still stripped.
      if (declaration is IrClass && this is IrClass) return@removeAll false

      // Keep backing field/property of inline/value classes (required even if private)
      if (declaration.isInlineClassBackingMember(inlineClassBackingFieldName))
          return@removeAll false

      // Remove private/local members only (NOT internal - K1 kept internal in ABI)
      val visibility = (declaration as? IrDeclarationWithVisibility)?.visibility
      visibility == DescriptorVisibilities.PRIVATE ||
          visibility == DescriptorVisibilities.PRIVATE_TO_THIS ||
          visibility == DescriptorVisibilities.LOCAL
    }
  }

  private fun IrDeclaration.isInlineClassBackingMember(backingFieldName: Name?): Boolean {
    if (backingFieldName == null) return false
    return (this is IrField || this is IrProperty) &&
        (this as IrDeclarationWithName).name == backingFieldName
  }

  private fun generateDefaultReturnStatement(
      type: IrType,
      symbol: IrReturnTargetSymbol,
  ): IrReturn? {
    val defaultValue = generateDefaultValue(type) ?: return null
    return IrReturnImpl(-1, -1, irBuiltins.nothingType, symbol, defaultValue)
  }

  private fun generateDefaultBody(type: IrType, symbol: IrReturnTargetSymbol): IrBody? {
    val returnStatement = generateDefaultReturnStatement(type, symbol) ?: return null
    return irFactory.createBlockBody(-1, -1).apply { this.statements.add(returnStatement) }
  }

  private fun generateDefaultExpressionBody(type: IrType): IrExpressionBody? {
    val defaultValue = generateDefaultValue(type) ?: return null
    return irFactory.createExpressionBody(-1, -1, defaultValue)
  }

  override fun visitSimpleFunction(declaration: IrSimpleFunction): IrStatement {
    if (!declaration.origin.isSynthetic) {
      if (declaration.parent is IrProperty) {
        // for properties we need to generate a default body
        // otherwise we get a crash in the compiler
        // handles properties likeval DEFAULT_SHADOW_COLOR: Int = Color.argb(128, 0, 0, 0)
        // we shouldn't generate default values for constants becuase the values are getting
        // inlined
        // TODO: fix it: T235115614
        val body = generateDefaultBody(declaration.returnType, declaration.symbol)
        declaration.body = body ?: irFactory.createBlockBody(-1, -1)
      } else {
        declaration.body = irFactory.createBlockBody(-1, -1)
      }
      val parametersWithDefaultValues =
          declaration.valueParameters.filter { it.defaultValue != null }
      for (parameter in parametersWithDefaultValues) {
        // if we can - we resolve the default value to a constant
        // handles default values in functions like fun foo(x: Int = Something.SomeValue)
        generateDefaultExpressionBody(parameter.type)?.let { parameter.defaultValue = it }
      }
    }
    return super.visitSimpleFunction(declaration)
  }

  override fun visitAnonymousInitializer(
      declaration: IrAnonymousInitializer,
  ): IrStatement {
    // we also need to strip bodies from init {} blocks
    declaration.body = irFactory.createBlockBody(-1, -1)
    return super.visitAnonymousInitializer(declaration)
  }
}

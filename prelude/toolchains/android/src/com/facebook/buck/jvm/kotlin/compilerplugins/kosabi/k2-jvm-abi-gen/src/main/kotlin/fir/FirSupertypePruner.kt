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
    com.facebook.DirectDeclarationsAccessCompat::class,
)

package com.facebook

import org.jetbrains.kotlin.descriptors.ClassKind
import org.jetbrains.kotlin.descriptors.Visibilities
import org.jetbrains.kotlin.fir.FirSession
import org.jetbrains.kotlin.fir.backend.FirMetadataSource
import org.jetbrains.kotlin.fir.declarations.FirDeclarationOrigin
import org.jetbrains.kotlin.fir.declarations.FirRegularClass
import org.jetbrains.kotlin.fir.moduleData
import org.jetbrains.kotlin.fir.resolve.providers.symbolProvider
import org.jetbrains.kotlin.fir.serialization.providedDeclarationsForMetadataService
import org.jetbrains.kotlin.fir.symbols.SymbolInternals
import org.jetbrains.kotlin.fir.symbols.impl.FirClassSymbol
import org.jetbrains.kotlin.fir.symbols.impl.FirNamedFunctionSymbol
import org.jetbrains.kotlin.fir.types.ConeClassLikeType
import org.jetbrains.kotlin.fir.types.ConeTypeProjection
import org.jetbrains.kotlin.fir.types.constructType
import org.jetbrains.kotlin.ir.IrElement
import org.jetbrains.kotlin.ir.declarations.IrClass
import org.jetbrains.kotlin.ir.declarations.IrMetadataSourceOwner
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.name.CallableId
import org.jetbrains.kotlin.name.ClassId

/**
 * Strips private (and optionally internal) supertypes from FIR metadata sources attached to IR
 * class declarations, and converts fake override methods from stripped interfaces to real methods.
 */
internal class FirSupertypePruner {

  /**
   * Strip PRIVATE supertypes from FIR metadata sources. Private classes ARE removed from the ABI
   * JAR by removeNonPublicApi(), so we must also strip their references from metadata.
   */
  fun stripPrivateSupertypesFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            stripPrivateSupertypesFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          private fun stripPrivateSupertypesFromDeclaration(declaration: IrClass) {
            // Access metadata via IrMetadataSourceOwner interface
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return

            // Check if this is a FirMetadataSource
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            // Get the FIR class declaration
            val firClass = firMetadataSource.fir as? FirRegularClass ?: return

            // Collect class IDs of private supertypes that will be stripped
            val strippedSupertypeClassIds = mutableSetOf<ClassId>()

            // Filter out private supertypes from the FIR class's superTypeRefs
            try {
              val superTypeRefs = FirReflectionUtils.getMutableSuperTypeRefs(firClass) ?: return

              // Find supertypes that reference PRIVATE classes (not internal)
              val toRemove = superTypeRefs.filter { typeRef ->
                isPrivateSupertype(typeRef, firClass)
              }

              // Collect the ClassIds of stripped supertypes for fake override conversion
              for (typeRef in toRemove) {
                val classId = getPrivateClassIdFromTypeRef(typeRef)
                if (classId != null) {
                  strippedSupertypeClassIds.add(classId)
                }
              }

              if (toRemove.isNotEmpty()) {
                superTypeRefs.removeAll(toRemove)

                // Convert fake override methods from stripped interfaces to real methods in FIR
                convertFirFakeOverridesFromStrippedPrivateSupertypes(
                    firClass,
                    strippedSupertypeClassIds,
                )
              }
            } catch (e: Exception) {
              // Reflection failures are silently ignored
            }
          }

          // Extract ClassId from a type reference
          private fun getPrivateClassIdFromTypeRef(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef
          ): ClassId? {
            val coneType =
                (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                    ?: return null
            return (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                ?.lookupTag
                ?.classId
          }

          // Add method declarations from stripped private interfaces to the class.
          @OptIn(SymbolInternals::class)
          private fun convertFirFakeOverridesFromStrippedPrivateSupertypes(
              firClass: FirRegularClass,
              strippedSupertypeClassIds: Set<ClassId>,
          ) {
            if (strippedSupertypeClassIds.isEmpty()) return

            val interfaceMethods =
                collectMethodsFromPrivateInterfaces(
                    firClass.moduleData.session,
                    strippedSupertypeClassIds,
                )

            if (interfaceMethods.isEmpty()) return

            val existingMethodNames =
                firClass.declarations
                    .filterIsInstance<FirNamedFunctionCompat>()
                    .map { it.name.asString() }
                    .toSet()

            for (interfaceMethod in interfaceMethods) {
              val methodName = interfaceMethod.name.asString()
              if (methodName in existingMethodNames) continue

              val copiedMethod = copyPrivateInterfaceMethodToClass(interfaceMethod, firClass)
              if (copiedMethod != null) {
                FirReflectionUtils.getMutableDeclarations(firClass)?.add(copiedMethod)
                try {
                  firClass.moduleData.session.providedDeclarationsForMetadataService
                      .registerDeclaration(copiedMethod)
                } catch (e: Exception) {
                  // Registration failure is silently ignored
                }
              }
            }
          }

          @OptIn(SymbolInternals::class)
          private fun copyPrivateInterfaceMethodToClass(
              interfaceMethod: FirNamedFunctionCompat,
              targetClass: FirRegularClass,
          ): FirNamedFunctionCompat? {
            return try {
              val targetClassId = targetClass.symbol.classId
              val newCallableId =
                  CallableId(
                      targetClassId.packageFqName,
                      targetClassId.relativeClassName,
                      interfaceMethod.name,
                  )
              buildNamedFunctionCopyCompat(interfaceMethod) {
                origin = FirDeclarationOrigin.Source
                symbol = FirNamedFunctionSymbol(newCallableId)
                dispatchReceiverType =
                    targetClass.symbol.constructType(
                        ConeTypeProjection.EMPTY_ARRAY,
                        isMarkedNullable = false,
                    )
              }
            } catch (e: Exception) {
              null
            }
          }

          @OptIn(SymbolInternals::class)
          private fun collectMethodsFromPrivateInterfaces(
              session: FirSession,
              interfaceClassIds: Set<ClassId>,
          ): List<FirNamedFunctionCompat> {
            val methods = mutableListOf<FirNamedFunctionCompat>()
            for (classId in interfaceClassIds) {
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: continue
              val firClass = classSymbol.fir as? FirRegularClass ?: continue
              if (firClass.classKind != ClassKind.INTERFACE) continue

              for (decl in firClass.declarations) {
                if (decl is FirNamedFunctionCompat) {
                  val visibility = decl.status.visibility
                  if (visibility == Visibilities.Public || visibility == Visibilities.Protected) {
                    methods.add(decl)
                  }
                }
              }
            }
            return methods
          }

          // Check if a supertype is PRIVATE (not internal or public).
          // This only returns true for Visibilities.Private or Visibilities.Local.
          private fun isPrivateSupertype(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef,
              firClass: FirRegularClass,
          ): Boolean {
            try {
              val coneType =
                  (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                      ?: return false
              val classId =
                  (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                      ?.lookupTag
                      ?.classId ?: return false

              val session = firClass.moduleData.session
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: return false

              // Check if the class itself is private
              if (isClassPrivate(classSymbol)) {
                return true
              }

              // Also check if any containing class is private
              var outerClassId = classId.outerClassId
              while (outerClassId != null) {
                val outerSymbol =
                    session.symbolProvider.getClassLikeSymbolByClassId(outerClassId)
                        as? FirClassSymbol<*>
                if (outerSymbol != null && isClassPrivate(outerSymbol)) {
                  return true
                }
                outerClassId = outerClassId.outerClassId
              }

              return false
            } catch (e: Exception) {
              return false
            }
          }

          // Check if a class has PRIVATE visibility (not internal)
          private fun isClassPrivate(classSymbol: FirClassSymbol<*>): Boolean {
            val visibility = classSymbol.resolvedStatus.visibility
            return visibility == Visibilities.Private || visibility == Visibilities.Local
          }
        },
        null,
    )
  }

  /**
   * Strip internal supertypes from FIR metadata sources. NOTE: Currently disabled in the pipeline -
   * internal classes are kept in ABI.
   */
  fun stripInternalSupertypesFromFirMetadataSources(moduleFragment: IrModuleFragment) {
    moduleFragment.accept(
        object : IrElementVisitorVoidCompat() {
          override fun visitElement(element: IrElement) {
            element.acceptChildren(this, null)
          }

          override fun visitClass(declaration: IrClass) {
            stripInternalSupertypesFromDeclaration(declaration)
            super.visitClass(declaration)
          }

          private fun stripInternalSupertypesFromDeclaration(declaration: IrClass) {
            // Access metadata via IrMetadataSourceOwner interface
            val metadataSourceOwner = declaration as? IrMetadataSourceOwner ?: return
            val metadataSource = metadataSourceOwner.metadata ?: return

            // Check if this is a FirMetadataSource
            val firMetadataSource = metadataSource as? FirMetadataSource ?: return

            // Get the FIR class declaration
            val firClass = firMetadataSource.fir as? FirRegularClass ?: return

            // Collect class IDs of internal supertypes that will be stripped
            val strippedSupertypeClassIds = mutableSetOf<ClassId>()

            // Find supertypes that reference non-public classes
            val toRemove =
                firClass.superTypeRefs.filter { typeRef -> isInternalSupertype(typeRef, firClass) }

            // Collect the ClassIds of stripped supertypes for fake override conversion
            for (typeRef in toRemove) {
              val classId = getClassIdFromTypeRef(typeRef)
              if (classId != null) {
                strippedSupertypeClassIds.add(classId)
              }
            }

            if (toRemove.isNotEmpty()) {
              firClass.replaceSuperTypeRefs(firClass.superTypeRefs - toRemove.toSet())

              // Convert fake override methods from stripped interfaces to real methods in FIR
              // This is necessary because fake overrides are NOT serialized into Kotlin metadata,
              // so downstream Kotlin code would fail with "unresolved reference" errors.
              convertFirFakeOverridesFromStrippedSupertypes(firClass, strippedSupertypeClassIds)
            }
          }

          // Extract ClassId from a type reference
          private fun getClassIdFromTypeRef(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef
          ): ClassId? {
            val coneType =
                (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                    ?: return null
            return (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                ?.lookupTag
                ?.classId
          }

          // Add method declarations from stripped interfaces to the class.
          // When we strip private interfaces from the supertype list, the methods
          // from those interfaces need to be explicitly added to the class's
          // declarations. For enums, FIR does NOT create fake override declarations
          // for interface methods, so we must copy the methods from the interface.
          @OptIn(SymbolInternals::class)
          private fun convertFirFakeOverridesFromStrippedSupertypes(
              firClass: FirRegularClass,
              strippedSupertypeClassIds: Set<ClassId>,
          ) {
            if (strippedSupertypeClassIds.isEmpty()) {
              return
            }

            // Collect methods from stripped interfaces
            val interfaceMethods =
                collectMethodsFromInterfaces(
                    firClass.moduleData.session,
                    strippedSupertypeClassIds,
                )

            if (interfaceMethods.isEmpty()) {
              return
            }

            // Get existing method names in the class to avoid duplicates
            val existingMethodNames =
                firClass.declarations
                    .filterIsInstance<FirNamedFunctionCompat>()
                    .map { it.name.asString() }
                    .toSet()

            // Copy methods from interfaces to the class
            for (interfaceMethod in interfaceMethods) {
              val methodName = interfaceMethod.name.asString()

              // Skip if the class already has this method
              if (methodName in existingMethodNames) {
                continue
              }

              // Copy the method and add it to the class
              val copiedMethod = copyInterfaceMethodToClass(interfaceMethod, firClass)
              // Add to the mutable declarations list (needed for bytecode generation)
              FirReflectionUtils.getMutableDeclarations(firClass)?.add(copiedMethod)

              // Register with the metadata service for proper serialization into Kotlin metadata.
              // This is critical because FirElementSerializer.memberDeclarations() uses
              // scope-based iteration and getProvidedCallables() from this service, NOT the
              // declarations list.
              firClass.moduleData.session.providedDeclarationsForMetadataService
                  .registerDeclaration(copiedMethod)
            }
          }

          // Copy an interface method to a target class, creating a new FirNamedFunctionCompat
          // with the appropriate origin and symbol for the target class.
          @OptIn(SymbolInternals::class)
          private fun copyInterfaceMethodToClass(
              interfaceMethod: FirNamedFunctionCompat,
              targetClass: FirRegularClass,
          ): FirNamedFunctionCompat {
            val targetClassId = targetClass.symbol.classId
            val newCallableId =
                CallableId(
                    targetClassId.packageFqName,
                    targetClassId.relativeClassName,
                    interfaceMethod.name,
                )

            return buildNamedFunctionCopyCompat(interfaceMethod) {
              origin = FirDeclarationOrigin.Source
              symbol = FirNamedFunctionSymbol(newCallableId)
              // Update the dispatch receiver type to the target class
              dispatchReceiverType =
                  targetClass.symbol.constructType(
                      ConeTypeProjection.EMPTY_ARRAY,
                      isMarkedNullable = false,
                  )
            }
          }

          // Collect methods from the given interface ClassIds
          // Returns the actual FirNamedFunctionCompat objects so they can be copied into target
          // classes
          @OptIn(SymbolInternals::class)
          private fun collectMethodsFromInterfaces(
              session: FirSession,
              interfaceClassIds: Set<ClassId>,
          ): List<FirNamedFunctionCompat> {
            val methods = mutableListOf<FirNamedFunctionCompat>()
            for (classId in interfaceClassIds) {
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: continue
              val firClass = classSymbol.fir as? FirRegularClass ?: continue
              if (firClass.classKind != ClassKind.INTERFACE) continue

              for (decl in firClass.declarations) {
                if (decl is FirNamedFunctionCompat) {
                  // Only include public/protected methods
                  val visibility = decl.status.visibility
                  if (visibility == Visibilities.Public || visibility == Visibilities.Protected) {
                    methods.add(decl)
                  }
                }
              }
            }
            return methods
          }

          private fun isInternalSupertype(
              typeRef: org.jetbrains.kotlin.fir.types.FirTypeRef,
              firClass: FirRegularClass,
          ): Boolean {
            try {
              // Use coneTypeOrNull to avoid throwing on unresolved types
              val coneType =
                  (typeRef as? org.jetbrains.kotlin.fir.types.FirResolvedTypeRef)?.coneType
                      ?: return false
              val classId =
                  (coneType as? org.jetbrains.kotlin.fir.types.ConeClassLikeType)
                      ?.lookupTag
                      ?.classId ?: return false

              // Check if the class or any of its containing classes are internal
              // We need to resolve the class symbol to check its visibility
              val session = firClass.moduleData.session
              val classSymbol =
                  session.symbolProvider.getClassLikeSymbolByClassId(classId) as? FirClassSymbol<*>
                      ?: return false

              // Check if the class is internal/private
              if (!isClassPubliclyAccessible(classSymbol)) {
                return true
              }

              // Also check if any containing class is internal (recursively)
              var outerClassId = classId.outerClassId
              while (outerClassId != null) {
                val outerSymbol =
                    session.symbolProvider.getClassLikeSymbolByClassId(outerClassId)
                        as? FirClassSymbol<*>
                if (outerSymbol != null && !isClassPubliclyAccessible(outerSymbol)) {
                  return true
                }
                outerClassId = outerClassId.outerClassId
              }

              return false
            } catch (e: Exception) {
              return false
            }
          }

          private fun isClassPubliclyAccessible(classSymbol: FirClassSymbol<*>): Boolean {
            val visibility = classSymbol.resolvedStatus.visibility
            return visibility == Visibilities.Public || visibility == Visibilities.Protected
          }
        },
        null,
    )
  }
}

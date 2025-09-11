/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.facebook.kotlin.compilerplugins.kosabi.stubsgen.util

import com.facebook.kotlin.compilerplugins.kosabi.common.FullTypeQualifier

// created by parsing types contained in kotlin/libraries/stdlib,
// https://github.com/JetBrains/kotlin/tree/master/libraries/stdlib

object PlainKTStdlibTypes : FTQCollection {
  /**
   * Returns the simple names of all types in this collection.
   *
   * @return A collection of simple type names defined in this class
   */
  override fun all(): Collection<String> {
    return listOf(
        "EnumEntries",
        "EnumEntriesList",
        "Iterable",
        "MutableIterable",
        "Collection",
        "MutableCollection",
        "List",
        "MutableList",
        "Set",
        "MutableSet",
        "Map",
        "MutableMap",
        "PlatformThreadLocalRandom",
        "PathWalkOption",
        "ExperimentalPathApi",
        "OnErrorResult",
        "CopyActionResult",
        "FileVisitorBuilder",
        "FileVisitorBuilderImpl",
        "FileVisitorImpl",
        "CopyActionContext",
        "PathRelativizer",
        "DefaultCopyActionContext",
        "ExceptionsCollector",
        "IllegalFileNameException",
        "PathTreeWalk",
        "PathNode",
        "LinkFollowing",
        "DirectoryEntriesReader",
        "UByteArray",
        "ULongRange",
        "ULongProgression",
        "ULongProgressionIterator",
        "UInt",
        "UIntRange",
        "UIntProgression",
        "UIntProgressionIterator",
        "ExperimentalUnsignedTypes",
        "UByte",
        "UShort",
        "ULongArray",
        "ULong",
        "UIntArray",
        "UShortArray",
        "Ctor",
        "JsArrayView",
        "JsSetView",
        "JsMapView",
        "JsObject",
        "DefaultConstructorMarker",
        "JsIntrinsic",
        "Metadata",
        "FunctionAdapter",
        "IrLinkageError",
        "DoNotIntrinsify",
        "JsFun",
        "JsGenerator",
        "JsImplicitExport",
        "ArrayBuffer",
        "Float64Array",
        "Float32Array",
        "Int32Array",
        "String",
        "Enum",
        "Throwable",
        "ByteArray",
        "CharArray",
        "ShortArray",
        "IntArray",
        "LongArray",
        "FloatArray",
        "DoubleArray",
        "BooleanArray",
        "Boolean",
        "Byte",
        "Short",
        "Int",
        "Long",
        "Float",
        "Double",
        "Char",
        "Letter",
        "Digit",
        "OtherLowercase",
        "Category",
        "Console",
        "Volatile",
        "Synchronized",
        "Error",
        "Exception",
        "RuntimeException",
        "IllegalArgumentException",
        "IllegalStateException",
        "IndexOutOfBoundsException",
        "ConcurrentModificationException",
        "UnsupportedOperationException",
        "NumberFormatException",
        "NullPointerException",
        "ClassCastException",
        "AssertionError",
        "NoSuchElementException",
        "ArithmeticException",
        "NoWhenBranchMatchedException",
        "UninitializedPropertyAccessException",
        "JsPolyfill",
        "RegExp",
        "RegExpMatch",
        "JsMath",
        "ExceptionTraceBuilder",
        "Date",
        "Json",
        "KType",
        "KFunction",
        "ExperimentalAssociatedObjects",
        "AssociatedObjectKey",
        "KProperty",
        "KMutableProperty",
        "KProperty0",
        "KMutableProperty0",
        "KProperty1",
        "KMutableProperty1",
        "KProperty2",
        "KMutableProperty2",
        "KCallable",
        "KClass",
        "JsClass",
        "EnumEntriesSerializationProxy",
        "DefaultTimeSource",
        "MonotonicTimeSource",
        "Process",
        "HrTimeSource",
        "PerformanceTimeSource",
        "DateNowTimeSource",
        "GlobalPerformance",
        "Performance",
        "DurationUnit",
        "Comparator",
        "BaseOutput",
        "NodeJsOutput",
        "OutputToConsoleLog",
        "BufferedOutput",
        "BufferedOutputToConsoleLog",
        "Promise",
        "JsReadonlyArray",
        "JsArray",
        "JsReadonlySet",
        "JsSet",
        "JsReadonlyMap",
        "JsMap",
        "CancellationException",
        "JsIterationStep",
        "JsIterator",
        "GeneratorCoroutineImpl",
        "SafeContinuation",
        "CoroutineImpl",
        "CompletedContinuation",
        "InterceptedCoroutine",
        "RegexOption",
        "MatchGroup",
        "Regex",
        "Appendable",
        "CharacterCodingException",
        "StringBuilder",
        "CharCategory",
        "RandomAccess",
        "HashSet",
        "AbstractMutableMap",
        "HashMapKeysDefault",
        "HashMapValuesDefault",
        "LinkedHashMap",
        "AbstractMutableSet",
        "HashMap",
        "LinkedHashSet",
        "JsRawArray",
        "InternalStringMap",
        "InternalHashMap",
        "AbstractMutableList",
        "ArrayList",
        "AbstractMutableCollection",
        "InternalStringLinkedMap",
        "InternalMap",
        "HashMapKeys",
        "HashMapValues",
        "HashMapEntrySetBase",
        "HashMapEntrySet",
        "AutoCloseable",
        "ConstrainedOnceSequence",
        "Serializable",
        "JsName",
        "JsFileName",
        "JsModule",
        "JsNonModule",
        "JsQualifier",
        "JsExport",
        "EagerInitialization",
        "JsExternalInheritorsOnly",
        "JsExternalArgument",
        "JsStatic",
        "AtomicReference",
        "Dynamic",
        "JsAny",
        "JsString",
        "JsReference",
        "JsBigInt",
        "JsException",
        "JsBoolean",
        "JsNumber",
        "Nothing",
        "Array",
        "Any",
        "WasiErrorCode",
        "WasiError",
        "OutOfMemoryError",
        "WasmImport",
        "WasmExport",
        "MemoryAllocator",
        "ScopedMemoryAllocator",
        "UnsafeWasmMemoryApi",
        "Pointer",
        "BitSet",
        "ObsoleteNativeApi",
        "TypeCastException",
        "JvmOverloads",
        "JvmStatic",
        "JvmName",
        "JvmMultifileClass",
        "JvmPackageName",
        "JvmSynthetic",
        "Throws",
        "ImplicitlyActualizedByJvmDeclaration",
        "JvmField",
        "JvmSuppressWildcards",
        "JvmWildcard",
        "JvmInline",
        "JvmRecord",
        "Transient",
        "Strictfp",
        "JvmSerializableLambda",
        "KotlinReflectionNotSupportedError",
        "FunctionN",
        "Function0",
        "Function1",
        "Function2",
        "Function3",
        "Function4",
        "Function5",
        "Function6",
        "Function7",
        "Function8",
        "Function9",
        "Function10",
        "Function11",
        "Function12",
        "Function13",
        "Function14",
        "Function15",
        "Function16",
        "Function17",
        "Function18",
        "Function19",
        "Function20",
        "Function21",
        "Function22",
        "PurelyImplements",
        "KotlinNullPointerException",
        "Constants",
        "SynchronizedLazyImpl",
        "SafePublicationLazyImpl",
        "KParameter",
        "KAnnotatedElement",
        "KVisibility",
        "KDeclarationContainer",
        "TypeImpl",
        "TypeVariableImpl",
        "GenericArrayTypeImpl",
        "WildcardTypeImpl",
        "ParameterizedTypeImpl",
        "FileSystemException",
        "FileAlreadyExistsException",
        "AccessDeniedException",
        "NoSuchFileException",
        "LinesSequence",
        "DecodeInputStream",
        "EncodeOutputStream",
        "OnErrorAction",
        "TerminateException",
        "FileWalkDirection",
        "FileTreeWalk",
        "FilePathComponents",
        "LineReader",
        "ExposingBufferByteArrayOutputStream",
        "JvmDefault",
        "JvmDefaultWithoutCompatibility",
        "JvmDefaultWithCompatibility",
        "CharDirectionality",
        "FlagEnum",
        "MatcherMatchResult",
        "SystemProperties",
        "Charsets",
        "ScreenFloatValueRegEx",
        "AbstractPlatformRandom",
        "FallbackThreadLocalRandom",
        "PlatformRandom",
        "KotlinRandom",
        "ListBuilder",
        "SerializedCollection",
        "MapBuilder",
        "MapBuilderKeys",
        "MapBuilderValues",
        "AbstractMapBuilderEntrySet",
        "MapBuilderEntries",
        "SerializedMap",
        "SetBuilder",
        "ExperimentalJsFileName",
        "ExperimentalJsExport",
        "ExperimentalJsStatic",
        "ExperimentalJsReflectionCreateInstance",
        "ExperimentalJsCollectionsApi",
        "ReadAfterEOFException",
        "KotlinNothingValueException",
        "ObsoleteWorkersApi",
        "FreezingIsDeprecated",
        "SpecialToken",
        "Lexer",
        "Quantifier",
        "NamedGroup",
        "UnicodeCategory",
        "UnicodeCategoryScope",
        "AbstractCharClass",
        "JointSet",
        "CharSet",
        "SimpleSet",
        "AbstractSet",
        "FixedLengthQuantifierSet",
        "ReluctantFixedLengthQuantifierSet",
        "EmptySet",
        "LowSurrogateCharSet",
        "HighSurrogateCharSet",
        "ReluctantLeafQuantifierSet",
        "PossessiveFixedLengthQuantifierSet",
        "RangeSet",
        "PossessiveGroupQuantifierSet",
        "SequenceSet",
        "FSet",
        "FinalSet",
        "NonCapFSet",
        "AheadFSet",
        "BehindFSet",
        "AtomicFSet",
        "SingleSet",
        "WordBoundarySet",
        "DecomposedCharSet",
        "LookAroundSet",
        "HangulDecomposedCharSet",
        "QuantifierSet",
        "LeafSet",
        "DotQuantifierSet",
        "PositiveLookAheadSet",
        "NegativeLookAheadSet",
        "NonCapturingJointSet",
        "PositiveLookBehindSet",
        "NegativeLookBehindSet",
        "LeafQuantifierSet",
        "EOLSet",
        "UnifiedQuantifierSet",
        "PreviousMatchSet",
        "DotSet",
        "AtomicJointSet",
        "EOISet",
        "PossessiveLeafQuantifierSet",
        "SupplementaryRangeSet",
        "CompositeRangeSet",
        "SOLSet",
        "GroupQuantifierSet",
        "SurrogateRangeSet",
        "BackReferenceSet",
        "ReluctantGroupQuantifierSet",
        "MatchResultImpl",
        "CharClass",
        "Pattern",
        "AbstractLineTerminator",
        "PatternSyntaxException",
        "Iterator",
        "MutableIterator",
        "ListIterator",
        "MutableListIterator",
        "CharProgression",
        "IntProgression",
        "LongProgression",
        "CharRange",
        "IntRange",
        "LongRange",
        "ComparableRange",
        "ComparableOpenEndRange",
        "ClosedFloatingPointRange",
        "ClosedDoubleRange",
        "OpenEndDoubleRange",
        "ClosedFloatRange",
        "OpenEndFloatRange",
        "CharProgressionIterator",
        "IntProgressionIterator",
        "LongProgressionIterator",
        "ClosedRange",
        "OpenEndRange",
        "ExperimentalTypeInference",
        "ExperimentalObjCRefinement",
        "ExperimentalObjCName",
        "ExperimentalNativeApi",
        "ReversedComparator",
        "NaturalOrderComparator",
        "ReverseOrderComparator",
        "DeepRecursiveFunction",
        "DeepRecursiveScope",
        "DeepRecursiveScopeImpl",
        "Result",
        "Lazy",
        "LazyThreadSafetyMode",
        "UnsafeLazyImpl",
        "InitializedLazyImpl",
        "Pair",
        "Triple",
        "NotImplementedError",
        "KotlinVersion",
        "KotlinVersionCurrentValue",
        "Effect",
        "ConditionalEffect",
        "SimpleEffect",
        "Returns",
        "ReturnsNotNull",
        "CallsInPlace",
        "ExperimentalContracts",
        "ContractBuilder",
        "InvocationKind",
        "KTypeProjection",
        "KClassifier",
        "KTypeParameter",
        "KVariance",
        "Base64",
        "ExperimentalEncodingApi",
        "TimedValue",
        "AbstractLongTimeSource",
        "AbstractDoubleTimeSource",
        "TestTimeSource",
        "TimeSource",
        "ValueTimeMarkReading",
        "TimeMark",
        "ComparableTimeMark",
        "AdjustedTimeMark",
        "ExperimentalTime",
        "Duration",
        "WasExperimental",
        "BuilderInference",
        "OverloadResolutionByLambdaReturnType",
        "ThreadLocal",
        "SharedImmutable",
        "CName",
        "ObjCName",
        "HidesFromObjC",
        "HiddenFromObjC",
        "RefinesInSwift",
        "ShouldRefineInSwift",
        "ExperimentalMultiplatform",
        "OptionalExpectation",
        "RequiresOptIn",
        "OptIn",
        "ExperimentalSubclassOptIn",
        "SubclassOptInRequired",
        "ConsistentCopyVisibility",
        "ExposedCopyVisibility",
        "ExperimentalStdlibApi",
        "Delegates",
        "NotNullVar",
        "ObservableProperty",
        "ReadOnlyProperty",
        "ReadWriteProperty",
        "PropertyDelegateProvider",
        "ContinuationInterceptor",
        "AbstractCoroutineContextElement",
        "AbstractCoroutineContextKey",
        "EmptyCoroutineContext",
        "CombinedContext",
        "CoroutineSingletons",
        "CoroutineContext",
        "Continuation",
        "RestrictsSuspension",
        "DelimitedRangesSequence",
        "MatchGroupCollection",
        "MatchNamedGroupCollection",
        "MatchResult",
        "HexFormat",
        "Typography",
        "XorWowRandom",
        "Random",
        "IndexingIterator",
        "AbstractList",
        "EmptyIterator",
        "EmptyList",
        "ArrayAsCollection",
        "AbstractMap",
        "State",
        "AbstractIterator",
        "AbstractCollection",
        "IndexingIterable",
        "ReversedListReadOnly",
        "ReversedList",
        "ByteIterator",
        "CharIterator",
        "ShortIterator",
        "IntIterator",
        "LongIterator",
        "FloatIterator",
        "DoubleIterator",
        "BooleanIterator",
        "IndexedValue",
        "EmptySequence",
        "FilteringSequence",
        "TransformingSequence",
        "TransformingIndexedSequence",
        "IndexingSequence",
        "MergingSequence",
        "FlatteningSequence",
        "DropTakeSequence",
        "SubSequence",
        "TakeSequence",
        "TakeWhileSequence",
        "DropSequence",
        "DropWhileSequence",
        "DistinctSequence",
        "DistinctIterator",
        "GeneratorSequence",
        "SequenceScope",
        "SequenceBuilderIterator",
        "EmptyMap",
        "MovingSubList",
        "RingBuffer",
        "MapWithDefault",
        "MutableMapWithDefault",
        "MapWithDefaultImpl",
        "MutableMapWithDefaultImpl",
        "ArrayDeque",
        "Grouping",
        "Sequence",
    )
  }
}

object KTStdlibTypes {
  val KTStdLib_EnumEntries: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "enums",
              "EnumEntries",
          )
      )
  val KTStdLib_EnumEntriesList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "enums",
              "EnumEntriesList",
          )
      )
  val KTStdLib_Iterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Iterable",
          )
      )
  val KTStdLib_MutableIterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableIterable",
          )
      )
  val KTStdLib_Collection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Collection",
          )
      )
  val KTStdLib_MutableCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableCollection",
          )
      )
  val KTStdLib_List: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "List",
          )
      )
  val KTStdLib_MutableList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableList",
          )
      )
  val KTStdLib_Set: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Set",
          )
      )
  val KTStdLib_MutableSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableSet",
          )
      )
  val KTStdLib_Map: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Map",
          )
      )
  val KTStdLib_MutableMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableMap",
          )
      )
  val KTStdLib_PlatformThreadLocalRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "jdk8",
              "PlatformThreadLocalRandom",
          )
      )
  val KTStdLib_PathWalkOption: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "PathWalkOption",
          )
      )
  val KTStdLib_ExperimentalPathApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "ExperimentalPathApi",
          )
      )
  val KTStdLib_OnErrorResult: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "OnErrorResult",
          )
      )
  val KTStdLib_CopyActionResult: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "CopyActionResult",
          )
      )
  val KTStdLib_FileVisitorBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "FileVisitorBuilder",
          )
      )
  val KTStdLib_FileVisitorBuilderImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "FileVisitorBuilderImpl",
          )
      )
  val KTStdLib_FileVisitorImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "FileVisitorImpl",
          )
      )
  val KTStdLib_CopyActionContext: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "CopyActionContext",
          )
      )
  val KTStdLib_PathRelativizer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "PathRelativizer",
          )
      )
  val KTStdLib_DefaultCopyActionContext: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "DefaultCopyActionContext",
          )
      )
  val KTStdLib_ExceptionsCollector: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "ExceptionsCollector",
          )
      )
  val KTStdLib_IllegalFileNameException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "IllegalFileNameException",
          )
      )
  val KTStdLib_PathTreeWalk: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "PathTreeWalk",
          )
      )
  val KTStdLib_PathNode: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "PathNode",
          )
      )
  val KTStdLib_LinkFollowing: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "LinkFollowing",
          )
      )
  val KTStdLib_DirectoryEntriesReader: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "path",
              "DirectoryEntriesReader",
          )
      )
  val KTStdLib_UByteArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UByteArray",
          )
      )
  val KTStdLib_ULongRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ULongRange",
          )
      )
  val KTStdLib_ULongProgression: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ULongProgression",
          )
      )
  val KTStdLib_ULongProgressionIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ULongProgressionIterator",
          )
      )
  val KTStdLib_UInt: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UInt",
          )
      )
  val KTStdLib_UIntRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "UIntRange",
          )
      )
  val KTStdLib_UIntProgression: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "UIntProgression",
          )
      )
  val KTStdLib_UIntProgressionIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "UIntProgressionIterator",
          )
      )
  val KTStdLib_ExperimentalUnsignedTypes: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExperimentalUnsignedTypes",
          )
      )
  val KTStdLib_UByte: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UByte",
          )
      )
  val KTStdLib_UShort: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UShort",
          )
      )
  val KTStdLib_ULongArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ULongArray",
          )
      )
  val KTStdLib_ULong: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ULong",
          )
      )
  val KTStdLib_UIntArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UIntArray",
          )
      )
  val KTStdLib_UShortArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UShortArray",
          )
      )
  val KTStdLib_Ctor: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Ctor",
          )
      )
  val KTStdLib_JsArrayView: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "JsArrayView",
          )
      )
  val KTStdLib_JsSetView: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "JsSetView",
          )
      )
  val KTStdLib_JsMapView: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "JsMapView",
          )
      )
  val KTStdLib_JsObject: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsObject",
          )
      )
  val KTStdLib_DefaultConstructorMarker: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "DefaultConstructorMarker",
          )
      )
  val KTStdLib_JsIntrinsic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsIntrinsic",
          )
      )
  val KTStdLib_Metadata: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Metadata",
          )
      )
  val KTStdLib_FunctionAdapter: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "FunctionAdapter",
          )
      )
  val KTStdLib_IrLinkageError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "IrLinkageError",
          )
      )
  val KTStdLib_DoNotIntrinsify: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "DoNotIntrinsify",
          )
      )
  val KTStdLib_JsFun: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsFun",
          )
      )
  val KTStdLib_JsGenerator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsGenerator",
          )
      )
  val KTStdLib_JsImplicitExport: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsImplicitExport",
          )
      )
  val KTStdLib_ArrayBuffer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ArrayBuffer",
          )
      )
  val KTStdLib_Float64Array: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Float64Array",
          )
      )
  val KTStdLib_Float32Array: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Float32Array",
          )
      )
  val KTStdLib_Int32Array: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Int32Array",
          )
      )
  val KTStdLib_String: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "String",
          )
      )
  val KTStdLib_Enum: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Enum",
          )
      )
  val KTStdLib_Throwable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Throwable",
          )
      )
  val KTStdLib_ByteArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ByteArray",
          )
      )
  val KTStdLib_CharArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "CharArray",
          )
      )
  val KTStdLib_ShortArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ShortArray",
          )
      )
  val KTStdLib_IntArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "IntArray",
          )
      )
  val KTStdLib_LongArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "LongArray",
          )
      )
  val KTStdLib_FloatArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "FloatArray",
          )
      )
  val KTStdLib_DoubleArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DoubleArray",
          )
      )
  val KTStdLib_BooleanArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "BooleanArray",
          )
      )
  val KTStdLib_Boolean: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Boolean",
          )
      )
  val KTStdLib_Byte: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Byte",
          )
      )
  val KTStdLib_Short: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Short",
          )
      )
  val KTStdLib_Int: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Int",
          )
      )
  val KTStdLib_Long: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Long",
          )
      )
  val KTStdLib_Float: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Float",
          )
      )
  val KTStdLib_Double: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Double",
          )
      )
  val KTStdLib_Char: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Char",
          )
      )
  val KTStdLib_Letter: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Letter",
          )
      )
  val KTStdLib_Digit: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Digit",
          )
      )
  val KTStdLib_OtherLowercase: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "OtherLowercase",
          )
      )
  val KTStdLib_Category: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Category",
          )
      )
  val KTStdLib_Console: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Console",
          )
      )
  val KTStdLib_Volatile: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "Volatile",
          )
      )
  val KTStdLib_Synchronized: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "Synchronized",
          )
      )
  val KTStdLib_Error: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Error",
          )
      )
  val KTStdLib_Exception: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Exception",
          )
      )
  val KTStdLib_RuntimeException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "RuntimeException",
          )
      )
  val KTStdLib_IllegalArgumentException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "IllegalArgumentException",
          )
      )
  val KTStdLib_IllegalStateException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "IllegalStateException",
          )
      )
  val KTStdLib_IndexOutOfBoundsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "IndexOutOfBoundsException",
          )
      )
  val KTStdLib_ConcurrentModificationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ConcurrentModificationException",
          )
      )
  val KTStdLib_UnsupportedOperationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UnsupportedOperationException",
          )
      )
  val KTStdLib_NumberFormatException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NumberFormatException",
          )
      )
  val KTStdLib_NullPointerException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NullPointerException",
          )
      )
  val KTStdLib_ClassCastException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ClassCastException",
          )
      )
  val KTStdLib_AssertionError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "AssertionError",
          )
      )
  val KTStdLib_NoSuchElementException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NoSuchElementException",
          )
      )
  val KTStdLib_ArithmeticException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ArithmeticException",
          )
      )
  val KTStdLib_NoWhenBranchMatchedException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NoWhenBranchMatchedException",
          )
      )
  val KTStdLib_UninitializedPropertyAccessException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UninitializedPropertyAccessException",
          )
      )
  val KTStdLib_JsPolyfill: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsPolyfill",
          )
      )
  val KTStdLib_RegExp: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "RegExp",
          )
      )
  val KTStdLib_RegExpMatch: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "RegExpMatch",
          )
      )
  val KTStdLib_JsMath: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsMath",
          )
      )
  val KTStdLib_ExceptionTraceBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExceptionTraceBuilder",
          )
      )
  val KTStdLib_Date: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Date",
          )
      )
  val KTStdLib_Json: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Json",
          )
      )
  val KTStdLib_KType: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KType",
          )
      )
  val KTStdLib_KFunction: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KFunction",
          )
      )
  val KTStdLib_ExperimentalAssociatedObjects: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "ExperimentalAssociatedObjects",
          )
      )
  val KTStdLib_AssociatedObjectKey: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "AssociatedObjectKey",
          )
      )
  val KTStdLib_KProperty: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KProperty",
          )
      )
  val KTStdLib_KMutableProperty: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KMutableProperty",
          )
      )
  val KTStdLib_KProperty0: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KProperty0",
          )
      )
  val KTStdLib_KMutableProperty0: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KMutableProperty0",
          )
      )
  val KTStdLib_KProperty1: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KProperty1",
          )
      )
  val KTStdLib_KMutableProperty1: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KMutableProperty1",
          )
      )
  val KTStdLib_KProperty2: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KProperty2",
          )
      )
  val KTStdLib_KMutableProperty2: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KMutableProperty2",
          )
      )
  val KTStdLib_KCallable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KCallable",
          )
      )
  val KTStdLib_KClass: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KClass",
          )
      )
  val KTStdLib_JsClass: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsClass",
          )
      )
  val KTStdLib_EnumEntriesSerializationProxy: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "enums",
              "EnumEntriesSerializationProxy",
          )
      )
  val KTStdLib_DefaultTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "DefaultTimeSource",
          )
      )
  val KTStdLib_MonotonicTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "MonotonicTimeSource",
          )
      )
  val KTStdLib_Process: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "Process",
          )
      )
  val KTStdLib_HrTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "HrTimeSource",
          )
      )
  val KTStdLib_PerformanceTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "PerformanceTimeSource",
          )
      )
  val KTStdLib_DateNowTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "DateNowTimeSource",
          )
      )
  val KTStdLib_GlobalPerformance: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "GlobalPerformance",
          )
      )
  val KTStdLib_Performance: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "Performance",
          )
      )
  val KTStdLib_DurationUnit: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "DurationUnit",
          )
      )
  val KTStdLib_Comparator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Comparator",
          )
      )
  val KTStdLib_BaseOutput: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "BaseOutput",
          )
      )
  val KTStdLib_NodeJsOutput: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "NodeJsOutput",
          )
      )
  val KTStdLib_OutputToConsoleLog: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "OutputToConsoleLog",
          )
      )
  val KTStdLib_BufferedOutput: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "BufferedOutput",
          )
      )
  val KTStdLib_BufferedOutputToConsoleLog: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "BufferedOutputToConsoleLog",
          )
      )
  val KTStdLib_Promise: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Promise",
          )
      )
  val KTStdLib_JsReadonlyArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsReadonlyArray",
          )
      )
  val KTStdLib_JsArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsArray",
          )
      )
  val KTStdLib_JsReadonlySet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsReadonlySet",
          )
      )
  val KTStdLib_JsSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsSet",
          )
      )
  val KTStdLib_JsReadonlyMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsReadonlyMap",
          )
      )
  val KTStdLib_JsMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "collections",
              "JsMap",
          )
      )
  val KTStdLib_CancellationException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "cancellation",
              "CancellationException",
          )
      )
  val KTStdLib_JsIterationStep: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "JsIterationStep",
          )
      )
  val KTStdLib_JsIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "JsIterator",
          )
      )
  val KTStdLib_GeneratorCoroutineImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "GeneratorCoroutineImpl",
          )
      )
  val KTStdLib_SafeContinuation: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "SafeContinuation",
          )
      )
  val KTStdLib_CoroutineImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "CoroutineImpl",
          )
      )
  val KTStdLib_CompletedContinuation: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "CompletedContinuation",
          )
      )
  val KTStdLib_InterceptedCoroutine: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "InterceptedCoroutine",
          )
      )
  val KTStdLib_RegexOption: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "RegexOption",
          )
      )
  val KTStdLib_MatchGroup: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "MatchGroup",
          )
      )
  val KTStdLib_Regex: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Regex",
          )
      )
  val KTStdLib_Appendable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Appendable",
          )
      )
  val KTStdLib_CharacterCodingException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "CharacterCodingException",
          )
      )
  val KTStdLib_StringBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "StringBuilder",
          )
      )
  val KTStdLib_CharCategory: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "CharCategory",
          )
      )
  val KTStdLib_RandomAccess: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "RandomAccess",
          )
      )
  val KTStdLib_HashSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashSet",
          )
      )
  val KTStdLib_AbstractMutableMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractMutableMap",
          )
      )
  val KTStdLib_HashMapKeysDefault: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapKeysDefault",
          )
      )
  val KTStdLib_HashMapValuesDefault: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapValuesDefault",
          )
      )
  val KTStdLib_LinkedHashMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "LinkedHashMap",
          )
      )
  val KTStdLib_AbstractMutableSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractMutableSet",
          )
      )
  val KTStdLib_HashMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMap",
          )
      )
  val KTStdLib_LinkedHashSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "LinkedHashSet",
          )
      )
  val KTStdLib_JsRawArray: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "JsRawArray",
          )
      )
  val KTStdLib_InternalStringMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "InternalStringMap",
          )
      )
  val KTStdLib_InternalHashMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "InternalHashMap",
          )
      )
  val KTStdLib_AbstractMutableList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractMutableList",
          )
      )
  val KTStdLib_ArrayList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ArrayList",
          )
      )
  val KTStdLib_AbstractMutableCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractMutableCollection",
          )
      )
  val KTStdLib_InternalStringLinkedMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "InternalStringLinkedMap",
          )
      )
  val KTStdLib_InternalMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "InternalMap",
          )
      )
  val KTStdLib_HashMapKeys: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapKeys",
          )
      )
  val KTStdLib_HashMapValues: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapValues",
          )
      )
  val KTStdLib_HashMapEntrySetBase: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapEntrySetBase",
          )
      )
  val KTStdLib_HashMapEntrySet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "HashMapEntrySet",
          )
      )
  val KTStdLib_AutoCloseable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "AutoCloseable",
          )
      )
  val KTStdLib_ConstrainedOnceSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "ConstrainedOnceSequence",
          )
      )
  val KTStdLib_Serializable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "Serializable",
          )
      )
  val KTStdLib_JsName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsName",
          )
      )
  val KTStdLib_JsFileName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsFileName",
          )
      )
  val KTStdLib_JsModule: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsModule",
          )
      )
  val KTStdLib_JsNonModule: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsNonModule",
          )
      )
  val KTStdLib_JsQualifier: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsQualifier",
          )
      )
  val KTStdLib_JsExport: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsExport",
          )
      )
  val KTStdLib_EagerInitialization: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "EagerInitialization",
          )
      )
  val KTStdLib_JsExternalInheritorsOnly: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsExternalInheritorsOnly",
          )
      )
  val KTStdLib_JsExternalArgument: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsExternalArgument",
          )
      )
  val KTStdLib_JsStatic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsStatic",
          )
      )
  val KTStdLib_AtomicReference: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "concurrent",
              "AtomicReference",
          )
      )
  val KTStdLib_Dynamic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "Dynamic",
          )
      )
  val KTStdLib_JsAny: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsAny",
          )
      )
  val KTStdLib_JsString: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsString",
          )
      )
  val KTStdLib_JsReference: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsReference",
          )
      )
  val KTStdLib_JsBigInt: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsBigInt",
          )
      )
  val KTStdLib_JsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsException",
          )
      )
  val KTStdLib_JsBoolean: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsBoolean",
          )
      )
  val KTStdLib_JsNumber: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "JsNumber",
          )
      )
  val KTStdLib_Nothing: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Nothing",
          )
      )
  val KTStdLib_Array: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Array",
          )
      )
  val KTStdLib_Any: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Any",
          )
      )
  val KTStdLib_WasiErrorCode: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "WasiErrorCode",
          )
      )
  val KTStdLib_WasiError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "WasiError",
          )
      )
  val KTStdLib_OutOfMemoryError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "OutOfMemoryError",
          )
      )
  val KTStdLib_WasmImport: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "WasmImport",
          )
      )
  val KTStdLib_WasmExport: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "WasmExport",
          )
      )
  val KTStdLib_MemoryAllocator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "unsafe",
              "MemoryAllocator",
          )
      )
  val KTStdLib_ScopedMemoryAllocator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "unsafe",
              "ScopedMemoryAllocator",
          )
      )
  val KTStdLib_UnsafeWasmMemoryApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "unsafe",
              "UnsafeWasmMemoryApi",
          )
      )
  val KTStdLib_Pointer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "wasm",
              "unsafe",
              "Pointer",
          )
      )
  val KTStdLib_BitSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "BitSet",
          )
      )
  val KTStdLib_ObsoleteNativeApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "ObsoleteNativeApi",
          )
      )
  val KTStdLib_TypeCastException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "TypeCastException",
          )
      )
  val KTStdLib_JvmOverloads: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmOverloads",
          )
      )
  val KTStdLib_JvmStatic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmStatic",
          )
      )
  val KTStdLib_JvmName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmName",
          )
      )
  val KTStdLib_JvmMultifileClass: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmMultifileClass",
          )
      )
  val KTStdLib_JvmPackageName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmPackageName",
          )
      )
  val KTStdLib_JvmSynthetic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmSynthetic",
          )
      )
  val KTStdLib_Throws: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "Throws",
          )
      )
  val KTStdLib_ImplicitlyActualizedByJvmDeclaration: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "ImplicitlyActualizedByJvmDeclaration",
          )
      )
  val KTStdLib_JvmField: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmField",
          )
      )
  val KTStdLib_JvmSuppressWildcards: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmSuppressWildcards",
          )
      )
  val KTStdLib_JvmWildcard: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmWildcard",
          )
      )
  val KTStdLib_JvmInline: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmInline",
          )
      )
  val KTStdLib_JvmRecord: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmRecord",
          )
      )
  val KTStdLib_Transient: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "Transient",
          )
      )
  val KTStdLib_Strictfp: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "Strictfp",
          )
      )
  val KTStdLib_JvmSerializableLambda: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmSerializableLambda",
          )
      )
  val KTStdLib_KotlinReflectionNotSupportedError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "KotlinReflectionNotSupportedError",
          )
      )
  val KTStdLib_FunctionN: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "FunctionN",
          )
      )
  val KTStdLib_Function0: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function0",
          )
      )
  val KTStdLib_Function1: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function1",
          )
      )
  val KTStdLib_Function2: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function2",
          )
      )
  val KTStdLib_Function3: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function3",
          )
      )
  val KTStdLib_Function4: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function4",
          )
      )
  val KTStdLib_Function5: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function5",
          )
      )
  val KTStdLib_Function6: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function6",
          )
      )
  val KTStdLib_Function7: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function7",
          )
      )
  val KTStdLib_Function8: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function8",
          )
      )
  val KTStdLib_Function9: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function9",
          )
      )
  val KTStdLib_Function10: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function10",
          )
      )
  val KTStdLib_Function11: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function11",
          )
      )
  val KTStdLib_Function12: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function12",
          )
      )
  val KTStdLib_Function13: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function13",
          )
      )
  val KTStdLib_Function14: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function14",
          )
      )
  val KTStdLib_Function15: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function15",
          )
      )
  val KTStdLib_Function16: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function16",
          )
      )
  val KTStdLib_Function17: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function17",
          )
      )
  val KTStdLib_Function18: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function18",
          )
      )
  val KTStdLib_Function19: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function19",
          )
      )
  val KTStdLib_Function20: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function20",
          )
      )
  val KTStdLib_Function21: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function21",
          )
      )
  val KTStdLib_Function22: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "functions",
              "Function22",
          )
      )
  val KTStdLib_PurelyImplements: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "PurelyImplements",
          )
      )
  val KTStdLib_KotlinNullPointerException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "KotlinNullPointerException",
          )
      )
  val KTStdLib_Constants: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "math",
              "Constants",
          )
      )
  val KTStdLib_SynchronizedLazyImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "SynchronizedLazyImpl",
          )
      )
  val KTStdLib_SafePublicationLazyImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "SafePublicationLazyImpl",
          )
      )
  val KTStdLib_KParameter: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KParameter",
          )
      )
  val KTStdLib_KAnnotatedElement: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KAnnotatedElement",
          )
      )
  val KTStdLib_KVisibility: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KVisibility",
          )
      )
  val KTStdLib_KDeclarationContainer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KDeclarationContainer",
          )
      )
  val KTStdLib_TypeImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "TypeImpl",
          )
      )
  val KTStdLib_TypeVariableImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "TypeVariableImpl",
          )
      )
  val KTStdLib_GenericArrayTypeImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "GenericArrayTypeImpl",
          )
      )
  val KTStdLib_WildcardTypeImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "WildcardTypeImpl",
          )
      )
  val KTStdLib_ParameterizedTypeImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "ParameterizedTypeImpl",
          )
      )
  val KTStdLib_FileSystemException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "FileSystemException",
          )
      )
  val KTStdLib_FileAlreadyExistsException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "FileAlreadyExistsException",
          )
      )
  val KTStdLib_AccessDeniedException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "AccessDeniedException",
          )
      )
  val KTStdLib_NoSuchFileException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "NoSuchFileException",
          )
      )
  val KTStdLib_LinesSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "LinesSequence",
          )
      )
  val KTStdLib_DecodeInputStream: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "encoding",
              "DecodeInputStream",
          )
      )
  val KTStdLib_EncodeOutputStream: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "encoding",
              "EncodeOutputStream",
          )
      )
  val KTStdLib_OnErrorAction: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "OnErrorAction",
          )
      )
  val KTStdLib_TerminateException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "TerminateException",
          )
      )
  val KTStdLib_FileWalkDirection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "FileWalkDirection",
          )
      )
  val KTStdLib_FileTreeWalk: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "FileTreeWalk",
          )
      )
  val KTStdLib_FilePathComponents: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "FilePathComponents",
          )
      )
  val KTStdLib_LineReader: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "LineReader",
          )
      )
  val KTStdLib_ExposingBufferByteArrayOutputStream: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "ExposingBufferByteArrayOutputStream",
          )
      )
  val KTStdLib_JvmDefault: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmDefault",
          )
      )
  val KTStdLib_JvmDefaultWithoutCompatibility: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmDefaultWithoutCompatibility",
          )
      )
  val KTStdLib_JvmDefaultWithCompatibility: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "jvm",
              "JvmDefaultWithCompatibility",
          )
      )
  val KTStdLib_CharDirectionality: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "CharDirectionality",
          )
      )
  val KTStdLib_FlagEnum: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "FlagEnum",
          )
      )
  val KTStdLib_MatcherMatchResult: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "MatcherMatchResult",
          )
      )
  val KTStdLib_SystemProperties: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "SystemProperties",
          )
      )
  val KTStdLib_Charsets: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Charsets",
          )
      )
  val KTStdLib_ScreenFloatValueRegEx: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "ScreenFloatValueRegEx",
          )
      )
  val KTStdLib_AbstractPlatformRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "AbstractPlatformRandom",
          )
      )
  val KTStdLib_FallbackThreadLocalRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "FallbackThreadLocalRandom",
          )
      )
  val KTStdLib_PlatformRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "PlatformRandom",
          )
      )
  val KTStdLib_KotlinRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "KotlinRandom",
          )
      )
  val KTStdLib_ListBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "ListBuilder",
          )
      )
  val KTStdLib_SerializedCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "SerializedCollection",
          )
      )
  val KTStdLib_MapBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "MapBuilder",
          )
      )
  val KTStdLib_MapBuilderKeys: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "MapBuilderKeys",
          )
      )
  val KTStdLib_MapBuilderValues: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "MapBuilderValues",
          )
      )
  val KTStdLib_AbstractMapBuilderEntrySet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "AbstractMapBuilderEntrySet",
          )
      )
  val KTStdLib_MapBuilderEntries: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "MapBuilderEntries",
          )
      )
  val KTStdLib_SerializedMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "SerializedMap",
          )
      )
  val KTStdLib_SetBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "builders",
              "SetBuilder",
          )
      )
  val KTStdLib_ExperimentalJsFileName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ExperimentalJsFileName",
          )
      )
  val KTStdLib_ExperimentalJsExport: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ExperimentalJsExport",
          )
      )
  val KTStdLib_ExperimentalJsStatic: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ExperimentalJsStatic",
          )
      )
  val KTStdLib_ExperimentalJsReflectionCreateInstance: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ExperimentalJsReflectionCreateInstance",
          )
      )
  val KTStdLib_ExperimentalJsCollectionsApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "js",
              "ExperimentalJsCollectionsApi",
          )
      )
  val KTStdLib_ReadAfterEOFException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "ReadAfterEOFException",
          )
      )
  val KTStdLib_KotlinNothingValueException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "KotlinNothingValueException",
          )
      )
  val KTStdLib_ObsoleteWorkersApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "concurrent",
              "ObsoleteWorkersApi",
          )
      )
  val KTStdLib_FreezingIsDeprecated: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "FreezingIsDeprecated",
          )
      )
  val KTStdLib_SpecialToken: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SpecialToken",
          )
      )
  val KTStdLib_Lexer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "Lexer",
          )
      )
  val KTStdLib_Quantifier: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "Quantifier",
          )
      )
  val KTStdLib_NamedGroup: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "NamedGroup",
          )
      )
  val KTStdLib_UnicodeCategory: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "UnicodeCategory",
          )
      )
  val KTStdLib_UnicodeCategoryScope: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "UnicodeCategoryScope",
          )
      )
  val KTStdLib_AbstractCharClass: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AbstractCharClass",
          )
      )
  val KTStdLib_JointSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "JointSet",
          )
      )
  val KTStdLib_CharSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "CharSet",
          )
      )
  val KTStdLib_SimpleSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SimpleSet",
          )
      )
  val KTStdLib_AbstractSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AbstractSet",
          )
      )
  val KTStdLib_FixedLengthQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "FixedLengthQuantifierSet",
          )
      )
  val KTStdLib_ReluctantFixedLengthQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "ReluctantFixedLengthQuantifierSet",
          )
      )
  val KTStdLib_EmptySet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "EmptySet",
          )
      )
  val KTStdLib_LowSurrogateCharSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "LowSurrogateCharSet",
          )
      )
  val KTStdLib_HighSurrogateCharSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "HighSurrogateCharSet",
          )
      )
  val KTStdLib_ReluctantLeafQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "ReluctantLeafQuantifierSet",
          )
      )
  val KTStdLib_PossessiveFixedLengthQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PossessiveFixedLengthQuantifierSet",
          )
      )
  val KTStdLib_RangeSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "RangeSet",
          )
      )
  val KTStdLib_PossessiveGroupQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PossessiveGroupQuantifierSet",
          )
      )
  val KTStdLib_SequenceSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SequenceSet",
          )
      )
  val KTStdLib_FSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "FSet",
          )
      )
  val KTStdLib_FinalSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "FinalSet",
          )
      )
  val KTStdLib_NonCapFSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "NonCapFSet",
          )
      )
  val KTStdLib_AheadFSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AheadFSet",
          )
      )
  val KTStdLib_BehindFSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "BehindFSet",
          )
      )
  val KTStdLib_AtomicFSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AtomicFSet",
          )
      )
  val KTStdLib_SingleSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SingleSet",
          )
      )
  val KTStdLib_WordBoundarySet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "WordBoundarySet",
          )
      )
  val KTStdLib_DecomposedCharSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "DecomposedCharSet",
          )
      )
  val KTStdLib_LookAroundSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "LookAroundSet",
          )
      )
  val KTStdLib_HangulDecomposedCharSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "HangulDecomposedCharSet",
          )
      )
  val KTStdLib_QuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "QuantifierSet",
          )
      )
  val KTStdLib_LeafSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "LeafSet",
          )
      )
  val KTStdLib_DotQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "DotQuantifierSet",
          )
      )
  val KTStdLib_PositiveLookAheadSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PositiveLookAheadSet",
          )
      )
  val KTStdLib_NegativeLookAheadSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "NegativeLookAheadSet",
          )
      )
  val KTStdLib_NonCapturingJointSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "NonCapturingJointSet",
          )
      )
  val KTStdLib_PositiveLookBehindSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PositiveLookBehindSet",
          )
      )
  val KTStdLib_NegativeLookBehindSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "NegativeLookBehindSet",
          )
      )
  val KTStdLib_LeafQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "LeafQuantifierSet",
          )
      )
  val KTStdLib_EOLSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "EOLSet",
          )
      )
  val KTStdLib_UnifiedQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "UnifiedQuantifierSet",
          )
      )
  val KTStdLib_PreviousMatchSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PreviousMatchSet",
          )
      )
  val KTStdLib_DotSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "DotSet",
          )
      )
  val KTStdLib_AtomicJointSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AtomicJointSet",
          )
      )
  val KTStdLib_EOISet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "EOISet",
          )
      )
  val KTStdLib_PossessiveLeafQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "PossessiveLeafQuantifierSet",
          )
      )
  val KTStdLib_SupplementaryRangeSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SupplementaryRangeSet",
          )
      )
  val KTStdLib_CompositeRangeSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "CompositeRangeSet",
          )
      )
  val KTStdLib_SOLSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SOLSet",
          )
      )
  val KTStdLib_GroupQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "GroupQuantifierSet",
          )
      )
  val KTStdLib_SurrogateRangeSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "SurrogateRangeSet",
          )
      )
  val KTStdLib_BackReferenceSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "BackReferenceSet",
          )
      )
  val KTStdLib_ReluctantGroupQuantifierSet: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "ReluctantGroupQuantifierSet",
          )
      )
  val KTStdLib_MatchResultImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "MatchResultImpl",
          )
      )
  val KTStdLib_CharClass: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "CharClass",
          )
      )
  val KTStdLib_Pattern: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "Pattern",
          )
      )
  val KTStdLib_AbstractLineTerminator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "regex",
              "AbstractLineTerminator",
          )
      )
  val KTStdLib_PatternSyntaxException: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "PatternSyntaxException",
          )
      )
  val KTStdLib_Iterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Iterator",
          )
      )
  val KTStdLib_MutableIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableIterator",
          )
      )
  val KTStdLib_ListIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ListIterator",
          )
      )
  val KTStdLib_MutableListIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableListIterator",
          )
      )
  val KTStdLib_CharProgression: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "CharProgression",
          )
      )
  val KTStdLib_IntProgression: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "IntProgression",
          )
      )
  val KTStdLib_LongProgression: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "LongProgression",
          )
      )
  val KTStdLib_CharRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "CharRange",
          )
      )
  val KTStdLib_IntRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "IntRange",
          )
      )
  val KTStdLib_LongRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "LongRange",
          )
      )
  val KTStdLib_ComparableRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ComparableRange",
          )
      )
  val KTStdLib_ComparableOpenEndRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ComparableOpenEndRange",
          )
      )
  val KTStdLib_ClosedFloatingPointRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ClosedFloatingPointRange",
          )
      )
  val KTStdLib_ClosedDoubleRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ClosedDoubleRange",
          )
      )
  val KTStdLib_OpenEndDoubleRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "OpenEndDoubleRange",
          )
      )
  val KTStdLib_ClosedFloatRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ClosedFloatRange",
          )
      )
  val KTStdLib_OpenEndFloatRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "OpenEndFloatRange",
          )
      )
  val KTStdLib_CharProgressionIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "CharProgressionIterator",
          )
      )
  val KTStdLib_IntProgressionIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "IntProgressionIterator",
          )
      )
  val KTStdLib_LongProgressionIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "LongProgressionIterator",
          )
      )
  val KTStdLib_ClosedRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "ClosedRange",
          )
      )
  val KTStdLib_OpenEndRange: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ranges",
              "OpenEndRange",
          )
      )
  val KTStdLib_ExperimentalTypeInference: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "experimental",
              "ExperimentalTypeInference",
          )
      )
  val KTStdLib_ExperimentalObjCRefinement: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "experimental",
              "ExperimentalObjCRefinement",
          )
      )
  val KTStdLib_ExperimentalObjCName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "experimental",
              "ExperimentalObjCName",
          )
      )
  val KTStdLib_ExperimentalNativeApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "experimental",
              "ExperimentalNativeApi",
          )
      )
  val KTStdLib_ReversedComparator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "comparisons",
              "ReversedComparator",
          )
      )
  val KTStdLib_NaturalOrderComparator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "comparisons",
              "NaturalOrderComparator",
          )
      )
  val KTStdLib_ReverseOrderComparator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "comparisons",
              "ReverseOrderComparator",
          )
      )
  val KTStdLib_DeepRecursiveFunction: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DeepRecursiveFunction",
          )
      )
  val KTStdLib_DeepRecursiveScope: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DeepRecursiveScope",
          )
      )
  val KTStdLib_DeepRecursiveScopeImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "DeepRecursiveScopeImpl",
          )
      )
  val KTStdLib_Result: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Result",
          )
      )
  val KTStdLib_Lazy: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Lazy",
          )
      )
  val KTStdLib_LazyThreadSafetyMode: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "LazyThreadSafetyMode",
          )
      )
  val KTStdLib_UnsafeLazyImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "UnsafeLazyImpl",
          )
      )
  val KTStdLib_InitializedLazyImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "InitializedLazyImpl",
          )
      )
  val KTStdLib_Pair: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Pair",
          )
      )
  val KTStdLib_Triple: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "Triple",
          )
      )
  val KTStdLib_NotImplementedError: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "NotImplementedError",
          )
      )
  val KTStdLib_KotlinVersion: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "KotlinVersion",
          )
      )
  val KTStdLib_KotlinVersionCurrentValue: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "KotlinVersionCurrentValue",
          )
      )
  val KTStdLib_Effect: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "Effect",
          )
      )
  val KTStdLib_ConditionalEffect: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "ConditionalEffect",
          )
      )
  val KTStdLib_SimpleEffect: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "SimpleEffect",
          )
      )
  val KTStdLib_Returns: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "Returns",
          )
      )
  val KTStdLib_ReturnsNotNull: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "ReturnsNotNull",
          )
      )
  val KTStdLib_CallsInPlace: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "CallsInPlace",
          )
      )
  val KTStdLib_ExperimentalContracts: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "ExperimentalContracts",
          )
      )
  val KTStdLib_ContractBuilder: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "ContractBuilder",
          )
      )
  val KTStdLib_InvocationKind: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "contracts",
              "InvocationKind",
          )
      )
  val KTStdLib_KTypeProjection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KTypeProjection",
          )
      )
  val KTStdLib_KClassifier: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KClassifier",
          )
      )
  val KTStdLib_KTypeParameter: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KTypeParameter",
          )
      )
  val KTStdLib_KVariance: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "reflect",
              "KVariance",
          )
      )
  val KTStdLib_Base64: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "encoding",
              "Base64",
          )
      )
  val KTStdLib_ExperimentalEncodingApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "io",
              "encoding",
              "ExperimentalEncodingApi",
          )
      )
  val KTStdLib_TimedValue: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "TimedValue",
          )
      )
  val KTStdLib_AbstractLongTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "AbstractLongTimeSource",
          )
      )
  val KTStdLib_AbstractDoubleTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "AbstractDoubleTimeSource",
          )
      )
  val KTStdLib_TestTimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "TestTimeSource",
          )
      )
  val KTStdLib_TimeSource: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "TimeSource",
          )
      )
  val KTStdLib_ValueTimeMarkReading: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "ValueTimeMarkReading",
          )
      )
  val KTStdLib_TimeMark: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "TimeMark",
          )
      )
  val KTStdLib_ComparableTimeMark: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "ComparableTimeMark",
          )
      )
  val KTStdLib_AdjustedTimeMark: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "AdjustedTimeMark",
          )
      )
  val KTStdLib_ExperimentalTime: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "ExperimentalTime",
          )
      )
  val KTStdLib_Duration: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "time",
              "Duration",
          )
      )
  val KTStdLib_WasExperimental: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "WasExperimental",
          )
      )
  val KTStdLib_BuilderInference: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "BuilderInference",
          )
      )
  val KTStdLib_OverloadResolutionByLambdaReturnType: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "OverloadResolutionByLambdaReturnType",
          )
      )
  val KTStdLib_ThreadLocal: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "concurrent",
              "ThreadLocal",
          )
      )
  val KTStdLib_SharedImmutable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "concurrent",
              "SharedImmutable",
          )
      )
  val KTStdLib_CName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "CName",
          )
      )
  val KTStdLib_ObjCName: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "ObjCName",
          )
      )
  val KTStdLib_HidesFromObjC: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "HidesFromObjC",
          )
      )
  val KTStdLib_HiddenFromObjC: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "HiddenFromObjC",
          )
      )
  val KTStdLib_RefinesInSwift: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "RefinesInSwift",
          )
      )
  val KTStdLib_ShouldRefineInSwift: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "native",
              "ShouldRefineInSwift",
          )
      )
  val KTStdLib_ExperimentalMultiplatform: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExperimentalMultiplatform",
          )
      )
  val KTStdLib_OptionalExpectation: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "OptionalExpectation",
          )
      )
  val KTStdLib_RequiresOptIn: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "RequiresOptIn",
          )
      )
  val KTStdLib_OptIn: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "OptIn",
          )
      )
  val KTStdLib_ExperimentalSubclassOptIn: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExperimentalSubclassOptIn",
          )
      )
  val KTStdLib_SubclassOptInRequired: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "SubclassOptInRequired",
          )
      )
  val KTStdLib_ConsistentCopyVisibility: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ConsistentCopyVisibility",
          )
      )
  val KTStdLib_ExposedCopyVisibility: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExposedCopyVisibility",
          )
      )
  val KTStdLib_ExperimentalStdlibApi: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "ExperimentalStdlibApi",
          )
      )
  val KTStdLib_Delegates: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "Delegates",
          )
      )
  val KTStdLib_NotNullVar: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "NotNullVar",
          )
      )
  val KTStdLib_ObservableProperty: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "ObservableProperty",
          )
      )
  val KTStdLib_ReadOnlyProperty: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "ReadOnlyProperty",
          )
      )
  val KTStdLib_ReadWriteProperty: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "ReadWriteProperty",
          )
      )
  val KTStdLib_PropertyDelegateProvider: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "properties",
              "PropertyDelegateProvider",
          )
      )
  val KTStdLib_ContinuationInterceptor: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "ContinuationInterceptor",
          )
      )
  val KTStdLib_AbstractCoroutineContextElement: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "AbstractCoroutineContextElement",
          )
      )
  val KTStdLib_AbstractCoroutineContextKey: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "AbstractCoroutineContextKey",
          )
      )
  val KTStdLib_EmptyCoroutineContext: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "EmptyCoroutineContext",
          )
      )
  val KTStdLib_CombinedContext: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "CombinedContext",
          )
      )
  val KTStdLib_CoroutineSingletons: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "intrinsics",
              "CoroutineSingletons",
          )
      )
  val KTStdLib_CoroutineContext: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "CoroutineContext",
          )
      )
  val KTStdLib_Continuation: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "Continuation",
          )
      )
  val KTStdLib_RestrictsSuspension: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "coroutines",
              "RestrictsSuspension",
          )
      )
  val KTStdLib_DelimitedRangesSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "DelimitedRangesSequence",
          )
      )
  val KTStdLib_MatchGroupCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "MatchGroupCollection",
          )
      )
  val KTStdLib_MatchNamedGroupCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "MatchNamedGroupCollection",
          )
      )
  val KTStdLib_MatchResult: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "MatchResult",
          )
      )
  val KTStdLib_HexFormat: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "HexFormat",
          )
      )
  val KTStdLib_Typography: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "text",
              "Typography",
          )
      )
  val KTStdLib_XorWowRandom: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "XorWowRandom",
          )
      )
  val KTStdLib_Random: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "random",
              "Random",
          )
      )
  val KTStdLib_IndexingIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "IndexingIterator",
          )
      )
  val KTStdLib_AbstractList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractList",
          )
      )
  val KTStdLib_EmptyIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "EmptyIterator",
          )
      )
  val KTStdLib_EmptyList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "EmptyList",
          )
      )
  val KTStdLib_ArrayAsCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ArrayAsCollection",
          )
      )
  val KTStdLib_AbstractMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractMap",
          )
      )
  val KTStdLib_State: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "State",
          )
      )
  val KTStdLib_AbstractIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractIterator",
          )
      )
  val KTStdLib_AbstractCollection: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "AbstractCollection",
          )
      )
  val KTStdLib_IndexingIterable: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "IndexingIterable",
          )
      )
  val KTStdLib_ReversedListReadOnly: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ReversedListReadOnly",
          )
      )
  val KTStdLib_ReversedList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ReversedList",
          )
      )
  val KTStdLib_ByteIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ByteIterator",
          )
      )
  val KTStdLib_CharIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "CharIterator",
          )
      )
  val KTStdLib_ShortIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ShortIterator",
          )
      )
  val KTStdLib_IntIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "IntIterator",
          )
      )
  val KTStdLib_LongIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "LongIterator",
          )
      )
  val KTStdLib_FloatIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "FloatIterator",
          )
      )
  val KTStdLib_DoubleIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "DoubleIterator",
          )
      )
  val KTStdLib_BooleanIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "BooleanIterator",
          )
      )
  val KTStdLib_IndexedValue: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "IndexedValue",
          )
      )
  val KTStdLib_EmptySequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "EmptySequence",
          )
      )
  val KTStdLib_FilteringSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "FilteringSequence",
          )
      )
  val KTStdLib_TransformingSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "TransformingSequence",
          )
      )
  val KTStdLib_TransformingIndexedSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "TransformingIndexedSequence",
          )
      )
  val KTStdLib_IndexingSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "IndexingSequence",
          )
      )
  val KTStdLib_MergingSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "MergingSequence",
          )
      )
  val KTStdLib_FlatteningSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "FlatteningSequence",
          )
      )
  val KTStdLib_DropTakeSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "DropTakeSequence",
          )
      )
  val KTStdLib_SubSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "SubSequence",
          )
      )
  val KTStdLib_TakeSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "TakeSequence",
          )
      )
  val KTStdLib_TakeWhileSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "TakeWhileSequence",
          )
      )
  val KTStdLib_DropSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "DropSequence",
          )
      )
  val KTStdLib_DropWhileSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "DropWhileSequence",
          )
      )
  val KTStdLib_DistinctSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "DistinctSequence",
          )
      )
  val KTStdLib_DistinctIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "DistinctIterator",
          )
      )
  val KTStdLib_GeneratorSequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "GeneratorSequence",
          )
      )
  val KTStdLib_SequenceScope: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "SequenceScope",
          )
      )
  val KTStdLib_SequenceBuilderIterator: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "SequenceBuilderIterator",
          )
      )
  val KTStdLib_EmptyMap: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "EmptyMap",
          )
      )
  val KTStdLib_MovingSubList: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MovingSubList",
          )
      )
  val KTStdLib_RingBuffer: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "RingBuffer",
          )
      )
  val KTStdLib_MapWithDefault: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MapWithDefault",
          )
      )
  val KTStdLib_MutableMapWithDefault: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableMapWithDefault",
          )
      )
  val KTStdLib_MapWithDefaultImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MapWithDefaultImpl",
          )
      )
  val KTStdLib_MutableMapWithDefaultImpl: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "MutableMapWithDefaultImpl",
          )
      )
  val KTStdLib_ArrayDeque: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "ArrayDeque",
          )
      )
  val KTStdLib_Grouping: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "collections",
              "Grouping",
          )
      )
  val KTStdLib_Sequence: FullTypeQualifier =
      FullTypeQualifier.unsafeBuildQualifier(
          listOf(
              "kotlin",
              "sequences",
              "Sequence",
          )
      )
}

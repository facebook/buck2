#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.


"""
Verifies that an archive contains only allowed entries.
"""

import re
import unittest
from zipfile import ZipFile

import pkg_resources


ALLOWED_ENTRIES = """
com/facebook/buck/core/util/log/appendablelogrecord/AppendableLogRecord.class
com/facebook/buck/jvm/java/runner/FileClassPathRunner.class
com/facebook/buck/jvm/java/version/utils/JavaVersionUtils.class
com/facebook/buck/test/result/type/ResultType.class
com/facebook/buck/test/selectors/Nullable.class
com/facebook/buck/test/selectors/PatternTestSelector.class
com/facebook/buck/test/selectors/SimpleTestSelector.class
com/facebook/buck/test/selectors/TestDescription.class
com/facebook/buck/test/selectors/TestSelector.class
com/facebook/buck/test/selectors/TestSelectorList.class
com/facebook/buck/test/selectors/TestSelectorParseException.class
com/facebook/buck/testresultsoutput/TestResultsOutputEvent.class
com/facebook/buck/testresultsoutput/TestResultsOutputSender.class
com/facebook/buck/testrunner/BaseRunner.class
com/facebook/buck/testrunner/BuckBlockJUnit4ClassRunner.class
com/facebook/buck/testrunner/BuckXmlTestRunListener.class
com/facebook/buck/testrunner/CheckDependency.class
com/facebook/buck/testrunner/CrashCapturer.class
com/facebook/buck/testrunner/DelegateRunNotifier.class
com/facebook/buck/testrunner/DelegateRunnerWithTimeout.class
com/facebook/buck/testrunner/DeviceRunner.class
com/facebook/buck/testrunner/InstrumentationMain.class
com/facebook/buck/testrunner/InstrumentationTestRunner.class
com/facebook/buck/testrunner/InstrumentationMainForClout.class
com/facebook/buck/testrunner/InstrumentationTestRunnerForClout.class
com/facebook/buck/testrunner/RunShellCommand.class
com/facebook/buck/testrunner/LogcatBuffer.class
com/facebook/buck/testrunner/JUnitMain.class
com/facebook/buck/testrunner/JUnitRunner.class
com/facebook/buck/testrunner/JUnitOptions.class
com/facebook/buck/testrunner/JUnitSupport.class
com/facebook/buck/testrunner/JupiterMain.class
com/facebook/buck/testrunner/JupiterRunner.class
com/facebook/buck/testrunner/JulLogFormatter.class
com/facebook/buck/testrunner/SameThreadFailOnTimeout.class
com/facebook/buck/testrunner/SkipTestCondition.class
com/facebook/buck/testrunner/StandardOutputRecorder.class
com/facebook/buck/testrunner/TestRecorder.class
com/facebook/buck/testrunner/TestNGMain.class
com/facebook/buck/testrunner/TestNGRunner.class
com/facebook/buck/testrunner/TestResult.class
com/facebook/buck/testrunner/TestXmlEscaper.class
com/facebook/buck/testrunner/TpxStandardOutputTestListener.class
com/facebook/buck/testrunner/InstrumentationTpxStandardOutputTestListener.class
com/facebook/buck/testrunner/reportlayer/ReportLayer.class
com/facebook/buck/testrunner/reportlayer/VideoRecordingReportLayer.class
com/facebook/buck/testrunner/reportlayer/TombstonesReportLayer.class
com/facebook/buck/testrunner/reportlayer/LogExtractorReportLayer.class
com/facebook/buck/util/concurrent/MostExecutors.class
com/facebook/buck/util/environment/Architecture.class
com/facebook/buck/util/environment/Platform.class
com/facebook/buck/util/environment/PlatformType.class
com/facebook/buck/util/escaper/BashEscaper.class
com/facebook/buck/util/escaper/EscaperUtils.class
com/facebook/buck/util/escaper/Quoter.class
META-INF/services/org.junit.jupiter.api.extension.Extension
com/fasterxml/jackson/core/json/async/NonBlockingUtf8JsonParserBase.class
com/fasterxml/jackson/core/util/BufferRecyclers.class
com/fasterxml/jackson/core/TreeCodec.class
com/fasterxml/jackson/core/JsonGenerationException.class
com/fasterxml/jackson/core/json/ByteSourceJsonBootstrapper.class
com/fasterxml/jackson/core/io/BigIntegerParser.class
com/fasterxml/jackson/core/io/schubfach/MathUtils.class
com/fasterxml/jackson/core/json/JsonWriteContext.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/FastDoubleMath.class
com/fasterxml/jackson/core/json/JsonGeneratorImpl.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaDoubleParser.class
com/fasterxml/jackson/core/util/ByteArrayBuilder.class
com/fasterxml/jackson/core/package-info.class
com/fasterxml/jackson/core/io/MergedStream.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigDecimalFromCharSequence.class
com/fasterxml/jackson/core/async/ByteBufferFeeder.class
com/fasterxml/jackson/core/format/InputAccessor.class
com/fasterxml/jackson/core/json/JsonReadFeature.class
com/fasterxml/jackson/core/StreamWriteCapability.class
com/fasterxml/jackson/core/JsonParser.class
com/fasterxml/jackson/core/TreeNode.class
com/fasterxml/jackson/core/util/Separators.class
com/fasterxml/jackson/core/exc/StreamWriteException.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigDecimalFromByteArray.class
com/fasterxml/jackson/core/util/MinimalPrettyPrinter.class
com/fasterxml/jackson/core/util/ThreadLocalBufferManager.class
com/fasterxml/jackson/core/json/package-info.class
com/fasterxml/jackson/core/json/JsonReadContext.class
com/fasterxml/jackson/core/base/ParserBase.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigIntegerParser.class
com/fasterxml/jackson/core/json/WriterBasedJsonGenerator.class
com/fasterxml/jackson/core/Base64Variants.class
com/fasterxml/jackson/core/json/DupDetector.class
com/fasterxml/jackson/core/exc/StreamReadException.class
com/fasterxml/jackson/core/json/async/NonBlockingByteBufferJsonParser.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/FastIntegerMath.class
com/fasterxml/jackson/core/JsonPointer.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractJavaFloatingPointBitsFromCharArray.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigIntegerFromByteArray.class
com/fasterxml/jackson/core/json/async/NonBlockingJsonParserBase.class
com/fasterxml/jackson/core/filter/FilteringGeneratorDelegate.class
com/fasterxml/jackson/core/json/async/package-info.class
com/fasterxml/jackson/core/io/NumberOutput.class
com/fasterxml/jackson/core/format/package-info.class
com/fasterxml/jackson/core/sym/package-info.class
com/fasterxml/jackson/core/base/GeneratorBase.class
com/fasterxml/jackson/core/JsonFactoryBuilder.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigDecimalParser.class
com/fasterxml/jackson/core/io/schubfach/DoubleToDecimal.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigDecimalFromCharArray.class
com/fasterxml/jackson/core/type/TypeReference.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaDoubleBitsFromCharSequence.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigIntegerFromCharArray.class
com/fasterxml/jackson/core/type/ResolvedType.class
com/fasterxml/jackson/core/JsonEncoding.class
com/fasterxml/jackson/core/exc/StreamConstraintsException.class
com/fasterxml/jackson/core/io/ContentReference.class
com/fasterxml/jackson/core/JsonTokenId.class
com/fasterxml/jackson/core/util/JsonParserDelegate.class
com/fasterxml/jackson/core/JsonToken.class
com/fasterxml/jackson/core/StreamReadCapability.class
com/fasterxml/jackson/core/util/VersionUtil.class
com/fasterxml/jackson/core/io/IOContext.class
com/fasterxml/jackson/core/Versioned.class
com/fasterxml/jackson/core/type/package-info.class
com/fasterxml/jackson/core/filter/TokenFilter.class
com/fasterxml/jackson/core/exc/InputCoercionException.class
com/fasterxml/jackson/core/io/schubfach/FloatToDecimal.class
com/fasterxml/jackson/core/io/CharTypes.class
com/fasterxml/jackson/core/JsonProcessingException.class
com/fasterxml/jackson/core/sym/Name1.class
com/fasterxml/jackson/core/sym/Name2.class
com/fasterxml/jackson/core/TokenStreamFactory.class
com/fasterxml/jackson/core/json/JsonWriteFeature.class
com/fasterxml/jackson/core/sym/Name.class
com/fasterxml/jackson/core/util/DefaultPrettyPrinter.class
com/fasterxml/jackson/core/util/RequestPayload.class
com/fasterxml/jackson/core/util/JsonGeneratorDecorator.class
com/fasterxml/jackson/core/io/BigDecimalParser.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/package-info.class
com/fasterxml/jackson/core/util/RecyclerPool.class
com/fasterxml/jackson/core/sym/Name3.class
com/fasterxml/jackson/core/ErrorReportConfiguration.class
com/fasterxml/jackson/core/format/DataFormatDetector.class
com/fasterxml/jackson/core/io/UTF8Writer.class
com/fasterxml/jackson/core/StreamReadConstraints.class
com/fasterxml/jackson/core/filter/JsonPointerBasedFilter.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/FastDoubleSwar.class
com/fasterxml/jackson/core/io/JsonEOFException.class
com/fasterxml/jackson/core/base/ParserMinimalBase.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/ParseDigitsTaskByteArray.class
com/fasterxml/jackson/core/JsonLocation.class
com/fasterxml/jackson/core/async/package-info.class
com/fasterxml/jackson/core/type/WritableTypeId.class
com/fasterxml/jackson/core/format/MatchStrength.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractNumberParser.class
com/fasterxml/jackson/core/util/InternCache.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/ParseDigitsTaskCharSequence.class
com/fasterxml/jackson/core/util/JsonParserSequence.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractJavaFloatingPointBitsFromCharSequence.class
com/fasterxml/jackson/core/exc/package-info.class
com/fasterxml/jackson/core/io/JsonStringEncoder.class
com/fasterxml/jackson/core/io/SerializedString.class
com/fasterxml/jackson/core/Version.class
com/fasterxml/jackson/core/io/InputDecorator.class
com/fasterxml/jackson/core/FormatSchema.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractBigIntegerParser.class
com/fasterxml/jackson/core/util/ReadConstrainedTextBuffer.class
com/fasterxml/jackson/core/JsonFactory.class
com/fasterxml/jackson/core/filter/TokenFilterContext.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/FastFloatMath.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaFloatBitsFromCharSequence.class
com/fasterxml/jackson/core/io/NumberInput.class
com/fasterxml/jackson/core/util/BufferRecycler.class
com/fasterxml/jackson/core/FormatFeature.class
com/fasterxml/jackson/core/TSFBuilder.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractJavaFloatingPointBitsFromByteArray.class
com/fasterxml/jackson/core/ObjectCodec.class
com/fasterxml/jackson/core/JsonStreamContext.class
com/fasterxml/jackson/core/io/OutputDecorator.class
com/fasterxml/jackson/core/util/TextBuffer.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaFloatBitsFromByteArray.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaFloatParser.class
com/fasterxml/jackson/core/JsonpCharacterEscapes.class
com/fasterxml/jackson/core/util/JsonGeneratorDelegate.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractBigDecimalParser.class
com/fasterxml/jackson/core/io/DataOutputAsStream.class
com/fasterxml/jackson/core/Base64Variant.class
com/fasterxml/jackson/core/async/ByteArrayFeeder.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/AbstractFloatValueParser.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaDoubleBitsFromCharArray.class
com/fasterxml/jackson/core/json/UTF8JsonGenerator.class
com/fasterxml/jackson/core/sym/CharsToNameCanonicalizer.class
com/fasterxml/jackson/core/util/Instantiatable.class
com/fasterxml/jackson/core/StreamWriteConstraints.class
com/fasterxml/jackson/core/util/JacksonFeatureSet.class
com/facebook/buck/testrunner/JUnitTpxStandardOutputListener.class
com/fasterxml/jackson/core/PrettyPrinter.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/BigSignificand.class
com/fasterxml/jackson/core/json/UTF8StreamJsonParser.class
com/fasterxml/jackson/core/io/SegmentedStringWriter.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/ParseDigitsTaskCharArray.class
com/fasterxml/jackson/core/filter/FilteringParserDelegate.class
com/fasterxml/jackson/core/StreamReadFeature.class
com/fasterxml/jackson/core/util/JacksonFeature.class
com/fasterxml/jackson/core/base/package-info.class
com/fasterxml/jackson/core/format/DataFormatMatcher.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaFloatBitsFromCharArray.class
com/fasterxml/jackson/core/sym/ByteQuadsCanonicalizer.class
com/fasterxml/jackson/core/JsonGenerator.class
com/fasterxml/jackson/core/json/PackageVersion.class
com/fasterxml/jackson/core/StreamWriteFeature.class
com/fasterxml/jackson/core/io/UTF32Reader.class
com/fasterxml/jackson/core/util/JsonRecyclerPools.class
com/fasterxml/jackson/core/async/NonBlockingInputFeeder.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaDoubleBitsFromByteArray.class
com/fasterxml/jackson/core/JsonParseException.class
com/fasterxml/jackson/core/json/JsonParserBase.class
com/fasterxml/jackson/core/SerializableString.class
com/fasterxml/jackson/core/sym/NameN.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/JavaBigIntegerFromCharSequence.class
com/fasterxml/jackson/core/JacksonException.class
com/fasterxml/jackson/core/util/DefaultIndenter.class
com/fasterxml/jackson/core/json/async/NonBlockingJsonParser.class
com/fasterxml/jackson/core/internal/shaded/fdp/v2_18_0/FftMultiplier.class
com/fasterxml/jackson/core/json/ReaderBasedJsonParser.class
com/fasterxml/jackson/core/io/CharacterEscapes.class
com/fasterxml/jackson/core/json/UTF8DataInputJsonParser.class
com/fasterxml/jackson/core/util/package-info.class
"""


class TestAppend(unittest.TestCase):
    def test_allowed_jar_entries(self):
        containing_classes = []
        with pkg_resources.resource_stream(__name__, "testrunner-bin-fixed.jar") as r:
            with ZipFile(r) as zip_file:
                for entry in zip_file.namelist():
                    if not entry.endswith("/"):
                        # Strip inner class names and only consider the containing class.
                        containing_class = re.sub(r"\$[^./]*\.class", ".class", entry)
                        containing_classes.append(containing_class)

        not_containing_classes = set(containing_classes) - set(ALLOWED_ENTRIES.split())

        self.assertTrue(
            len(not_containing_classes) == 0,
            "Found unexpected entry in testrunner jar:\n\n%s"
            % "\n".join(not_containing_classes),
        )

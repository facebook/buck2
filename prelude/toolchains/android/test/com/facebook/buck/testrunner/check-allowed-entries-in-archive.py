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

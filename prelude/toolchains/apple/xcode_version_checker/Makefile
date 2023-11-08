CFLAGS = -x objective-c -lobjc -fobjc-arc -fobjc-weak -O3 -Wall

.PHONY: all clean test

xcode_version_checker: xcode_version_checker.x86_64 xcode_version_checker.arm64
	lipo -create -output xcode_version_checker xcode_version_checker.x86_64 xcode_version_checker.arm64
xcode_version_checker.x86_64: src/xcode_version_checker.m src/xcode_version_checks.m
	$(CC) $(CFLAGS) src/xcode_version_checker.m src/xcode_version_checks.m -o xcode_version_checker.x86_64 -target x86_64-apple-macos
xcode_version_checker.arm64: src/xcode_version_checker.m src/xcode_version_checks.m
	$(CC) $(CFLAGS) src/xcode_version_checker.m src/xcode_version_checks.m -o xcode_version_checker.arm64 -target arm64-apple-macos

all: xcode_version_checker test

test: xcode_version_tester xcode_exec_tester
	./xcode_version_tester $(abspath test/Xcode_14.2.0_14C18_fb_version.plist)
	./xcode_exec_tester "/usr/bin/true"
	./xcode_exec_tester "/bin/bash" "-c" "! /usr/bin/false"

xcode_version_tester: src/xcode_version_tester.m src/xcode_version_checks.m
	$(CC) $(CFLAGS) src/xcode_version_tester.m src/xcode_version_checks.m -o xcode_version_tester

xcode_exec_tester: src/xcode_exec_tester.m src/xcode_version_checks.m
	$(CC) $(CFLAGS) src/xcode_exec_tester.m src/xcode_version_checks.m -o xcode_exec_tester

clean:
	rm -f xcode_version_checker.x86_64
	rm -f xcode_version_checker.arm64
	rm -f xcode_version_tester
	rm -f xcode_exec_tester

import subprocess
from time import sleep

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env


@buck_test(inplace=True)
@env("BUCK2_TEST_DISABLE_CACHING", "true")
async def test_scrub_archive(buck: Buck) -> None:
    target = "fbsource//fbobjc/buck2/tests/use_system_frameworks:library"
    result1 = await buck.build(target, "--prefer-local", "--show-output")
    output1 = result1.get_build_report().output_for_target(target)
    await buck.kill()
    sleep(2)  # so that timestamp of static lib changes
    result2 = await buck.build(target, "--prefer-local", "--show-output")
    output2 = result2.get_build_report().output_for_target(target)
    ret = subprocess.call(["diff", "-s", output1, output2])
    assert ret == 0

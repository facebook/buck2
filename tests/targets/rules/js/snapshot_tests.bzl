prelude = native

def js_snapshot_test(target, fixture, subtarget = None, file = None):
    fixture_name = fixture.replace(".json", "").replace("/", "-")
    prelude.export_file(
        name = fixture_name,
        src = fixture,
    )

    redacted_target = "{}.redacted".format(fixture_name)
    prelude.genrule(
        name = redacted_target,
        out = "redacted.json",
        bash = 'sed "s/buck-out.*\\/js\\//BUCK_OUT_REDACTED\\/js\\//g" $(location {}{}){} >> $OUT '.format(
            target,
            "[{}]".format(subtarget) if subtarget else "",
            "/{}".format(file) if file else "",
        ),
    )

    prelude.genrule(
        name = "js_snapshot_{}".format(fixture_name),
        out = "out.txt",
        bash = "diff -sb $(location :{}) $(location :{}) > $OUT".format(
            redacted_target,
            fixture_name,
        ),
    )

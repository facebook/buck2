load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

# All modules owned by a library. This will be used by top-level tests to find
# paths that corresponds to the library.
PythonNeededCoverageInfo = provider(fields = [
    "modules",  # {str.type: str.type}
])

PythonNeededCoverageSpec = record(
    label = field("label"),
    ratio = field(float.type),
    specific_module = field([str.type, None], None),
)

PythonNeededCoverage = record(
    # A value from 0.0 to 1.0 indicating the ratio of coveraged code in the
    # associated modules.
    ratio = field(float.type),
    # Modules that need to be covered.
    modules = field([str.type]),
)

def _parse_python_needed_coverage_spec(
        raw_spec: (int.type, "dependency", [str.type, None])) -> PythonNeededCoverageSpec.type:
    ratio_percentage, dep, specific_module = raw_spec
    if ratio_percentage < 0 or ratio_percentage > 100:
        fail("ratio_percentage must be between 0 and 100 (inclusive): {}".format(ratio_percentage))
    return PythonNeededCoverageSpec(
        label = dep.label,
        ratio = ratio_percentage // 100.0,
        specific_module = specific_module,
    )

def parse_python_needed_coverage_specs(
        raw_specs: [(int.type, "dependency", [str.type, None])]) -> [PythonNeededCoverageSpec.type]:
    return [_parse_python_needed_coverage_spec(raw_spec) for raw_spec in raw_specs]

def gather_python_needed_coverage(
        specs: [PythonNeededCoverageSpec.type],
        deps: ["dependency"]) -> [(float.type, [str.type])]:
    """
    Extract needed coverage paths.
    """

    def find_modules_for_spec(spec: PythonNeededCoverageSpec.type) -> [str.type]:
        for dep in deps:
            # Only search needed coverage providers.
            if dep[PythonNeededCoverageInfo] == None:
                continue
            coverage = dep[PythonNeededCoverageInfo]

            # Make sure this dep matches the spec label.
            if spec.label != dep.label:
                continue

            # Extract modules for this dep.
            if spec.specific_module != None:
                module = coverage.modules.get(spec.specific_module)
                if module == None:
                    fail(
                        "module {} specified in needed_coverage not found in target {}"
                            .format(spec.specific_module, spec.label),
                    )
                modules = [module]
            else:
                modules = coverage.modules.values()

            expect(len(modules) > 0, "no modules found for {} ({})", spec.label, coverage)

            return modules

        fail("could not find matching Python library for coverage spec target {}".format(spec.label))

    return [
        (spec.ratio, find_modules_for_spec(spec))
        for spec in specs
    ]

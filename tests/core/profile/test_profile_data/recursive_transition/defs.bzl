# @nolint

def _tr(platform, refs, attrs):
    _ignore = platform
    constraint_value = getattr(refs, attrs.use_constraint)[ConstraintValueInfo]
    return PlatformInfo(
        label = "pppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp",
        configuration = ConfigurationInfo(
            constraints = {
                constraint_value.setting.label: constraint_value,
            },
            values = {},
        ),
    )

tr = transition(
    impl = _tr,
    refs = {
        "a": "//recursive_transition:a",
        "b": "//recursive_transition:b",
        "c": "//recursive_transition:c",
    },
    attrs = [
        "use_constraint",
    ],
)

def _ooo(_ctx):
    return [DefaultInfo()]

ooo = rule(
    impl = _ooo,
    attrs = {
        "use_constraint": attrs.string(),
        "deps": attrs.list(attrs.dep()),
    },
    cfg = tr,
)

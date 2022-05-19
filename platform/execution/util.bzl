def local_execution_incompatible():
    return select({
        "DEFAULT": [],
        "fbcode//buck2/platform/execution:may_run_local": ["ovr_config//:none"],
    })

def remote_execution_incompatible():
    return select({
        "DEFAULT": [],
        "fbcode//buck2/platform/execution:may_run_remote": ["ovr_config//:none"],
    })

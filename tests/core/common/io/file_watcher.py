# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Optional

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.helper.utils import filter_events


class FileWatcherProvider(Enum):
    WATCHMAN = 0
    RUST_NOTIFY = 1
    FS_HASH_CRAWLER = 2
    EDEN_FS = 3


class FileWatcherEventType(Enum):
    CREATE = 0
    MODIFY = 1
    DELETE = 2


class FileWatcherKind(Enum):
    FILE = 0
    DIRECTORY = 1
    SYMLINK = 2


@dataclass
class FileWatcherEvent:
    event: FileWatcherEventType
    kind: FileWatcherKind
    path: str

    def __lt__(self, other: "FileWatcherEvent") -> bool:
        return (
            (self.event.value < other.event.value)
            or (
                (self.event.value == other.event.value)
                and (self.kind.value < other.kind.value)
            )
            or (
                (self.event.value == other.event.value)
                and (self.kind.value == other.kind.value)
                and (self.path < other.path)
            )
        )


#
# Example FileWatcher.stats - see https://fburl.com/code/pphlekfn:
#   "FileWatcher": {
#     "stats": {
#       "fresh_instance": false,
#       "events_total": 2,
#       "events_processed": 1,
#       "branched_from_revision": "e40262cca30528e4bc0b209e4e7d9cf823528359",
#       "branched_from_global_rev": 1018835633,
#       "events": [
#         {
#           "event": 1,
#           "kind": 0,
#           "path": "fbcode//buck2/tests/core/io/test_watchman.py"
#         }
#       ],
#       "incomplete_events_reason": null,
#       "watchman_version": "2024-12-13T03:32:07Z",
#       "fresh_instance_data": null,
#       "branched_from_revision_timestamp": 1734038044
#     }
#  }
#
async def get_file_watcher_events(
    buck: Buck, target_pattern: str = "root//:", rel_cwd: Optional[Path] = None
) -> tuple[bool, list[FileWatcherEvent]]:
    await buck.targets(target_pattern, rel_cwd=rel_cwd)
    filtered_events = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "FileWatcher",
        "stats",
        "events",
        rel_cwd=rel_cwd,
    )

    file_watcher_events: list[FileWatcherEvent] = []
    for events in filtered_events:
        for event in events:
            file_watcher_events.append(
                FileWatcherEvent(
                    FileWatcherEventType(event.get("event")),
                    FileWatcherKind(event.get("kind")),
                    event.get("path"),
                )
            )

    fresh_instance = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "FileWatcher",
        "stats",
        "fresh_instance",
        rel_cwd=rel_cwd,
    )

    return fresh_instance[0], file_watcher_events

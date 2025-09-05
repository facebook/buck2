# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from pathlib import Path
from typing import List, Optional, Set


class Cgroup:
    """
    A Python class for managing cgroup v2 with path-based operations.
    """

    def __init__(self, path: str | Path):
        """
        Initialize a Cgroup instance with the given path.

        Args:
            path: Path to the cgroup directory (e.g., /sys/fs/cgroup/mygroup)

        Raises:
            FileNotFoundError: If the cgroup path doesn't exist
            ValueError: If the path is not a valid cgroup directory
        """
        self.path = Path(path).resolve()

        if not self.path.exists():
            raise FileNotFoundError(f"Cgroup path does not exist: {self.path}")

        if not self.path.is_dir():
            raise ValueError(f"Cgroup path is not a directory: {self.path}")

    def _read_control_file(self, filename: str) -> str:
        """
        Read the contents of a cgroup control file.

        Args:
            filename: Name of the control file to read

        Returns:
            Contents of the file as a string

        Raises:
            FileNotFoundError: If the control file doesn't exist
            PermissionError: If access to the file is denied
        """
        file_path = self.path / filename
        try:
            return file_path.read_text().strip()
        except FileNotFoundError:
            raise FileNotFoundError(f"Control file not found: {file_path}")
        except PermissionError:
            raise PermissionError(f"Permission denied reading: {file_path}")

    @property
    def subtree_control(self) -> Set[str]:
        """
        Get the controllers enabled in cgroup.subtree_control.

        Returns:
            Set of controller names enabled for child cgroups
        """
        try:
            content = self._read_control_file("cgroup.subtree_control")
            if not content:
                return set()
            return set(content.split())
        except FileNotFoundError:
            return set()

    @property
    def controllers(self) -> Set[str]:
        """
        Get the controllers available in cgroup.controllers.

        Returns:
            Set of controller names available in this cgroup
        """
        try:
            content = self._read_control_file("cgroup.controllers")
            if not content:
                return set()
            return set(content.split())
        except FileNotFoundError:
            return set()

    @property
    def cgroup_type(self) -> str:
        """
        Get the cgroup type from cgroup.type.

        Returns:
            The cgroup type (e.g., "domain", "domain threaded", "domain invalid")
        """
        return self._read_control_file("cgroup.type")

    @property
    def procs(self) -> List[int]:
        """
        Get the list of process IDs in this cgroup.

        Returns:
            List of process IDs in this cgroup
        """
        try:
            content = self._read_control_file("cgroup.procs")
            if not content:
                return []
            return [int(pid) for pid in content.split("\n") if pid.strip()]
        except FileNotFoundError:
            return []

    def list_subcgroups(self) -> List["Cgroup"]:
        """
        List all direct subcgroups of this cgroup.

        Returns:
            List of Cgroup instances for each subcgroup
        """
        subcgroups = []
        try:
            for item in self.path.iterdir():
                if item.is_dir() and (item / "cgroup.type").exists():
                    subcgroups.append(Cgroup(item))
        except PermissionError:
            # If we can't read the directory, return empty list
            pass

        return sorted(subcgroups, key=lambda cg: cg.path.name)

    def get_subcgroup(self, name: str) -> Optional["Cgroup"]:
        """
        Get a specific subcgroup by name.

        Args:
            name: Name of the subcgroup to retrieve

        Returns:
            Cgroup instance for the subcgroup if it exists, None otherwise
        """
        subcgroup_path = self.path / name
        try:
            if subcgroup_path.is_dir() and (subcgroup_path / "cgroup.type").exists():
                return Cgroup(subcgroup_path)
        except (PermissionError, FileNotFoundError):
            # If we can't access the subcgroup, return None
            pass
        return None

    def get_memory_current(self) -> Optional[int]:
        """
        Get current memory usage in bytes.

        Returns:
            Current memory usage in bytes, or None if memory controller is not available
        """
        try:
            return int(self._read_control_file("memory.current"))
        except FileNotFoundError:
            return None

    @property
    def memory_peak(self) -> Optional[int]:
        """
        Get peak memory usage in bytes.

        Returns:
            Peak memory usage in bytes, or None if memory controller is not available
        """
        try:
            return int(self._read_control_file("memory.peak"))
        except FileNotFoundError:
            return None

    @property
    def is_frozen(self) -> bool:
        """
        Check if the cgroup is frozen.

        Returns:
            True if the cgroup is frozen, False otherwise
        """
        try:
            return self._read_control_file("cgroup.freeze") == "1"
        except FileNotFoundError:
            return False

    def __str__(self) -> str:
        """String representation of the cgroup."""
        return f"Cgroup({self.path})"

    def __repr__(self) -> str:
        """Detailed string representation of the cgroup."""
        return f"Cgroup(path='{self.path}', type='{self.cgroup_type}', controllers={self.controllers})"

    def __eq__(self, other) -> bool:
        """Check equality based on cgroup path."""
        if not isinstance(other, Cgroup):
            return False
        return self.path == other.path

    def __hash__(self) -> int:
        """Hash based on cgroup path."""
        return hash(self.path)

    @classmethod
    def from_pid(cls, pid: int) -> "Cgroup":
        """
        Create a Cgroup instance from a process ID.

        Args:
            pid: Process ID to get the cgroup for

        Returns:
            Cgroup instance for the process's cgroup

        Raises:
            FileNotFoundError: If the process doesn't exist
            ValueError: If the cgroup path is invalid
        """
        try:
            with open(f"/proc/{pid}/cgroup") as f:
                # e.g. 0::/user.slice/user-xxx.slice/user@xxx.service/xxx/yyy.scope
                content = f.read().strip()
                if not content:
                    raise ValueError(f"Empty cgroup file for PID {pid}")

                # Parse the cgroup v2 format: 0::/path
                lines = content.split("\n")
                for line in lines:
                    if line.startswith("0::"):
                        cgroup_path = line.split("::", 1)[1]
                        full_path = Path("/sys/fs/cgroup") / cgroup_path.lstrip("/")
                        return cls(full_path)

                raise ValueError(f"No cgroup v2 entry found for PID {pid}")
        except FileNotFoundError:
            raise FileNotFoundError(f"Process {pid} does not exist")

    @classmethod
    def root(cls) -> "Cgroup":
        """
        Get the root cgroup.

        Returns:
            Cgroup instance for the root cgroup
        """
        return cls("/sys/fs/cgroup")

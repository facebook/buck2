package main

import "errors"
import "fmt"
import "os"
import "os/exec"
import "path/filepath"

func bail(msg string) {
	fmt.Fprintln(os.Stderr, msg)
	os.Exit(1)
}

func main() {
	// Get our directory
	cur, err := os.Executable()
	if err != nil {
		bail(fmt.Sprintf("failed to locate wrapper binary path: %v", err))
	}
	exePath, err := filepath.EvalSymlinks(cur)
	if err != nil {
		bail(fmt.Sprintf("failed to resolve symlinks for wrapper binary path: %v", err))
	}
	dir := filepath.Dir(exePath)

	// Look up the wrapped binary relative to ourself
	path := filepath.Join(dir, "../__hello_world__/hello_world")

	// Execute the wrapped binary
	cmd := exec.Command(path)
	cmd.Env = os.Environ()
	cmd.Stderr = os.Stderr
	cmd.Stdout = os.Stdout
	cmd.Stdin = os.Stdin
	if err := cmd.Start(); err != nil {
		bail(fmt.Sprintf("failed to start child program: %v", err))
	}

	if err := cmd.Wait(); err != nil {
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			os.Exit(exitErr.ExitCode())
		}
	}
}

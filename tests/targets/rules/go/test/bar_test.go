//go:build disabled
// +build disabled

package main

import (
	"testing"
)

func TestBar(t *testing.T) {
	t.Fail()
}

package lib

import (
	"testing"
)

func TestFoo(t *testing.T) {
	foo := Foo()
	if foo != "hello world" {
		t.Fatalf("%s = does not equal 'hello world'", foo)
	}
}

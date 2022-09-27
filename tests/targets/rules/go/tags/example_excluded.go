//go:build !(tag1 && tag2)

package main

// HelloWorld returns bad string
func HelloWorld() string {
	return "bad bad bad"
}

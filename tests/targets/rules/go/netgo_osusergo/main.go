package main

import (
	"net"
	"os/user"
)

func main() {
	net.JoinHostPort("localhost", "80")
	user.Current()
}

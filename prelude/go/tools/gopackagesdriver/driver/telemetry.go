/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package driver

import (
	"context"
	"time"

	"golang.org/x/tools/go/packages"
)

type RequestFinishedEvent struct {
	Duration time.Duration
	BuckRoot string
	Request  *packages.DriverRequest
	Patterns []string
	// Note: Response or Error can be nil
	Response *packages.DriverResponse
	Error    error
}

type PanicEvent struct {
	PanicValue interface{} // from recover
	Stack      []byte      // from debug.Stack
}

// Telemetry abstracts loging of the driver events. It is recemmended to be:
// - qucik to avoid slowing down the tool
// - thread safe as we can run multiple gorutines
// - flexible to skip unknown events
type Telemetry interface {
	LogEvent(ctx context.Context, event any)
}

// NoopTelemetry is a noop implementation of Telemetry
type NoopTelemetry struct{}

func (*NoopTelemetry) LogEvent(ctx context.Context, event any) {
	// noop
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#include <Python.h>
#include <iostream>

static PyObject* method_func(PyObject* self, PyObject* args) {
  std::cout << "hello from cpython extension from cpp toolchain" << std::endl;
  return self;
}

static PyMethodDef methods[] = {
    {"hello_world", method_func, METH_VARARGS, "Print hello world from cpp"},
    {NULL, NULL, 0, NULL}};

static struct PyModuleDef extension = {
    PyModuleDef_HEAD_INIT,
    "cpprint",
    "A module for printing from cpp",
    -1,
    methods};

PyMODINIT_FUNC PyInit_cpprint(void) {
  return PyModule_Create(&extension);
}

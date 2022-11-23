/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

int main(int argc, char* argv[]) {
  wchar_t* program = Py_DecodeLocale(argv[0], NULL);
  if (program == NULL) {
    fprintf(stderr, "Fatal error: cannot decode argv[0]\n");
    exit(1);
  }

  /* Add a built-in module, before Py_Initialize */
  if (PyImport_AppendInittab("cppring", PyInit_cpprint) == -1) {
    fprintf(stderr, "Error: could not extend in-built modules table\n");
    exit(1);
  }

  /* Pass argv[0] to the Python interpreter */
  Py_SetProgramName(program);

  /* Initialize the Python interpreter.  Required.
     If this step fails, it will be a fatal error. */
  Py_Initialize();

  return 0;
}

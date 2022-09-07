#include <python/Python.h>
#include <stdio.h>

static PyObject* func(PyObject* self, PyObject* args) {
  printf("%s\n", PY_VERSION);
  return NULL;
}

static PyMethodDef ExtMethods[] = {
    {"func", func, METH_VARARGS},
    {NULL, NULL, 0, NULL}};

static struct PyModuleDef extmodule =
    {PyModuleDef_HEAD_INIT, "ext", "mdoule", -1, ExtMethods};

PyMODINIT_FUNC PyInit_ext(void) {
  return PyModule_Create(&extmodule);
}

#include <Python.h>

extern "C" int lib_func();

static PyObject* method_func(PyObject* self, PyObject* args) {
  return PyLong_FromLong(lib_func());
}

static PyMethodDef methods[] = {
    {"func",
     method_func,
     METH_VARARGS,
     "Python interface for fputs C library function"},
    {NULL, NULL, 0, NULL}};

static struct PyModuleDef extension =
    {PyModuleDef_HEAD_INIT, "extension", "Test extension", -1, methods};

PyMODINIT_FUNC PyInit_extension(void) {
  return PyModule_Create(&extension);
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include "Python.h"

typedef PyObject* (*pyinitfunc)();

typedef struct {
} StaticExtensionFinderObject;

// This map is generated at build time by generate_static_extension_info.py
// and should not be modified at runtime.
extern std::unordered_map<std::string_view, pyinitfunc> _static_extension_info;

namespace {

static PyObject* _handle_single_phase_initialization(
    PyObject* mod,
    PyObject* name,
    PyObject* spec,
    pyinitfunc initfunc,
    PyObject* modules) {
  /* Remember pointer to module init function. */
  PyModuleDef* def = PyModule_GetDef(mod);
  if (def == nullptr) {
    Py_DECREF(name);
    return nullptr;
  }
  PyObject* path = PyObject_GetAttrString(spec, "origin");
  if (PyModule_AddObject(mod, "__file__", path) < 0) {
    PyErr_Clear();
  } else {
    Py_INCREF(path);
  }
  def->m_base.m_init = initfunc;
  if (modules == nullptr) {
    modules = PyImport_GetModuleDict();
  }

  // TODO private api usage
#if PY_VERSION_HEX >= 0x030D0000
  PyThreadState* tstate = PyThreadState_GET();
  int result = _Ci_PyImport_FinishSinglePhaseExtension(
      (int*)tstate, mod, def, name, modules);
#else
  int result = _PyImport_FixupExtensionObject(mod, name, name, modules);
#endif
  Py_DECREF(path);
  Py_DECREF(name);
  if (result < 0) {
    return nullptr;
  }
  return mod;
}
static PyObject* _create_module(PyObject* self, PyObject* spec) {
  PyObject* name;
  PyObject* mod;

  name = PyObject_GetAttrString(spec, "name");
  if (name == nullptr) {
    return nullptr;
  }
  // Attempt to import the module normally (builtins, etc)
  mod = PyImport_GetModule(name);
  if (mod || PyErr_Occurred()) {
    Py_DECREF(name);
    Py_XINCREF(mod);
    return mod;
  }

  // If the module is not found, try to find it in the static extension map
  // its generated at build time by generate_static_extension_info.py
  const std::string namestr = PyUnicode_AsUTF8(name);
  if (namestr.empty()) {
    Py_DECREF(name);
    return nullptr;
  }

  pyinitfunc initfunc = nullptr;
  if (auto result = _static_extension_info.find(namestr);
      result != _static_extension_info.end())
    initfunc = result->second;

  if (initfunc == nullptr) {
    PyErr_SetString(
        PyExc_ImportError, "Module unknown to static extension finder");
    return nullptr;
  }

  PyObject* modules = nullptr;
#if PY_VERSION_HEX >= 0x030E0000
  throw std::runtime_error(
      "Native python does not support Python 3.14 and later.");
#elif PY_VERSION_HEX >= 0x030D0000
  // Python 3.13 has a new C-API for calling module init functions
  mod = _Ci_PyImport_CallInitFuncWithContext(namestr.c_str(), initfunc);
#elif PY_VERSION_HEX >= 0x030C0000 && !defined(OSS_PYTHON)
  // Use our custom Python 3.12 C-API to call the statically linked module init
  // function
  mod = _Ci_PyImport_CallInitFuncWithContext(namestr.c_str(), initfunc);
#elif PY_VERSION_HEX >= 0x030A0000
  // In Python 3.10 we need to handle package context swapping
  // ourselves
  const char* oldcontext = _Py_PackageContext;
  _Py_PackageContext = namestr.c_str();
  if (_Py_PackageContext == nullptr) {
    _Py_PackageContext = oldcontext;
    Py_DECREF(name);
    return nullptr;
  }
  mod = initfunc();
  _Py_PackageContext = oldcontext;
#else
  // _Py_PackageContext undefined in 3.9 and earlier
  throw std::runtime_error(
      "Native python does not support Python 3.9 and earlier.");
#endif
  if (mod == nullptr) {
    Py_DECREF(name);
    return nullptr;
  }
  // If the result is a PyModuleDef, then this is multi-phase initialization
  // Return the PyModule so module_exec can be called on it later
  if (PyObject_TypeCheck(mod, &PyModuleDef_Type)) {
    Py_DECREF(name);
    return PyModule_FromDefAndSpec((PyModuleDef*)mod, spec);
  }
  // At this point we know this is single-phase initialization
  return _handle_single_phase_initialization(
      mod, name, spec, initfunc, modules);
}

static PyObject* _exec_module(PyObject* self, PyObject* module) {
  PyModuleDef* def;

  // TODO errors
  if (!PyModule_Check(module)) {
    // TODO
    Py_RETURN_NONE;
  }

  def = PyModule_GetDef(module);
  if (def == nullptr) {
    // TODO
    Py_RETURN_NONE;
  }

  // TODO check this result
  PyModule_ExecDef(module, def);

  Py_RETURN_NONE;
}

static PyObject* _get_source(PyObject* self, PyObject* /* module_name */) {
  // TODO properly implement the function
  Py_RETURN_NONE;
}

PyDoc_STRVAR(
    StaticExtensionLoader_doc,
    "static_extension_loader(name: str)\n\
\n\
a loader for extensions linked statically into the binary");

static PyMethodDef StaticExtensionLoaderType_methods[] = {
    {"create_module",
     (PyCFunction)(void (*)(void))_create_module,
     METH_STATIC | METH_O,
     nullptr},
    {"exec_module",
     (PyCFunction)(void (*)(void))_exec_module,
     METH_STATIC | METH_O,
     nullptr},
    {"get_source",
     (PyCFunction)(void (*)(void))_get_source,
     METH_STATIC | METH_O,
     nullptr},
    {}};

static PyType_Slot StaticExtensionLoader_slots[] = {
    {Py_tp_doc, (void*)StaticExtensionLoader_doc},
    {Py_tp_methods, StaticExtensionLoaderType_methods},
    {}};

static PyType_Spec StaticExtensionLoader_spec = {
    "static_extension_utils.StaticExtensionLoader",
    0,
    0,
    Py_TPFLAGS_DEFAULT,
    StaticExtensionLoader_slots};

static int _static_extension_utils_exec(PyObject* m) {
  PyObject* loader_type = PyType_FromSpec(&StaticExtensionLoader_spec);
  if (loader_type == nullptr) {
    return -1;
  }
  int result = PyModule_AddObject(m, "StaticExtensionLoader", loader_type);
  if (result == -1) {
    Py_DECREF(loader_type);
    return -1;
  }
  return 0;
}

PyDoc_STRVAR(
    _check_module_doc,
    "Check if a module is contained in the C Extension map \n");

static PyObject* _check_module(PyObject* self, PyObject* fullname) {
  if (!PyUnicode_Check(fullname)) {
    PyErr_SetString(PyExc_TypeError, "Expected a unicode object");
    return nullptr;
  }
  const std::string modname = PyUnicode_AsUTF8(fullname);
  if (_static_extension_info.find(modname) != _static_extension_info.end()) {
    Py_INCREF(Py_True);
    return Py_True;
  }
  Py_INCREF(Py_False);
  return Py_False;
}

static PyModuleDef_Slot _static_extension_utils_slots[] = {
    {Py_mod_exec, (void*)_static_extension_utils_exec},
#ifdef Py_GIL_DISABLED
    {Py_mod_gil, Py_MOD_GIL_NOT_USED},
#endif
    {}};

static PyMethodDef _static_extension_utils_methods[] = {
    {"_check_module", _check_module, METH_O, _check_module_doc},
    {}};

PyDoc_STRVAR(
    module_doc,
    "Utils for importing modules statically linked into the python binary \n");

static struct PyModuleDef _static_extension_utils_def = {
    PyModuleDef_HEAD_INIT,
    "_static_extension_utils_def",
    module_doc,
    0,
    _static_extension_utils_methods,
    _static_extension_utils_slots,
    nullptr,
    nullptr,
    nullptr};

PyMODINIT_FUNC PyInit__static_extension_utils(void) {
  return PyModuleDef_Init(&_static_extension_utils_def);
}
} // namespace

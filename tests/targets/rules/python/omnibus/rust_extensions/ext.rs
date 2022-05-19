use cpython::{py_fn, py_module_initializer, PyObject, PyResult, Python};

fn main(py: Python) -> PyResult<PyObject> {
    println!("the_test_string");
    Ok(py.None())
}

py_module_initializer!(ext, |py, m| {
    m.add(py, "main", py_fn!(py, main()))?;
    Ok(())
});

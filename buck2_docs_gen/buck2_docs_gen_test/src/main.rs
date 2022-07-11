//! This is a test for the doc gen
//! We just verify that this returns some valid looking docs.
//! We can't compile this with the rest of buck2 as the doc registration will be polluted.

use buck2_docs_gen::Buck2Docs;
use buck2_docs_gen::StarlarkObject;
use buck2_docs_gen::StarlarkObjectDoc;
use starlark::environment::MethodsBuilder;
use starlark::starlark_module;
use starlark::values::docs::DocItem;
use starlark::values::Value;

#[derive(Buck2Docs)]
#[buck2_docs(foo)]
#[allow(unused)]
struct Foo;

#[starlark_module]
fn foo(_builder: &mut MethodsBuilder) {
    /// foo_a docs
    fn foo_a<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        let _ = this;
        Ok(Value::new_none())
    }
}

#[derive(Buck2Docs)]
#[buck2_docs(bar, name = "bar_name_override")]
#[allow(unused)]
struct Bar;

#[starlark_module]
fn bar(builder: &mut MethodsBuilder) {
    /// bar_b docs
    fn bar_b<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        let _ = this;
        Ok(Value::new_none())
    }
}

#[derive(Buck2Docs)]
#[buck2_docs(baz, directory = "namespaced", name = "baz_name_override")]
#[allow(unused)]
struct Baz;

#[starlark_module]
fn baz(builder: &mut MethodsBuilder) {
    /// baz_b docs
    fn baz_b<'v>(this: Value<'v>) -> anyhow::Result<Value<'v>> {
        let _ = this;
        Ok(Value::new_none())
    }
}

fn main() {
    for StarlarkObjectDoc {
        name,
        directory,
        item,
    } in StarlarkObject::all_docs()
    {
        let dir = directory.path().to_string_lossy().to_string();
        let first_member = match item {
            DocItem::Object(o) => o.members.first().map(|(id, _)| id.to_owned()),
            _ => None,
        };
        println!(
            "`{}` {}: {}",
            dir,
            name,
            first_member.unwrap_or_else(|| "MISSING MEMBER".to_owned())
        );
    }
}

use crate::check::{manifest, manifest_self};

#[test]
fn empty() {
  manifest_self("{}");
}

#[test]
fn non_empty() {
  manifest(
    r#"
{
  num: 1,
  bool: true,
  str: "bar",
  "foo quz": null,
}
"#,
    r#"
{
  "num": 1,
  "bool": true,
  "str": "bar",
  "foo quz": null
}
"#,
  );
}

#[test]
#[should_panic = "+ for non-prim"]
fn implicit_plus() {
  manifest(
    r#"
{ a: 1 } { b: 2 }
"#,
    r#"
{ "a": 1, "b": 2 }
"#,
  );
}

// TODO fix
#[test]
#[should_panic]
fn use_fns() {
  manifest(
    r#"
local Person(name='Alice') = {
  name: name,
  welcome: 'Hello ' + name + '!',
};
{
  person1: Person(),
  person2: Person('Bob'),
}
"#,
    r#"
{
  "person1": {
    "name": "Alice",
    "welcome": "Hello Alice!"
  },
  "person2": {
    "name": "Bob",
    "welcome": "Hello Bob!"
  }
}
"#,
  );
}

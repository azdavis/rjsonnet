//! Examples from the [homepage](https://jsonnet.org/).

use crate::check::JsonnetInput;

#[test]
fn t1() {
  JsonnetInput::manifest(
    r#"
// Edit me!
{
  person1: {
    name: "Alice",
    welcome: "Hello " + self.name + "!",
  },
  person2: self.person1 { name: "Bob" },
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
  )
  .check_one();
}

#[test]
fn t2() {
  JsonnetInput::manifest(
    r"
// A function that returns an object.
local Person(name='Alice') = {
  name: name,
  welcome: 'Hello ' + name + '!',
};
{
  person1: Person(),
  person2: Person('Bob'),
}
",
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
  )
  .check_one();
}

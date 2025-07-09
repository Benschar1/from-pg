# from-pg

Derive `FromPg` on a struct to extract it to generate a mapping between a struct and a `tokio-postgres` row.

## Examples
```rust
use from_pg::FromPg;
use tokio_postgres::Row;

#[derive(FromPg)]
pub struct Contact {
    pub id: i64,
    pub name: String,
    pub phone_number: String,
}

let row = client.query_one("SELECT id, name, phone_number FROM contacts", &[]).unwrap();
let contact = Contact::from_pg_default(&row);
```

If you want to do a conversion after extracting a field using `FromSql`, you can use the `#[frompg(from = T, func = f)]` attribute. For instance, the following code first extracts `permission` using the `FromSql` implementation for `&str`, then runs it through the `text_to_permission` function:

```rust
#[derive(FromPg)]
pub struct User {
    pub id: i64,
    pub name: String,
    #[frompg(from = &str, func = text_to_permission)]
    pub permission: Permission,
}

pub fn text_to_permission(s: &str) -> Result<Permission, Box<dyn std::error::Error + Send + Sync>> {
    match s {
        "read" => Ok(Permission::Read),
        "write" => Ok(Permission::Write),
        "admin" => Ok(Permission::Admin),
        _ => Err(Box::<dyn std::error::Error + Send + Sync>::from(
            "invalid permission",
        )),
    }
}
```

Taking the `User` example above, the macro generates two new types: `UserConfig` and `UserError`.

```rust
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UserConfig {
    id: String,
    name: String,
    permission: String,
}

#[derive(Debug, Default)]
pub struct UserError {
    id: Option<
        Box<dyn std::error::Error + Sync + Send>,
    >,
    name: Option<
        Box<dyn std::error::Error + Sync + Send>,
    >,
    permission: Option<
        Box<dyn std::error::Error + Sync + Send>,
    >,
}
```

`UserConfig` lets you specify which column name to look for on a per-field basis. For instance, for the query `SELECT user_id, username, perm FROM ...`, you can configure it as follows:

```rust
let _ = User::from_pg(
    row,
    &UserConfig {
        id: "id".to_string(),
        name: "username".to_string(),
        permission: "perm".to_string(),
    },
)
```

The `Config` structs also have helper methods:

```rust
impl ::std::default::Default for UserConfig {
    fn default() -> Self {
        Self {
            id: String::from("id"),
            name: String::from("name"),
            permission: String::from("permission"),
        }
    }
}

impl UserConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn id(&self) -> &str {
        self.id.as_ref()
    }

    pub fn set_id(self, name: String) -> Self {
        Self { id: name, ..self }
    }

    pub fn prefix_id(self, prefix: String) -> Self {
        let mut name = prefix.clone();
        name.push_str("id");
        Self { id: name, ..self }
    }

    // and so on for each field
}
```

The `from_pg_default` method just uses the default `Config` object.

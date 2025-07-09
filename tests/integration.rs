use from_pg::FromPg;
use tokio_postgres::Row;

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

pub enum Permission {
    Read,
    Write,
    Admin,
}

#[derive(FromPg)]
pub struct EmptyStruct {}

#[allow(dead_code)]
fn test_func(row: &Row) {
    let _ = User::from_pg_default(row);
    let _ = EmptyStruct::from_pg_default(row);

    let _ = User::from_pg(
        row,
        &UserConfig::new()
            .prefix_name("user".into())
            .set_permission("perm".into()),
    );
}

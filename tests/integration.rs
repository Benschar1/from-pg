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
pub struct Address {
    pub street: String,
    pub city: String,
    pub zip: String,
}

#[derive(FromPg)]
pub struct ContactInfo {
    pub email: String,
    pub phone: String,
}

#[derive(FromPg)]
pub struct UserWithAddress {
    pub id: i64,
    pub name: String,
    #[frompg(derive = Address)]
    pub address: Address,
}

#[derive(FromPg)]
pub struct UserWithContact {
    pub id: i64,
    pub name: String,
    #[frompg(derive = ContactInfo)]
    pub contact: ContactInfo,
}

#[derive(FromPg)]
pub struct EmptyStruct {}

#[derive(FromPg)]
pub struct UserWithOptionalAddress {
    pub id: i64,
    pub name: String,
    #[frompg(derive = Address)]
    pub address: Option<Address>,
}

#[allow(dead_code)]
fn test_func(row: &Row) {
    let _ = User::from_pg_default(row);
    let _ = EmptyStruct::from_pg_default(row);
    let _ = UserWithAddress::from_pg_default(row);
    let _ = UserWithContact::from_pg_default(row);
    let _ = UserWithOptionalAddress::from_pg_default(row);

    let _ = User::from_pg(
        row,
        &UserConfig::new()
            .prefix_name("user".into())
            .set_permission("perm".into()),
    );

    let _ = UserWithAddress::from_pg(
        row,
        &UserWithAddressConfig::new()
            .prefix_name("user".into())
            .set_address(AddressConfig::new()),
    );

    let _ = UserWithContact::from_pg(
        row,
        &UserWithContactConfig::new()
            .prefix_name("user".into())
            .set_contact(ContactInfoConfig::new()),
    );

    let _ = UserWithOptionalAddress::from_pg(
        row,
        &UserWithOptionalAddressConfig::new()
            .prefix_name("user".into())
            .set_address(AddressConfig::new()),
    );
}

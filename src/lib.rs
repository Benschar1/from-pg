pub use from_pg_derive::FromPg;
pub use tokio_postgres::Row;

pub trait FromPg: Sized {
    type Config: Default;
    type Error: Into<Box<dyn std::error::Error + Send + Sync>>;

    fn from_pg(row: &Row, conf: &Self::Config) -> Result<Self, Self::Error>;

    fn from_pg_default(row: &Row) -> Result<Self, Self::Error> {
        Self::from_pg(row, &Self::Config::default())
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         let result = add(2, 2);
//         assert_eq!(result, 4);
//     }
// }

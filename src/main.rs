use serde_dhall::*;

fn main() {
    println!("{:?}", from_str::<Value>("let x = 0 in { x, a = 0 }"));
    println!("{:?}", from_str::<Value>("let x = 0 in { x , a = 0 }"));
}

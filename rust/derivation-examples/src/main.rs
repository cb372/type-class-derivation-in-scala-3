extern crate derivation_examples;
use derivation_examples::ImplementHeight;

#[derive(Debug, PartialEq, Eq, ImplementHeight)]
struct Person {
  name: String,
  height: i32
}

trait Height {
    fn height_in_metres(&self) -> f32;
}

fn main() {
    let chris = Person { name: "Chris".to_string(), height: 180 };
    let yoshiko = Person { name: "Yoshiko".to_string(), height: 148 };

    // Making use of the derived stock traits: Debug (for {:?}) and Eq
    let answer =
        if chris == yoshiko {
            "yes"
        } else {
            "no"
        };
    println!("Are {:?} and {:?} the same person? {}", chris, yoshiko, answer);

    // Making use of the macro-derived Height impl
    println!("Chris's height is {}m", chris.height_in_metres());
}

//! Lab (answers)
//! References
//!
//! Fix all the compile errors below by following the hints provided
//!

fn main() {
    struct ReferencesStruct {
        count: i8,
        value: f64,
    }

    // Task 1 Goals
    //   a) Create a shared reference to 'angle'
    //   b) Print the value for the shared reference
    //   c) Double the value for each field
    //   d) Print the value for shared reference
    // Hint:
    //   Cannot modify a shared reference directly
    println!("Task 1 - Observer");
    let mut angle = ReferencesStruct {
        count: 12,
        value: 34.5,
    };
    let ref_angle = &angle;
    println!("Before: {} {}", ref_angle.count, ref_angle.value);
    angle = ReferencesStruct {
        count: angle.count * 2,
        value: angle.value * 2.0,
    };
    let ref_angle = &angle;
    println!("After: {} {}", ref_angle.count, ref_angle.value);

    // Task 2 Goals
    //   a) Create a reference object to point to multiple objects
    //   b) Use the reference object print the values of both objects
    // Hint: A shared reference can be mutable!
    println!("Task 2 - Rebinder");
    let bricks = ReferencesStruct {
        count: 6,
        value: 78.9,
    };
    let buildings = ReferencesStruct {
        count: 1,
        value: 2.3,
    };
    let mut ref_both = &bricks;
    println!("Bricks: {} {}", ref_both.count, ref_both.value);
    ref_both = &buildings;
    println!("Buildings: {} {}", ref_both.count, ref_both.value);

    // Task 3 Goals
    //   a) Create a reference object that allows modification of the contents
    //   b) Update the the original object via the reference object
    //   c) Print the before and after value for the original object
    // Hint:
    //   The original object must be modifiable
    //   The reference object must allow for modification of the contents
    println!("Task 3 - Modifier");
    let mut cars = ReferencesStruct {
        count: 67,
        value: 8.9,
    };
    println!("Before: {} {}", cars.count, cars.value);
    let ref_cars = &mut cars;
    ref_cars.count = ref_cars.count / 2;
    ref_cars.value = ref_cars.value / 2.0;
    println!("After: {} {}", cars.count, cars.value);

    // Task 4 Goals
    //   a) Create a single reference object to print and modify multiple objects
    //   b) Print the before and after values for each object
    // Hint: The mutable reference needs to be mutable
    println!("Task 4 - Free Agent");
    let mut dog = ReferencesStruct {
        count: 1,
        value: 2.3,
    };
    let mut duck = ReferencesStruct {
        count: 4,
        value: 5.6,
    };
    let mut ref_both: &mut ReferencesStruct = &mut dog;
    println!("Before dog: {} {}", ref_both.count, ref_both.value);
    *ref_both = ReferencesStruct {
        count: 0,
        value: 0.0,
    };
    println!("After dog: {} {}", ref_both.count, ref_both.value);

    ref_both = &mut duck;
    println!("Before duck: {} {}", ref_both.count, ref_both.value);
    *ref_both = ReferencesStruct {
        count: 0,
        value: 0.0,
    };
    println!("After duck: {} {}", ref_both.count, ref_both.value);

    // Task 5 Goals
    //   Modify the existing code to print correctly
    // Hint: Need to make the referenced object last longer
    println!("Task 5 - Dangling Reference");
    let jack = String::from("Jack");
    let rose = {
        &jack // 'rose' is a reference to 'jack'
    };
    println!("Jack screams '{rose}'");

    // Task 6 Goals
    //   Use a single reference object to print all different length slices of 'floats'
    // Hint:
    //   Reference object refers to a slice of the object, not length
    //   Use "{:?}" is used to print the contents of the array (Debug)
    println!("Task 6 - Slices");
    let floats: [f64; 5] = [1.1, 22.22, 333.333, 4444.4444, 55555.555555];
    let mut ref_floats: &[f64] = &floats[0..1];
    println!("{:?}", ref_floats);
    ref_floats = &floats[1..3];
    println!("{:?}", ref_floats);
    ref_floats = &floats[2..];
    println!("{:?}", ref_floats);
}

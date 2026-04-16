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

    // TASK 1 - Get the "println" to print double the field contents
    // Hint: Cannot modify a shared reference
    println!("Task 1 - Observer");
    let angle = ReferencesStruct {
        count: 12,
        value: 34.5,
    };
    let ref_angle = &angle;
    println!("{} {}", ref_angle.count * 2, ref_angle.value * 2.0);

    // TASK 2 - Use one reference object to print the contents of both objects
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
    println!("{} {}", ref_both.count, ref_both.value);
    ref_both = &buildings;
    println!("{} {}", ref_both.count, ref_both.value);

    // TASK 3 - Divide the pointed-to data contents by 2
    // Hint: Need a mutable reference
    println!("Task 3 - Modifier");
    let mut cars = ReferencesStruct {
        count: 67,
        value: 8.9,
    };
    let ref_cars = &mut cars;
    ref_cars.count = ref_cars.count / 2;
    ref_cars.value = ref_cars.value / 2.0;
    println!("{} {}", ref_cars.count, ref_cars.value);

    // TASK 4 - Print the two objects and clear the contents all using one reference
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
    let mut ref_both = &mut dog;
    println!("{} {}", ref_both.count, ref_both.value);
    *ref_both = ReferencesStruct {
        count: 0,
        value: 0.0,
    };
    println!("{} {}", ref_both.count, ref_both.value);
    ref_both = &mut duck;
    println!("{} {}", ref_both.count, ref_both.value);
    *ref_both = ReferencesStruct {
        count: 0,
        value: 0.0,
    };
    println!("{} {}", ref_both.count, ref_both.value);

    // TASK 5 - Fix the dangling reference
    // Hint: Need to make the referenced object last longer
    let first = ReferencesStruct {
        count: 9,
        value: 8.7,
    };
    let second = ReferencesStruct {
        count: first.count + 1,
        value: first.value - 1.1,
    };
    let ref_emu = &second;
    println!("{} {}", ref_emu.count, ref_emu.value);

    // TASK 6 - Use a single reference to print all possible slices of 'floats'
    // Hint: Reference refers to object, not length
    // Note: "{:?}" is used to print the contents of the array
    let floats: [f64; 5] = [1.1, 22.22, 333.333, 4444.4444, 55555.555555];
    let mut ref_floats: &[f64] = &floats[0..1];
    println!("{:?}", ref_floats);
    ref_floats = &floats[1..3];
    println!("{:?}", ref_floats);
    ref_floats = &floats[2..];
    println!("{:?}", ref_floats);
}

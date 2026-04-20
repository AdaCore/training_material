//! Lab (answers)
//! Borrowing
//!
//! Fix all the compile errors below by following the hints provided
//!

fn task_1() {
    println!("Task 1 - Mutable and Immutable Borrows");
    // Task 1 Goals
    //   a) Make multiple borrows of an object
    //   b) Print the items you've borrowed
    //   c) Modify the borrowed object
    //   d) Print the borrowed object and the original object
    // Hint:
    //   Cannot borrow for writing if a reader is in-use
    //   Cannot modify something that is not specified as modifiable

    struct Sensor(i32);
    let object = Sensor(111);

    let reader1 = &object;
    let reader2 = &object;
    let writer = &mut object;
    writer.0 += 1000;
    println!("Reader values: {} and {}", reader1.0, reader2.0);
    println!("Writer value: {}", writer.0)
}

fn task_2() {
    println!("Task 2 - Function Borrows");
    // Task 2 Goals
    //   a) Pass an object to a read-only function
    //   b) Pass object to a function that changes the parameter
    //   d) Print the original object
    // Hint:
    //   Parameters must specify if they can be modified

    struct Sensor(i32);
    fn reader(param: Sensor) {
        println!("Reader param: {}", param.0)
    }
    fn writer(param: Sensor) {
        param.0 += 2000;
        println!("Writer param: {}", param.0);
    }
    let object = Sensor(222);
    reader(object);
    writer(object);
    println!("Original value: {}", object.0);
}

fn task_3() {
    println!("Task 3 - Overlapping Borrows");
    // Task 3 Goals
    //   a) Borrow an object for printing a value
    //   b) Pass object to a function that changes the parameter
    //   d) Print the original object
    // Hint:
    //   Parameters must specify if they can be modified

    struct Sensor(i32);
    fn writer(param: mut Sensor) {
        param.0 += 3000;
        println!("Writer param: {}", param.0);
    }
    let mut object = Sensor(333);
    let reader = &object;
    writer(&object);
    println!("Original value: {}", object.0);
    println!("Reader value: {}", reader.0);
}

fn task_4() {
    println!("Task 4 - Multiple Mutable Borrows");
    // Task 4 Goals
    //    a) 'sync_sensors' should copy contents of one sensor to the other
    //    b) Print the values before and after
    // Hint:
    //    Not actually allowed!
    //    Modify 'sync_sensor' and the caller to get the job done
    struct Sensor(i32);
    fn sync_sensor(sensor1: &mut Sensor, sensor2: &mut Sensor) {
        sensor1.0 = sensor2.0 + 4000;
    }
    let mut object = Sensor(444);
    println!("Original value: {}", object.0);
    sync_sensor (&mut object, &mut object);
    println!("New value: {}", object.0);
}

fn task_5() {
    println!("Task 5 - Method Borrows");
    // Task 5 Goals
    //   a) Pass an object to a read-only method
    //   b) Pass object to a method that changes the parameter
    //   d) Print the original object
    // Hint:
    //   Parameter must indicate that the object is the parameter
    //   Parameters must specify if they can be modified
    //   Function cannot modify object if a reader is in-use
    //   
    struct Sensor(i32);
    impl Sensor {
        fn read(param: &Sensor) -> i32 {
            param.0
        }
        fn calibrate(param : &mut Sensor) {
            param.0 += 5000;
        }
    }
    let mut object = Sensor(555);
    println!("Original value: {}", object.read());
    object.calibrate();
    println!("new value: {}", object.read());  
}

fn task_6() {
    println!("Task 6 - Cell<T>");
    // Task 6 Goals
    //   a) Increment counter every time object is read
    //   b) Reader should not be modifying the contents, just the counter
    //   c) See what happens with multiple reader references!
    // Hint:
    //   Use "Cell" reference type to implement the counter
    //   Do not forget to import the appropriate crate
    struct Sensor {
        data: i32,
        count: u32,
    }
    impl Sensor {
        fn read(&self) -> i32 {
            self.count += 1;
            self.data
        }
    }
    let object = Sensor {
        data: 42,
        count: 0,
    };
    println!(
        "Object: {} read {} times",
        object.read(),
        object.count
    );
    let reader1 = &object;
    let reader2 = &object;
    println!(
        "Reader1: {} read {} times",
        reader1.read(),
        reader1.count
    );

}

fn task_7() {
    println!("Task 7 - RefCell<T>");
    // Task 7 Goals
    //   a) Implement a sensor history
    //   b) Make sure you can read and modify the history data
    // Hint:
    //   Use "RefCell" reference type to implement the counter
    //   Do not forget to import the appropriate crate
    //   RefCell borrow failures are run-time problems, not compile errors

    struct History {
        data: RefCell<Vec<u32>>,
    }
    let values = History {
        data: RefCell::new(vec![1, 2, 3]),
    };

    let immutable = values.data.borrow();
    println!("Immutable values: {:?}", immutable);

    let mut mutable = values.data.borrow_mut();
    println!("Values: {:?}", mutable);
    mutable.push(4);
    println!("Values: {:?}", mutable);
    mutable.push(5);

    let immutable = values.data.borrow();
    println!("Values: {:?}", immutable);
}

fn main() {
    task_1();
    task_2();
    task_3();
    task_4();
    task_5();
    task_6();
    task_7();
}

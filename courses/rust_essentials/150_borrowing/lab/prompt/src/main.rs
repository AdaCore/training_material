//! Lab (prompt)
//! Borrowing
//!
//! Fix all the compile errors below by following the hints provided
//!

struct Sensor(i32);

impl Sensor {
    fn read(&self) -> i32 {
        self.0
    }
    fn calibrate(&mut self) {
        self.0 *= 10;
    }
}

fn read(device: Sensor) {
    println!("Read: {}", device.0);
}

fn calibrate(device: Sensor) {
    device.0 += 10;
}

fn sync_sensors(dest: &mut Sensor, src: &mut Sensor) {
    dest.0 = src.0;
    src.0 = 0;
}

fn main() {

    // TASK 1 - Use functions to print and modify the object
    //          print and modify the value
    // Hint: Cannot borrow something to modify if it's already borrowed
    println!("Task 1");
    let scanner1 = Sensor(101);
    read(scanner1);
    calibrate(scanner1);
    read(scanner1);

    // TASK 2 - Use mutable and immutable local borrowers to
    //          print and modify the value
    // Hint: Cannot borrow something to modify if it's already borrowed
    println!("Task 2");
    let scanner2 = Sensor(42);
    let reader2 = &scanner2;
    let writer2 = &mut scanner2;
    writer2.0 *= 11;
    println!("Reader: {}", reader2.0);
    println!("Writer {}", writer2.0);
    reader2 = &scanner2;
    println!("Reader: {}", reader2.0);

    // TASK 3 - Use both function and local borrowers to
    //          print and modify the value
    // Hint: Combining tasks 1 and 2!
    println!("Task 3");
    let mut scanner3 = Sensor(42);
    let reader3 = &scanner3;
    println!("Reader: {}", reader3.0);
    read(&scanner3);
    let writer3 = &mut scanner3;
    calibrate(&mut scanner3);
    writer3.0 *= 101;
    let reader3 = &scanner3;
    println!("Writer {}", writer3.0);
    println!("Reader: {}", reader3.0);

    // TASK 4 - Multiple mutable (and immutable) borrows via parameters
    // Hint: Not allowed - do it another way (modify 'sync_sensors' as well)
    println!("Task 4");
    let mut scanner4 = Sensor(100);
    read(&scanner4);
    sync_sensors(&mut scanner4, &mut scanner4);
    read(&scanner4);

    // TASK 5 - Borrows in methods
    // Hint: Use 'self' to refer to object but same conflict rules apply
    println!("Task 5");
    let mut scanner5 = Sensor(123);
    let reader = &scanner5;
    println!("Reader: {}", reader.read());
    scanner5.calibrate();
    println!("Reader: {}", reader.read());


    // TASK 6 - Interior Mutability - increment counter every time
    //          you read the sensor
    // Hint: Use "Cell" (and it's accessor functions) to increment
    //       counter on read, and print when queried
    println!("Task 6");

    struct SensorWithCount{
        data: i32,
        count: i32,
    }

    impl SensorWithCount {
        fn read(&self) -> i32 {
            self.count = self.count + 1;
            self.data
        }
    }

    let scanner6 = SensorWithCount {
        data: 42,
        count: 0,
    };
    println!("Reads: {}", scanner6.count);
    scanner6.read();
    println!("Reads: {}", scanner6.count);
    scanner6.read();
    scanner6.read();
    println!("Reads: {}", scanner6.count);

    // TASK 7 - Implement a sensor history
    // Hint: Use RefCell and its member functions to read/modify the history
    // Hint: RefCell Invalid borrows are runtime failures, not compile errors
    println!("Task 7");

    struct History{
        data: Vec<u32>,
    }
    let values = History {
        data: vec![1, 2, 3],
    }
    let mutable = &mut values;
    println!("Values: {:?}", mutable.data);
    mutable.data.push(4);
    println!("Values: {:?}", mutable.data);
    mutable.data.push(5);
    let immutable = values.data.borrow();
    println!("Values: {:?}", *immutable);
  
}

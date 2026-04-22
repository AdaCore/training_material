// Lab (prompt)
//  Types and Values

//! Fix all the compile errors below and/or follow the hints provided

#[allow(dead_code)]
#[allow(unused_variables)]


fn main() {
    // TASK 1 - Variable Declaration
    // Hint: Variable declaration has a particular syntax using ":" for types and "=" for value and ending with a ";"
    let apples := 5;
    let person = "Alice" 
    

    // TASK 2 - Mutable Variable
    // Hint: Keyword "mut" makes a variable mutable. Add a declaration to make 'money' mutable
    money = money + 20; 
    

    // TASK 3 - Variable Types
    // Hint: initial value must match the types
    let initial : i32 = 'a';
    let big_number : u8 = 150000;     
       

    // TASK 4 - Type inference and Static Type
    // Hint: Type is inferred by initial value. Numeric literals must match type assigned
    let mut my_float = 6.5;
    my_float = 5;
    let mut my_int = 18;
    my_int += 7.5; 
    

    // TASK 5 - Base Numeric Literal
    // Hint: Convert the value into different bases and verify the display
    let binary = 0b1111_0000;
    let decimal = ;
    let octal = ;
    let hexadecimal = ;

    println!("TASK5 => binary     : {}", binary);
    println!("TASK5 => decimal    : {}", decimal);
    println!("TASK5 => octal      : {}", octal);
    println!("TASK5 => hexadecimal: {}", hexadecimal);
        
    
    // TASK 6 - Numeric Conversion
    // Hint: Keyword "as" tells the compiler to interpret a value as a different type  
    let my_int2: i32 = 10;
    let my_float2: f64 = 5.5;
    let sum = my_int2 + my_float2;
        

    // TASK 7 - Arithmetic and the Exponent Trap
    // Hint: Code compiles but the value is not correct 
    let exponent = 5 ˆ 2;
    println!("TASK7 => 5 to the power of 2 equals: {}", exponent);
        

    // TASK 8 - Arithmetic Division
    // Hint: Code compiles but the value is not correct. A division between 2 integers results in an integer 
    let precise = 7 / 3;
    println!("TASK8 => 7 divided by 3 equals: {}", precise);
    let too_precise = 8.000 / 2.000;
    println!("TASK8 => 8 divided by 2 equals: {:?}", too_precise);   
}

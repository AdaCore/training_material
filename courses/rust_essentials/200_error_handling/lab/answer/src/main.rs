//! Lab (answer)
//! Error Handling
//!

#![allow(unused_assignments)]
#![allow(unused_variables)]
#![allow(dead_code)]

fn main() {
    // TASK 1 - Error Result
    // Hint: You can return an error using a 'Result' enum variant 'Err' 
    enum Reason { TooYoung, TooOld, }
    fn check_age(age: i32) -> Result<i32, Reason> {
        if age < 18 {
            Err(Reason::TooYoung)
        } else {
            Ok(age)
        }
    }
    let my_age = 17;

    // TASK 2 - Handling Results
    // Hint: Replace the 'unwrap()' function by a pattern matching to 'handle check_age' return
    match check_age(my_age) {
        Ok(age) => println!("Access granted! You are {} years old.", age),
        Err(Reason::TooYoung) => println!("Sorry, you're not old enough yet."),
        Err(Reason::TooOld) => println!("System error: User exceeds maximum age limit."),
    }
    
    // TASK 3 - The Try Operator
    // Hint: Use the try operator to return an error automatically to the caller of 'register' 
    // if check_age returns an error
    // The try operator tries to convert 'reason' to the return type of 'register' 'string'
    // From trait should be implemented
    impl From<Reason> for String {
        fn from(reason: Reason) -> Self {
            match reason {
                Reason::TooYoung => "User is under the required age limit".to_string(),
                Reason::TooOld => "User is above the maximum age limit".to_string(),
            }
        }
    }
    fn register() -> Result<(), String> {
        check_age(10)?;
        Ok(())
    }  

    // TASK 4 - Handling Propagation
    // Hint: Error has been propagated from 'check_age' to 'register' to 'main'
    // Use pattern matching to handle return from 'register' instead of 'unwrap()'
    // Print a message for each possible result
    match register() {
        Ok(_) => println!("Registration successful!"),
        Err(e) => println!("Registration failed: {}", e),
    }
    
    // TASK 5 - thiserror
    // Hint: Convert the comments over the two variants into error text using 'thiserror'
    // Then make sure withdraw returns an 'AccountError' of 'InsufficientFunds' 
    // Display the 'balance' and the 'amount'
    use thiserror::Error;
    #[derive(Error, Debug)]
    pub enum AccountError {
        #[error("insufficient funds: available {available}, requested {requested}")]
        InsufficientFunds { available: u64, requested: u64 },
        #[error("account is inactive")]
        InactiveAccount,
    }
    fn withdraw(balance: u64, amount: u64) -> Result<u64, AccountError> {
        if amount > balance {
            return Err(AccountError::InsufficientFunds { 
                available: balance, 
                requested: amount 
            });
        }
        Ok(balance - amount)
    }

    // Task 6 - Anyhow
    // Hint: Turn the 'println' in 'process_transaction' into an additional context of information
    // Attached to the withdraw function
    // You have to use the correct library to be able to use the 'with_context' method
    use anyhow::Context;
    fn process_transaction(balance: u64, amount: u64) -> anyhow::Result<()> {
        withdraw(balance, amount)
            .with_context(|| format!("Failed to process withdrawal of ${}", amount))?;
        Ok(())
    }
    let result = process_transaction(50, 100);
    if let Err(e) = result {
        // This will print a message that looks like an actual run-time error
        println!("Error: {}", e); 
        // This next line will print the following :
        // Debug Trace: Failed to process withdrawal of $100
        // Caused by:
        // insufficient funds: available 50, requested 100
        println!("\nDebug Trace: {:?}", e);
    }
}

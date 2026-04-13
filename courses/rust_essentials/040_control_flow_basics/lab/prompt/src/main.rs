// Objective: Survive the encounter and tame the dragon.
// Task: Fix the code section by section. Follow the compiler errors!

fn main() {
    let mut health = 100;
    let mut distance = 3;
    let is_brave = true;

    println!("--- A Wild Dragon Appears! ---");

    // --- TASK 1: The Fire Breath (Topic: Blocks) ---
    // GOAL: The block should calculate damage and return it.
    // ERROR: The semicolon on the last line is preventing the return!
    let damage: i32 = {
        let fire_intensity = 10;
        fire_intensity * 3; 
    };

    health -= damage;
    
    // --- TASK 2: Health Check (Topic: dbg! Macro) ---
    // GOAL: Use dbg! to print your health to the console.
    // ERROR: The compiler warns about an unused variable 'health' if we don't.
    // (Uncomment the line below and wrap health in the dbg! macro)
    // ???(health);

    // --- TASK 3: The Approach (Topic: Loops & Break) ---
    // GOAL: Approach the dragon (distance -> 0) and break the loop.
    // ERROR: This will 'panic' at runtime because of the todo! macro.
    loop {
        if distance == 0 {
            todo!("Replace this macro with the 'break' keyword");
        }
        
        if distance == 1 {
            println!("Stepping closer... distance is 1 meter");
        } else {
            println!("Stepping closer... distance is {} meters", distance);
        }

               distance -= 1;
    }

    // --- TASK 4: The Taming (Topic: Match Expressions) ---
    // GOAL: Handle the outcome based on a 'tame_chance' roll.
    // ERROR: The compiler will say "non-exhaustive patterns".
    let tame_chance = 1; 
    match tame_chance {
        1 => println!("The dragon bows its head! Success!"),
        2 => println!("The dragon is unimpressed."),
        // HINT: Add the '_' catch-all arm here!
    }

    // --- TASK 5: Survival Check (Topic: Functions & unreachable!) ---
    if is_brave && health > 0 {
        println!("You survived with {} health!", health);
    } else {
        // ERROR: If you make it this far, you should have survived.
        // If the code hits this branch, it should crash with a message.
        unreachable!("Wait, how did we die? The health check failed!");
    }
}

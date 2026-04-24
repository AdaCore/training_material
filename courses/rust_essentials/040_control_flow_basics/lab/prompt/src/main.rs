//! Lab (prompt)
//! Control Flow Basics

// Objective: Survive the encounter and tame the dragon.
// Task: Fix the code section by section. Follow the compiler errors!

fn main() {
    let mut health = 100;
    let mut distance = 3;
    let is_brave = true;

    println!("--- A Wild Dragon Appears! ---");

    // --- TASK 1: The Fire Breath ---
    // GOAL: The block should calculate damage and return it.
    // How do we get a block of code to return a value?
    let damage: i32 = {
        let fire_intensity = 10;
        fire_intensity * 3;
    };

    health -= damage;
    
    // --- TASK 2: Health Check ---
    // GOAL: Print your health to the console.
    // Replace 'todo!' with the correct macro to meet the goal
    todo!(health);

    // --- TASK 3: The Approach ---
    // GOAL: Approach the dragon (to 0 meters)!
    loop {
        if distance == 0 {
            todo!("What keyword that breaks out of a loop?");
        }
        
        if distance == 1 {
            println!("Stepping closer... distance is 1 meter");
        } else {
            println!("Stepping closer... distance is {} meters", distance);
        }

        distance -= 1;
    }

    // --- TASK 4: The Taming ---
    // GOAL: Handle the outcome based on a 'tame_chance' roll.
    // How do we handle all possible values? (replace ???)
    let tame_chance = 1; 
    match tame_chance {
        1 => println!("The dragon bows its head! Success!"),
        2 => println!("The dragon is unimpressed."),
        // ??? println!("The dragon stares blankly."),
    }

    // --- TASK 5: The Logic Guard ---
    // 1. Try setting 'is_brave' to false. What happens when you run it then?      
    // 2. Replace 'todo' with the macro that signals: "My logic assumes we never reach this!"
    if is_brave && health > 0 {
        println!("You survived with {} health!", health);
    } else {
        todo!("The ritual should have been a guaranteed success!");
    }
}

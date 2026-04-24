//! Lab (answer)
//! Control Flow Basics

fn main() {
    let mut health = 100;
    let mut distance = 3;
    let is_brave = true; 

    println!("--- A Wild Dragon Appears! ---");

    // --- TASK 1: The Fire Breath ---
    let damage: i32 = {
        let fire_intensity = 10;
        fire_intensity * 3 
    };

    health -= damage;
    
    // --- TASK 2: Health Check ---
    dbg!(health);

    // --- TASK 3: The Approach ---
    loop {
        if distance == 0 {
            break; 
        }
        
        if distance == 1 {
            println!("Stepping closer... distance is 1 meter");
        } else {
            println!("Stepping closer... distance is {} meters", distance);
        }

        distance -= 1;
    }

    // --- TASK 4: The Taming ---
    let tame_chance = 1; 
    match tame_chance {
        1 => println!("The dragon bows its head! Success!"),
        2 => println!("The dragon is unimpressed."),
        _ => println!("The dragon stares blankly."),
    }

    // --- TASK 5: The Logic Guard ---
    // 1. Try setting 'is_brave' to false. What happens when you run it then?      
    // 2. Replace 'todo' with the macro that signals: "My logic assumes we never reach this!"
    if is_brave && health > 0 {
        println!("You survived with {} health!", health);
    } else {
        unreachable!("The ritual should have been a guaranteed success!");
    }
}

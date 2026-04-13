fn main() {
    let mut health = 100;
    let mut distance = 3;
    let is_brave = true;

    println!("--- A Wild Dragon Appears! ---");

    // --- TASK 1: The Fire Breath (Topic: Blocks) ---
    // FIXED: Removed the semicolon so the block evaluates to 30.
    let damage: i32 = {
        let fire_intensity = 10;
        fire_intensity * 3 
    };

    health -= damage;
    
    // --- TASK 2: Health Check (Topic: dbg! Macro) ---
    // FIXED: Used dbg! to inspect health. Note the output includes line numbers!
    dbg!(health);

    // --- TASK 3: The Approach (Topic: Loops & Break) ---
    // FIXED: Replaced todo! with break to exit the loop once distance reaches 0.
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

    // --- TASK 4: The Taming (Topic: Match Expressions) ---
    // FIXED: Added the '_' catch-all arm to make the match exhaustive.
    let tame_chance = 1; 
    match tame_chance {
        1 => println!("The dragon bows its head! Success!"),
        2 => println!("The dragon is unimpressed."),
        _ => println!("The dragon ignores you."), 
    }

    // --- TASK 5: Survival Check (Topic: Functions & unreachable!) ---
    if is_brave && health > 0 {
        println!("You survived with {} health!", health);
    } else {
        // This remains as a logic guard. If we reached this branch despite 
        // starting with 100 health and taking only 30 damage, something is wrong!
        unreachable!("Wait, how did we die? The health check failed!");
    }
}

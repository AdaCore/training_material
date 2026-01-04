==================
Let Control Flow
==================

----------------------------------------
Concise Flow: "if let" and "while let"
----------------------------------------

- **if let** - For a match that only cares about **one** specific case
    - Unlike :rust:`match`, it does not have to cover all branches

- **while let** - Repeatedly tests a value against a pattern and loops as long as it matches

.. code:: rust

    // if let: executes only if 'secs' converts to Ok
    if let Ok(duration) = Duration::try_from_secs_f32(secs) { 
        println!("Slept for {duration:?}");
    }

    // while let: pops until the string is empty
    while let Some(c) = name.pop() { 
        println!("Char: {c}"); 
    }

--------------------------
The "let else" Statement
--------------------------

- **let else** - Used for "unwrapping or returning early"[cite: 187].

- **Flattening** - It supports flattening nested code by handling the "failure" case first

- **Divergence** - The :rust:`else` block **must** diverge (:rust:`return`, :rust:`break`, or :rust:`panic!``)

.. code:: rust

    fn hex_or_die(maybe_string: Option<String>) -> Result<u32, String> {
        // If None, return early 
        // If Some, 's' is available in current scope
        let Some(s) = maybe_string else { 
            return Err(String::from("got None")); 
        };

        // Further logic remains un-nested...
    }

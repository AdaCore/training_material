=========
HashMap
=========

-------------------------
What Is "HashMap<K,V>"?
-------------------------

* :rust:`HashMap<K,V>` is standard *key-value map*

  * Stores associations between keys (:rust:`K`) and values (:rust:`V`)

* Must be explicitly imported

  .. code::rust

    use std::collections::HashMap;

-------------
Basic Usage
-------------

* Creation

  .. code:: rust

    let mut map = HashMap::new();

* Insertion of key/value pair

  .. code:: rust

    map.insert("apple".to_string(), 3);
    map.insert("banana".to_string(), 5);

* Safe lookup

  * :rust:`get` returns an :rust:`Option<&V>`

     * Handle missing keys explicitly

  .. code:: rust

    if let Some(count) = map.get("apple") {
        println!("Apples: {}", count);
    }

----------------------------
Using ".entry()" to Update
----------------------------

* Use :rust:`entry()` to combine and insert in one step

  * Inserts a default if key isn’t present
  * Lets you modify the stored value by mutable reference

.. code:: rust

  let count = map.entry("pear".to_string()).or_insert(0);
  *count += 1;

------------------------------
Initialization and Iteration
------------------------------

* Inline initialization

  * No built-in macro (like :rust:`vec!`)
  * Use :rust:`from` instead

  .. code:: rust

    let map = HashMap::from([
        ("x".to_string(), 10),
        ("y".to_string(), 20),
    ]);

* Iterating over a map

  .. code:: rust

    for (k, v) in &map {
        println!("{k}: {v}");
    }

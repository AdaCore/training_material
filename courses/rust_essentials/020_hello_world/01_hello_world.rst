==============
Hello, World
==============

--------------
Hello, World
--------------

.. code:: rust

   // Our first program!
   fn main() {
       println!("Hello World!");
   }

What you see:

- :rust:`//` - indicates a single-line **comment**
- :rust:`fn` - "introduces" a **function**
- :rust:`{ }` - curly braces enclose a **block**
- :rust:`main` - the entry point of the program
- :rust:`println!` - macro for printing a string followed by newline

-----------------------------
Creating Your First Program
-----------------------------

- From a command prompt, execute the following in a directory of your choice:
  - :command:`cargo new hello_world`
- A package will be created for the executable program (:dfn:`binary crate`)
- A directory will also be created (:filename:`hello_world/`)
- A few noteworthy things automatically created at this step:
  - :filename:`Cargo.toml` - the :dfn:`manifest` used by **cargo**
  - :filename:`src/main.rs` - the program source code
    - Open :toolname:`VS Code` to explore this file

-----------------------------
Building Your First Program
-----------------------------

- From a command prompt, ensure you are in the project directory (:filename:`hello_world/`)

- Execute the following command:
  - :command:` cargo build`

- You should see something like:

::
  Compiling hello v0.1.0 (C:\rust\hello_world)
  Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.52s

----------------------------
Running Your First Program
----------------------------

- From a command prompt, ensure you are in the project directory (:filename:`hello_world/`)

- Execute the following command:
  - :command:` cargo run`

- You should see an output like the following:
::
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.01s
     Running `target\debug\hello_world.exe`
Hello, world!

----------------------
First Program: Done!
----------------------
* **Congratulations!** You've completed your first program!
  * Test sub

* We've touched on a few basic concepts like

  * Comments
  * Functions
  * Macros
  * Program output
  * Code blocks

* On to the bigger concepts!

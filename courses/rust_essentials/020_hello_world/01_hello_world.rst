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

* :rust:`//` - indicates a single-line **comment**
* :rust:`fn` - "introduces" a **function**
* :rust:`{ }` - curly braces enclose a **block**
* :rust:`main` - the entry point of the program
* :rust:`println!` - macro for printing a string followed by newline

-----------------------------
Creating Your First Program
-----------------------------

* From a command prompt, execute the following in a directory of your choice:
  
  * :command:`cargo new hello_world`

* A package will be created for the executable program (:dfn:`binary crate`)

* A directory will also be created (:filename:`hello_world/`)

* A few noteworthy things automatically created at this step:
 
   * :filename:`Cargo.toml` - the :dfn:`manifest` used by **cargo**
   * :filename:`src/main.rs` - the program source code
    
      * Open :toolname:`VS Code` to explore this file

-----------------------------
Creating Your First Program
-----------------------------

* From a command prompt, execute the following in a directory of your choice:
  
  * :command:`cargo new hello_world`

* A package will be created for the executable program (:dfn:`binary crate`)

* A directory will also be created (:filename:`hello_world/`)

* A few noteworthy things automatically created at this step:
 
   * :filename:`Cargo.toml` - the :dfn:`manifest` used by **cargo**
   * :filename:`src/main.rs` - the program source code
    
      * Open :toolname:`VS Code` to explore this file
      
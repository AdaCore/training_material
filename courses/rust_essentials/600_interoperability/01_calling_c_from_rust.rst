=====================
Calling C from Rust
=====================


-----------------
What Is an ABI?
-----------------

**API and ABI describe contracts at different levels**

* **API** - Application Programming Interface
  * Source-level contract
* **ABI** - Application Binary Interface
  * Compiled-code contract
  * Calling convention
  * Register and stack use
  * Argument and return representation
  * Data alignment
  * Symbol naming
  * Unwinding behavior


------------
extern "C"
------------

:rust:`unsafe extern "C"` **declares functions defined elsewhere that Rust code can call**

.. code:: rust

  unsafe extern "C" {
      fn foreign_function();
  }

* **Foreign Function Interface (FFI)** - enables calls across language boundaries
* This block declares the expected function signature, not an implementation
* :rust:`unsafe` marks the block because Rust cannot verify its declarations
* Calls to the declared function are unsafe by default
* A declaration may be marked :rust:`safe` when every valid call is safe
* The implementation need not be written in C

.. note::

  :rust:`"C"` describes the ABI, not the implementation language


---------------------------
C-Compatible Scalar Types
---------------------------

**Match the foreign declaration exactly**

* Fixed-width integers
  * :C:`int32_t` / :C:`uint32_t` |rightarrow| :rust:`i32` / :rust:`u32`
  * :C:`int64_t` / :C:`uint64_t` |rightarrow| :rust:`i64` / :rust:`u64`
* Platform C types
  * :C:`int` / :C:`unsigned int` |rightarrow| :rust:`c_int` / :rust:`c_uint`
  * :C:`char` |rightarrow| :rust:`c_char` (target-dependent signedness)
  * :C:`void *` / :C:`const void *` |rightarrow| :rust:`*mut c_void` / :rust:`*const c_void`

.. note::

  :rust:`c_int` matches C :C:`int`; unlike :rust:`i32`, its width is target-dependent


--------------------
A Small C Function
--------------------

* **C header** - declares the interface Rust must match

.. code:: c

  // rebel_math.h
  #include <stdint.h>

  int64_t calculate_jump_cost(
      int32_t distance,
      int32_t risk
  );

* **C source file** - illustrates an implementation; the arithmetic is not the focus

.. code:: c

  // rebel_math.c
  #include "rebel_math.h"

  int64_t calculate_jump_cost(
      int32_t distance,
      int32_t risk
  ) {
      return (int64_t)distance + ((int64_t)risk * 10);
  }


--------------------------------
Declaring the Function in Rust
--------------------------------

**Declare foreign functions in an unsafe external block**

.. code:: rust

  unsafe extern "C" {
      fn calculate_jump_cost(
          distance: i32,
          risk: i32,
      ) -> i64;
  }

* C calling convention
* **Symbol name:** :rust:`calculate_jump_cost`
* Two 32-bit signed integer inputs
* One 64-bit signed integer result

.. note::

  :rust:`bindgen` can generate raw Rust declarations from C or C++ headers,
  but the generated declarations still need review


------------------------------
Calling the Function in Rust
------------------------------

**This foreign function is unsafe to call**

.. code:: rust

  fn main() {
      // SAFETY: The declaration matches the C function,
      // which accepts any two i32 values
      let cost = unsafe {
          calculate_jump_cost(120, 4)
      };

      println!("Jump cost: {cost}");
  }

.. code:: output

  Jump cost: 160

* An idiomatic :rust:`// SAFETY:` comment explains
  * Why the declaration matches
  * Why the arguments are valid
  * Which lifetime or ownership rules apply, if any


----------------------------
Linking Is a Separate Step
----------------------------

**Linking is separate from declaring the function**

* Common approaches
  * Build bundled C source with the Rust package
  * Link a system-installed static library
  * Link a system-installed dynamic library
  * Let a larger build system link Rust and C object files together
* Typical failures
  * Symbol not found
  * Wrong library search path
  * Debug/release library mismatch
  * Architecture mismatch
  * Incompatible C runtime or compiler toolchain


------------------------------------
Compiling and Linking C with Cargo
------------------------------------

**Compile the bundled C implementation with Cargo**

* Place :filename:`rebel_math.c` and :filename:`rebel_math.h` in :filename:`native/`
* Add :rust:`cc` to :filename:`Cargo.toml`

.. code:: toml

  [build-dependencies]
  cc = "1"

* Create :filename:`build.rs` in the package root

.. code:: rust

  fn main() {
      cc::Build::new()
          .file("native/rebel_math.c")
          .include("native")
          .compile("rebel_math");
  }

* :rust:`cc` compiles C and emits the link instructions for Cargo

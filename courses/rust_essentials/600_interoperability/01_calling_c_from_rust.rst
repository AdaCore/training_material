=====================
Calling C from Rust
=====================


-----------------
What Is an ABI?
-----------------

**API and ABI describe contracts at different levels**

* API - Application Programming Interface
  * Source-level contract
* ABI - Application Binary Interface
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

:rust:`extern "C"` **selects the platform's C ABI**

.. code:: rust

  unsafe extern "C" {
      fn foreign_function();
  }

* Implementation need not be written in C
* Declaration may still be wrong
* Pointer validity is not checked
* Memory safety is not guaranteed
* Required dynamic libraries may still be missing at runtime

.. note::

  :rust:`"C"` describes the interface, not the implementation


---------------------------
C-Compatible Scalar Types
---------------------------

**Match the foreign declaration exactly**

* Fixed-width integers
  * :C:`int32_t` / :C:`uint32_t` |rightarrow| :rust:`i32` / :rust:`u32`
  * :C:`int64_t` / :C:`uint64_t` |rightarrow| :rust:`i64` / :rust:`u64`
* Platform C types
  * :C:`int` / :C:`unsigned int` |rightarrow| :rust:`c_int` / :rust:`c_uint`
  * :C:`char` |rightarrow| :rust:`c_char`
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

* **C source file** - provides the implementation

.. code:: c

  // rebel_math.c
  #include "rebel_math.h"

  int64_t calculate_jump_cost(
      int32_t distance,
      int32_t risk
  ) {
      return (int64_t)distance + ((int64_t)risk * 10);
  }


------------------------
Declaring the Function
------------------------

**Declare foreign functions in an unsafe external block**

.. code:: rust

  unsafe extern "C" {
      fn calculate_jump_cost(
          distance: i32,
          risk: i32,
      ) -> i64;
  }

* Symbol name: :rust:`calculate_jump_cost`
* C calling convention
* Two 32-bit signed integer inputs
* One 64-bit signed integer result


----------------------
Calling the Function
----------------------

**This foreign function is unsafe to call**

.. code:: rust

  fn main() {
      // SAFETY: The declaration matches `rebel_math.h`
      // and these scalar arguments have no extra preconditions
      let cost = unsafe {
          calculate_jump_cost(120, 4)
      };

      println!("Jump cost: {cost}");
  }

.. code:: output

  Jump cost: 160

* Functions declared in an external block are unsafe by default
* A declaration may be marked :rust:`safe` when every valid call is safe
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


---------------------
Cargo Build Scripts
---------------------

**Cargo can run an optional build script before compiling a package**

* :filename:`build.rs` is a Rust program in the package root
* It performs build-time tasks, such as compiling C source
* It can also tell Cargo which native libraries to link


------------------------------
Configuring the Native Build
------------------------------

**Declare native build dependencies in** :filename:`Cargo.toml`

.. code:: toml

  [package]
  name = "rebel_math"
  version = "0.1.0"
  edition = "2024"

  [build-dependencies]
  cc = "1"

* :rust:`cc` is a build dependency
  * Invokes an available C compiler
* :filename:`build.rs` is the package build script
  * Can use :rust:`cc` to compile bundled C source

.. note::

  A working C compiler is an additional build prerequisite


------------------------
Compiling the C Source
------------------------

**Cargo runs** :filename:`build.rs` **before compiling the Rust target**

.. code:: rust

  fn main() {
      println!(
          "cargo::rerun-if-changed=native/rebel_math.c"
      );
      println!(
          "cargo::rerun-if-changed=native/rebel_math.h"
      );

      cc::Build::new()
          .file("native/rebel_math.c")
          .include("native")
          .compile("rebel_math");
  }

* ``cargo::rerun-if-changed`` controls build-script reruns

.. note::

  :rust:`cc` builds a static library and emits Cargo metadata to link it

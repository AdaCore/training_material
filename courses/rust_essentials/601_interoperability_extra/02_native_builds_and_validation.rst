==============================
Native Builds and Validation
==============================


---------------------
Cargo Build Scripts
---------------------

**Cargo can run an optional build script before compiling a package**

* :filename:`build.rs` is a Rust program in the package root
* It performs build-time tasks, such as compiling C source
* It can also tell Cargo which native libraries to link
* Builds on the :filename:`rebel_math.c` example from module 600


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


-----------------------------
Checking Layout Assumptions
-----------------------------

**Verify C and Rust layout compatibility at build or test time**

* Compare size, alignment, and field offsets
  * Rust: :rust:`size_of`, :rust:`align_of`, :rust:`offset_of!`
  * C: :C:`sizeof`, :C:`_Alignof`, :C:`offsetof`
* Check against the same expected layout
  * Compile-time assertions or automated layout tests
* Run checks in CI for every supported target and toolchain configuration


-----------------------------
Managing Generated Bindings
-----------------------------

**Treat generated bindings as reviewed generated code**

* Allowlist only the needed API
* Pin :rust:`bindgen`, Clang/libclang, and header versions
* Generate deliberately rather than during every consumer build
* Review generated diffs
* Test layout and symbols on each supported platform
* Keep generated code out of the ergonomic public API

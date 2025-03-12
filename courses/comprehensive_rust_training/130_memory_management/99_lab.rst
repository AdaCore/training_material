========================
Exercise: Builder Type
========================

------------------------
Builder Type Setup
------------------------

In this example, we will implement a complex data type that owns all of
its data. We will use the *builder pattern* to support building a new
value piece-by-piece, using convenience functions.

.. container:: source_include 130_memory_management/src/130_memory_management.rs :start-after://ANCHOR-package :end-before://ANCHOR-package_solution :code:rust

.. code:: Rust

   impl Package {
       /// Return a representation of this package as a dependency, for use in
       /// building other packages.
       fn as_dependency(&self) -> Dependency {
           todo!("1")
       }
   }

.. container:: source_include 130_memory_management/src/130_memory_management.rs :start-after://ANCHOR-builder :end-before://ANCHOR-builder_solution :code:rust

------------------------
Builder Type Problem
------------------------

Fill in the missing pieces.

.. code:: rust

   impl PackageBuilder {
       fn new(name: impl Into<String>) -> Self {
           todo!("2")
       }

       /// Set the package version.
       fn version(mut self, version: impl Into<String>) -> Self {
           self.0.version = version.into();
           self
       }

       /// Set the package authors.
       fn authors(mut self, authors: Vec<String>) -> Self {
           todo!("3")
       }

       /// Add an additional dependency.
       fn dependency(mut self, dependency: Dependency) -> Self {
           todo!("4")
       }

       /// Set the language. If not set, language defaults to None.
       fn language(mut self, language: Language) -> Self {
           todo!("5")
       }

       fn build(self) -> Package {
           self.0
       }
   }

------------------------
Builder Type Main
------------------------

.. container:: source_include 130_memory_management/src/130_memory_management.rs :start-after://ANCHOR-main :code:rust

------------------------
Builder Type Solution
------------------------

.. container:: source_include 130_memory_management/src/130_memory_management.rs :start-after://ANCHOR-package_solution :end-before://ANCHOR-builder :code:rust

.. container:: source_include 130_memory_management/src/130_memory_management.rs :start-after://ANCHOR-builder_solution :end-before://ANCHOR-main :code:rust

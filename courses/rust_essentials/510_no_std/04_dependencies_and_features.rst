===========================
Dependencies and Features
===========================

----------------------------
"no_std" Is Not Transitive
----------------------------

* :rust:`#![no_std]` applies only to the crate where it is declared

* Dependencies used by the library must support intended configuration

  * Dependencies requiring :rust:`std` can be used on targets supporting :rust:`std`

  * On a target without :rust:`std` support, such a dependency prevents the build

* Default features of dependencies may require :rust:`std` or heap allocation

* Dependency compatibility depends on documented features, not just crate descriptions

--------------------------------
Offering Optional Capabilities
--------------------------------

**Feature definitions in** :filename:`sample_tools/Cargo.toml`

.. code:: toml

  [features]
  default = ["std"]
  alloc = []
  std = ["alloc"]

* ``default = ["std"]`` enables the :rust:`std` feature by default

* ``alloc = []`` defines a feature without enabling others or providing an allocator

* ``std = ["alloc"]`` makes the :rust:`std` feature also enable :rust:`alloc`

.. note::

  :rust:`std` and :rust:`alloc` are ordinary Cargo feature names

----------------------------------
Supported Library Configurations
----------------------------------

.. list-table::
  :header-rows: 1
  :widths: 32 68

  * - Enabled features
    - Intended library configuration
  * - None
    - :rust:`core` only
  * - :rust:`alloc`
    - :rust:`no_std` adding allocating APIs
  * - :rust:`std`
    - All APIs, including :rust:`std` conveniences

* Core APIs remain available in every configuration

* Allocating convenience APIs can remain optional for callers

----------------------------------
How Features Select Capabilities
----------------------------------

.. image:: rust_essentials/510_feature_configuration_flow.svg
   :width: 100%
   :align: center

* Build flags choose the active feature set

* In this module, :rust:`std` implies :rust:`alloc`

* :rust:`#[cfg(condition)]` includes the following item only when the condition holds

---------------------------------
Apply the Features to the Crate
---------------------------------

**Updated crate declarations replacing earlier** :rust:`#![no_std]` **and** :rust:`extern crate alloc` **declarations**

.. code:: rust

  #![cfg_attr(not(feature = "std"), no_std)]

  #[cfg(feature = "alloc")]
  extern crate alloc;

* :rust:`cfg_attr(predicate, attribute)` conditionally applies an attribute

  * Here, :rust:`not(feature = "std")` controls whether :rust:`no_std` applies

* :rust:`#[...]` is an outer attribute applying to the following item

  * Here, :rust:`#[cfg(feature = "alloc")]` includes :rust:`extern crate alloc` only when the feature is enabled

* :rust:`core` imports and non-allocating functions remain unchanged

.. warning::

  Gate each optional API together with its imports

-----------------------------------
Gate the Allocation-Requiring API
-----------------------------------

**Feature-gated versions of** :rust:`Vec` **import and** :rust:`sorted_samples` **implementation**

.. code:: rust

  #[cfg(feature = "alloc")]
  use alloc::vec::Vec;

  #[cfg(feature = "alloc")]
  pub fn sorted_samples(samples: &[u16]) -> Vec<u16> {
      let mut sorted = Vec::from(samples);
      sorted.sort_unstable();
      sorted
  }

* :rust:`sorted_samples` is absent when :rust:`alloc` is disabled

* :rust:`maximum`, :rust:`parse_pair`, and :rust:`write_maximum` remain available

--------------------------------
Gate the Second Allocating API
--------------------------------

**Feature-gated replacements for** :rust:`make_message` **and its imports**

.. code:: rust

  #[cfg(feature = "alloc")]
  use alloc::string::String;

  #[cfg(feature = "alloc")]
  use alloc::{format, vec};

  #[cfg(feature = "alloc")]
  pub fn make_message() -> (Vec<u16>, String) {
      let values = vec![10, 42, 7];
      let text = format!("{} samples", values.len());
      (values, text)
  }

* :rust:`Vec` import is already gated on the previous slide

* :rust:`make_message` is absent when :rust:`alloc` is disabled

-----------------------------------
Keep "std"-Only Adapters Separate
-----------------------------------

**Optional adapter isolated in its own module**

.. code:: rust

  #[cfg(feature = "std")]
  pub mod std_support {
      pub fn print_maximum(samples: &[u16]) {
          if let Some(value) = super::maximum(samples) {
              println!("Maximum: {value}");
          }
      }
  }

* Only this adapter relies on standard output

* Calculation remains usable without the adapter

.. tip::

  A :rust:`std` application can use this library without enabling its :rust:`std` feature

----------------------------------------
Select Features in a Dependent Project
----------------------------------------

**Feature selection in dependent package's** :filename:`Cargo.toml`

.. code:: toml

  [dependencies.sample_tools]
  path = "../sample_tools"
  default-features = false
  # features = ["alloc"]

* :filename:`../sample_tools` points to the library in a sibling directory

* Requests library without its default :rust:`std` feature

* Enabling the commented feature line also requests allocating APIs

* Dependency feature selection follows documented requirements

.. note::

  :command:`--no-default-features` applies to the selected package. It does not disable every dependency's default features

-----------------
Features Add Up
-----------------

* Feature requests for a dependency are generally combined

* Another dependency may request its :rust:`std` feature

* ``default-features = false`` is not a global ban on that feature

**Feature inspection commands executed from dependent package's directory**

.. code:: sh

  cargo tree -f "{p}: {f}"
  cargo tree -e features -i sample_tools

.. tip::

  Resolved features reflect requests across the dependency graph

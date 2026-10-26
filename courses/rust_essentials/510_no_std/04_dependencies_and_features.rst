===========================
Dependencies and Features
===========================

----------------------------
"no_std" Is Not Transitive
----------------------------

* Attribute *only* applies to crate where it is written

* Dependencies used by the library must support intended configuration

* A dependency's default features may require :rust:`std` or heap allocation

* Check dependency's documented features, not just its description

--------------------------------
Offering Optional Capabilities
--------------------------------

**Add this section to** :filename:`sample_tools/Cargo.toml`

.. code:: toml

  [features]
  default = ["std"]
  alloc = []
  std = ["alloc"]

* ``default = ["std"]`` enables the :rust:`std` feature by default

* ``alloc = []`` defines a feature without enabling other features

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
    - :rust:`no_std` with allocating APIs
  * - :rust:`std`
    - All APIs, including :rust:`std` conveniences

* Core APIs remain available in every configuration

* An allocating convenience API need not force every caller to use allocation

----------------------------------
How Features Select Capabilities
----------------------------------

.. image:: rust_essentials/510_feature_configuration_flow.svg
   :width: 100%
   :align: center

* Build flags choose the active feature set

* In this module, :rust:`std` implies :rust:`alloc`

* :rust:`#[cfg(...)]` keeps only the items for the selected configuration

---------------------------------
Apply the Features to the Crate
---------------------------------

**Replace the earlier** :rust:`#![no_std]` **and** :rust:`extern crate alloc` **declarations**

.. code:: rust

  #![cfg_attr(not(feature = "std"), no_std)]

  #[cfg(feature = "alloc")]
  extern crate alloc;

* :rust:`cfg_attr` applies :rust:`#![no_std]`

  * Only when the :rust:`std` feature is disabled

* :rust:`#[cfg(feature = "alloc")]` includes :rust:`extern crate alloc`

  * Only when the :rust:`alloc` feature is enabled

* Keep the :rust:`core` imports and non-allocating functions unchanged

.. warning::

  Gate each optional API together with its imports

-----------------------------------
Gate the Allocation-Requiring API
-----------------------------------

**Replace the earlier** :rust:`Vec` **import and** :rust:`sorted_samples` **implementation**

.. code:: rust

  #[cfg(feature = "alloc")]
  use alloc::vec::Vec;

  #[cfg(feature = "alloc")]
  pub fn sorted_samples(samples: &[u16]) -> Vec<u16> {
      let mut sorted = Vec::from(samples);
      sorted.sort_unstable();
      sorted
  }

* Function is absent when :rust:`alloc` is disabled

* :rust:`maximum`, :rust:`parse_pair`, and :rust:`write_maximum` remain available

-----------------------------------
Keep "std"-Only Adapters Separate
-----------------------------------

**Isolate an optional adapter in its own module**

.. code:: rust

  #[cfg(feature = "std")]
  pub mod std_support {
      pub fn print_maximum(samples: &[u16]) {
          println!("Maximum: {:?}", super::maximum(samples));
      }
  }

* Only this adapter relies on standard output

* Calculation remains usable without the adapter

.. tip::

  A :rust:`std` application can use this library without enabling its :rust:`std` feature

----------------------------------------
Select Features in a Dependent Project
----------------------------------------

**Add to the dependent package's** :filename:`Cargo.toml`

.. code:: toml

  [dependencies.sample_tools]
  path = "../sample_tools"
  default-features = false
  # features = ["alloc"]

* :filename:`../sample_tools` points to the library in a sibling directory

* Requests library without its default :rust:`std` feature

* Uncomment last line to request its allocating APIs as well

* Select features for each dependency according to its documentation

.. note::

  :command:`--no-default-features` applies to the selected package. It does not disable every dependency's default features

-----------------
Features Add Up
-----------------

* Feature requests for a dependency are generally combined

* Another dependency may request its :rust:`std` feature

* ``default-features = false`` is not a global ban on that feature

**Run from the dependent package's directory**

.. code:: sh

  cargo tree -f "{p}: {f}"
  cargo tree -e features -i sample_tools

.. tip::

  Check the resolved features, including requests from dependencies

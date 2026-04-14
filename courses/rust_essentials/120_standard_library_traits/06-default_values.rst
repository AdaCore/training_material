================
Default Values
================

-----------------
"Default" Trait
-----------------

* :rust:`std::default::Default` trait

  * Defines a default value for a type

    .. code:: rust

      fn default() -> Self

  * Great for API ergonomics and fallback values

* Common Usage

  * Automatically derived with :rust:`#[derive(Default)]`

    * If every field implements :rust:`std::default::Default`

----------------
Default Values
----------------

.. container:: latex_environment footnotesize

  .. list-table::
     :widths: 25 35 40
     :header-rows: 1
     :stub-columns: 1

     * - **Category**
       - **Type**
       - **Default Value**
     * - *Integers*
       - ``i8``, ``u32``, ``isize``, etc.
       - ``0``
     * - *Floats*
       - ``f32``, ``f64``
       - ``0.0``
     * - *Boolean*
       - ``bool``
       - ``false``
     * - *Characters*
       - ``char``
       - ``'\0'`` (Nul)
     * - *Strings*
       - ``String``
       - ``""`` (Empty)
     * - *Collections*
       - ``Vec<T>``, ``HashMap<K, V>``
       - Empty (Size 0)
     * - *Options*
       - ``Option<T>``
       - ``None``
     * - *Smart Pointers*
       - ``Box<T>``, ``Arc<T>``
       - Pointer to ``T::default()``
     * - *Tuples*
       - ``(A, B)``
       - ``(A::default(), B::default())``

------------------------------
Default Values in a "struct"
------------------------------

* Definition

  .. code:: rust

    #[derive(Debug, Default)]
    struct Config {
        port: u16,
        host: String,
        debug: bool,
    }

* Usage

  .. code:: rust

    let example = Config::default();
    println!("Defaults: {example:?}");

  * :rust:`std::default::Default` uses sensible default values for each field

    :command:`Defaults: Config \{ port: 0, host: "", debug: false \}`

* Using *struct update syntax*

  .. code:: rust

    let c2 = Config { port:123, ..Default::default() }

  * :rust:`std::default::Default` uses

    * Sensible default values for :rust:`host` and :rust:`debug`
    * New value for :rust:`port`

------------------------------
Default Values in an "enum"
------------------------------

* An :rust:`enum` can be one of multiple variants

  * So how does Rust pick the default?

* Default value specified by user when creating :rust:`enum`

  .. code:: rust

    #[derive(Default)]
    enum Status {
        #[default]
        Pending,
        Active(i32),
        Closed,
    }

  * :rust:`Pending` becomes the default value

* Derived defaults only work for *unit variants*

  * So :rust:`Active` would not be selectable

.. tip::

  If you want :rust:`Active` to be default, manually
  implement :rust:`Default` trait

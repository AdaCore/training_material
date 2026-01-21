================
Default Values
================

-----------------
"Default" Trait
-----------------

* :rust:`Default` trait

  * Defines a default value for a type via :rust:`fn default() -> Self`
  * Great for API ergonomics and fallback values

* Common Usage

  * Automatically derived with :rust:`#[derive(Default)]` when every
    field implements :rust:`Default`

---------
Example
---------

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

    let c = Config::default();

  * :rust:`default` uses sensible default values for :rust:`port`,
    :rust:`host`, and :rust:`debug`

* Using *struct update syntax*

  .. code:: rust

    let c2 = Config { port:123, ..Default::default() }

  * :rust:`default` uses sensible default values for :rust:`host` and
    :rust:`debug` while using a new value for :rust:`port`

=============================
Increasing the Proof Effort
=============================

-----------------------------
Control of the Proof Effort
-----------------------------

* Automatic provers have different strengths

  - More provers = more likely to prove checks
  - From one prover to four (Alt-Ergo, COLIBRI, cvc5, Z3)
  - Use switch :command:`--provers` e.g. :command:`--provers=all`

* Automatic provers heuristically search for a proof

  - More time = more likely to prove checks
  - Time given in seconds (:command:`--timeout`) or prover-specific steps
    (:command:`--steps`)

* Default proof effort is minimal (one prover, 100 steps)

* Timeout vs steps

  - Timeout is best to bound the running time
  - Steps are useful for reproducible results across machines

    + Still use timeout to avoid runaway proofs

--------------
Proof Levels
--------------

* Switch :command:`--level` bundles lower-level switches

  .. container:: latex_environment scriptsize

    .. list-table::
      :header-rows: 1

      * - **--level=**
        - --prover=
        - --timeout= *(seconds)*
        - --memlimit= *(MB)*
        - --counterexamples=

      * - **0**
        - cvc5
        - 1
        - 1000
        - off
      * - **1**
        - cvc5,z3,altergo
        - 1
        - 1000
        - off
      * - **2**
        - cvc5,z3,altergo
        - 5
        - 1000
        - on
      * - **3**
        - cvc5,z3,altergo
        - 20
        - 2000
        - on
      * - **4**
        - cvc5,z3,altergo
        - 60
        - 2000
        - on

* Level 2 is the recommended one to start (enables counterexamples)

* Levels do not use steps (:command:`--steps=0`) but do increase memory limit

* Specific values for lower-level switches take precedence

  - e.g. :command:`--level=2 --timeout=120 --steps=10000`

----------------------
Running Proof Faster
----------------------

* During development, run :toolname:`GNATprove` on relevant part

  - On given file

    + With :menu:`SPARK` |rightarrow| :menu:`Prove File` in :toolname:`GNAT Studio`
    + With task :menu:`Prove file` in Visual Studio Code
    + With :command:`-u file` in terminal

  - On given subprogram, selected region of code, selected line of code

    + With corresponding menus in IDEs and switches in terminal

* Use parallelism with :command:`-j` e.g. :command:`-j0` for all cores

  - Proof faster on more powerful machines: more cores, more memory, faster
    clock

* Sharing session files by setting attribute :code:`Proof_Dir` in project file

  - This also allows to simply replay proofs with :command:`--replay`

* Sharing proof results via a cache

  - Can store database in a file, or connect to a Memcached server


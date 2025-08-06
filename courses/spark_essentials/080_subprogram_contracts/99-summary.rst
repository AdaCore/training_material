=========
Summary
=========

----------------------
Subprogram Contracts
----------------------

* Functional contracts given by

  - The precondition with aspect :ada:`Pre`
  - The postcondition with aspect :ada:`Post`
  - The contract cases with aspect :ada:`Contract_Cases`
  - The exceptional cases with aspect :ada:`Exceptional_Cases`

* Postcondition may be imprecise

  - In particular, **frame condition** might be missing
  - This may prevent **proof of callers**

* Function contracts may lead to unsoundness

  - If contract is unfeasible
  - If function does not terminate
  - Prove functions **and** their termination!

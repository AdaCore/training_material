============
Exceptions
============

-------------------
Ada.IO_Exceptions
-------------------

* I/O Packages have common exceptions (defined in `Ada.IO_Exceptions` and renamed in `Ada.Text_IO` for easier reference)
* The most common Text I/O exceptions:

   * :ada:`Status_Error` :math:`\rightarrow` Raised on `Open`/`Create` if file being opened/created is already open. For any other operation, raised if file is not open
   * :ada:`Name_Error` :math:`\rightarrow` Raised if filename is invalid for `Open`/`Create`
   * :ada:`Use_Error` :math:`\rightarrow` Raised if unable to `Open`/`Create`
   * :ada:`Data_Error` :math:`\rightarrow` Failure of `Get` to read valid data


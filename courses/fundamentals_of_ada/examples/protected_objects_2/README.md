This is an example of protected objects being used to safely share a
variable between tasks.

Two tasks are created and a protected object is used to share an integer
value between them while performing IO at the same time.

The tasks are performing accesses to the integer value and setting/getting it.

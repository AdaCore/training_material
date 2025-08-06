Examples of `select ... else` calls and entries.

This contains three different mains:

    - `main_no_stop.adb` : No call to stop
    - `main_no_send_message.adb` : No entry to `send_message`
    - `main_no_call.adb` : No call at all is possible to `send_message`
      that's an issue with both client and server using the same construct.

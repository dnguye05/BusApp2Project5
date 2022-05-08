       01  WS-SYS-DATE.
           03  WS-SYS-YR.
               05  WS-SYS-YR-CENTURY       PIC 99.
               05  WS-SYS-YR-DECADE        PIC 99.
           03  WS-SYS-MO                   PIC 99.
           03  WS-SYS-DAY                  PIC 99.
           03  WS-SYS-HR                   PIC 99.
           03  WS-SYS-MIN                  PIC 99.
       01  WS-FMTD-DATE.
           03  WS-FMTD-MO                  PIC 99.
           03  FILLER                      PIC X              VALUE '/'.
           03  WS-FMTD-DAY                 PIC 99.
           03  FILLER                      PIC X              VALUE '/'.
           03  WS-FMTD-YR                  PIC 9999.
       01  WS-FMTD-TIME.
           03  WS-FMTD-HR                  PIC 99.
           03  FILLER                      PIC X              VALUE ':'.
           03  WS-FMTD-MIN                 PIC 99.
      
FUNCTION-POOL zfg_bi_auth.                  "MESSAGE-ID ..

* INCLUDE LZFG_BI_AUTHD...                   " Local class definition
INCLUDE zfmdatasave.

DEFINE fillmsg.
  rtype = &1.
  rtmsg = &2.
  IF rtype NE 'S'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.
END-OF-DEFINITION.

DATA:options TYPE TABLE OF rfc_db_opt.

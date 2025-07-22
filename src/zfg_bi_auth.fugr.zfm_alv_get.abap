FUNCTION zfm_alv_get.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TCODE) TYPE  SY-TCODE
*"  EXPORTING
*"     VALUE(DATA) TYPE REF TO  DATA
*"     VALUE(METADATA) TYPE  CL_SALV_BS_RUNTIME_INFO=>S_TYPE_METADATA
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"  TABLES
*"      SELTAB STRUCTURE  RSPARAMS OPTIONAL
*"----------------------------------------------------------------------

  CLEAR:data,metadata,rtype,rtmsg.
  DATA:pri_params TYPE pri_params.
  IF tcode IS INITIAL.
    rtmsg = 'tcode不能为空'.
    rtype = 'E'.
    RETURN.
  ENDIF.
*  AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TCD' FIELD tcode.
*  IF sy-subrc NE 0.
*    rtmsg = |你没有事务码[{ tcode }]的权限|.
*    rtype = 'E'.
*    RETURN.
*  ENDIF.
  SELECT SINGLE * FROM tstc
    WHERE tcode = @tcode
    INTO @DATA(wa_tstc)
          .
  IF sy-subrc NE 0.
    rtmsg = |tcode:[{ tcode }]不存在|.
    rtype = 'E'.
    RETURN.
  ENDIF.
  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_true data = abap_true ).
  TRY.
      SUBMIT (wa_tstc-pgmna) TO SAP-SPOOL SPOOL PARAMETERS pri_params WITHOUT SPOOL DYNPRO
      WITH SELECTION-TABLE seltab
      AND RETURN
      .
      "获取报表数据
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = data ).
    CATCH cx_root INTO DATA(exc).
      cl_salv_bs_runtime_info=>clear_all( ).
      rtype = 'E'.
      rtmsg = |获取[{ tcode }]的数据发生了异常：{ exc->get_text( ) }|.
      RETURN.
  ENDTRY.
  "如果不展示alv直接获取metadata会发生异常
  TRY.
      "获取字段名及其描述
      metadata = cl_salv_bs_runtime_info=>get_metadata( ).
    CATCH cx_root INTO exc.
  ENDTRY.
  cl_salv_bs_runtime_info=>clear_all( ).
  rtype = 'S'.
  rtmsg = |成功获取[{ tcode }]的数据|.

ENDFUNCTION.

FUNCTION zfm_bi_data_table_get .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TABNAME) TYPE  TABNAME
*"     VALUE(WHERE_JSON) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(OUT_JSON) TYPE  STRING
*"     VALUE(OUT_MAPPING_JSON) TYPE  STRING
*"  EXCEPTIONS
*"      TABLE_NOT_AVAILABLE
*"      TABLE_WITHOUT_DATA
*"      OPTION_NOT_VALID
*"      FIELD_NOT_VALID
*"      NOT_AUTHORIZED
*"      DATA_BUFFER_EXCEEDED
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_BI_DATA_TABLE_GET'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  CLEAR:rtype,rtmsg,out_json,out_mapping_json.
  IF tabname IS INITIAL.
    rtype = 'E'.
    rtmsg = 'tabname不能为空'.
    zfmdatasave2 'R'.
    EXIT.
  ENDIF.
*  调整为逻辑和rfc_read_table类似，添加open sql查询条件  18.04.2024 14:45:59 by kkw
  AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TABLE' FIELD tabname.
*  AUTHORITY-CHECK OBJECT 'ZBI_AUTH_C' ID 'ZE_TABLE' FIELD tabname.
  IF sy-subrc NE 0.
    rtmsg = |你没有底表{ tabname }的权限|.
    fillmsg 'E' rtmsg.
  ENDIF.
  DATA:query_table LIKE  dd02l-tabname,
       delimiter   LIKE  sonv-flag VALUE space,
       no_data     LIKE  sonv-flag VALUE space,
       rowskips    LIKE  soid-accnt VALUE 0,
       rowcount    LIKE  soid-accnt VALUE 0,
       get_sorted  TYPE  boole_d.
  DATA:lo_read TYPE REF TO zcl_rfc_read_table.
  DATA:options TYPE TABLE OF rfc_db_opt,
       fields  TYPE TABLE OF rfc_db_fld.
  TYPES: BEGIN OF t_JSON1,
           wherestr TYPE string,
         END OF t_JSON1.
  DATA:wa_where TYPE t_JSON1.
  DATA:ls_data TYPE REF TO data.
  DATA:BEGIN OF gs_mapping,
         fieldname TYPE lvc_fname,
         scrtext_l TYPE scrtext_l,
         scrtext_m TYPE scrtext_m,
         scrtext_s TYPE scrtext_s,
       END OF gs_mapping,
       gt_mapping LIKE TABLE OF gs_mapping.
  query_table = tabname.
*  赋值where查询条件  18.04.2024 14:55:36 by kkw
  IF where_json IS NOT INITIAL.
    /ui2/cl_json=>deserialize( EXPORTING json = where_json  pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = wa_where ).
    SPLIT wa_where-wherestr AT space INTO TABLE options.
  ENDIF.

  TRY.
      CREATE OBJECT lo_read
        EXPORTING
          id_table           = query_table
          id_block_list_tabl = 'RFC_READ_TABLE_TABL'
          id_block_list_call = 'RFC_READ_TABLE_CALL'
          id_delim           = delimiter
          id_no_data         = no_data
          id_sort            = get_sorted.
      CLEAR:fields.

      lo_read->get_table_dref( EXPORTING id_skip_rows = rowskips
                                         id_max_row   = rowcount
                                         it_fields    = fields[]
                                         it_selopt    = options[]
                               IMPORTING et_data      = ls_data
                                         et_fields    = fields[] ).



    CATCH cx_sais_raise_events.
      IF ( sy-msgno = '131' AND sy-msgid = 'DA' ).
        rtmsg = |table_not_available|.
        rtype = 'E'.
        zfmdatasave2 'R'.
        RAISE table_not_available.
      ELSEIF ( sy-msgno = '105' AND sy-msgid = 'TB' ) OR ( sy-msgno = '072' AND sy-msgid = 'SAIS' ).
        rtmsg = |not_authorized|.
        rtype = 'E'.
        zfmdatasave2 'R'.
        RAISE not_authorized.
      ELSEIF ( sy-msgno = '718' AND sy-msgid = 'AD' ).
        rtmsg = |table_without_data|.
        rtype = 'E'.
        zfmdatasave2 'R'.
        RAISE table_without_data.
      ELSEIF ( sy-msgno = '559' AND sy-msgid = 'AD' ).
        rtmsg = |data_buffer_exceeded|.
        rtype = 'E'.
        zfmdatasave2 'R'.
        RAISE data_buffer_exceeded.
      ELSEIF  ( sy-msgno = '000' AND sy-msgid = 'SAIS' ).
        rtmsg = |option_not_valid|.
        rtype = 'E'.
        zfmdatasave2 'R'.
        RAISE option_not_valid.
      ENDIF.

  ENDTRY.
  IF ls_data IS BOUND.
    ASSIGN ls_data->* TO FIELD-SYMBOL(<tab>).
    rtype = 'S'.
    IF <tab> IS NOT INITIAL.
      rtmsg = |成功获取{ tabname }的数据|.
      out_json = /ui2/cl_json=>serialize( data = <tab> compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ELSE.
      rtmsg = |{ tabname }空数据|.
    ENDIF.
    LOOP AT fields ASSIGNING FIELD-SYMBOL(<dfies_tab>).
      INSERT INITIAL LINE INTO TABLE gt_mapping ASSIGNING FIELD-SYMBOL(<gt_mapping>).
      <gt_mapping>-fieldname = to_lower( <dfies_tab>-fieldname ).
      <gt_mapping>-scrtext_l = <dfies_tab>-fieldtext.
      <gt_mapping>-scrtext_m = <dfies_tab>-fieldtext.
      <gt_mapping>-scrtext_s = <dfies_tab>-fieldtext.
    ENDLOOP.
    out_mapping_json = /ui2/cl_json=>serialize( data = gt_mapping  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ELSE.
    rtmsg = |表名{ tabname }创建的对象未被有效引用|.
    fillmsg 'E' rtmsg.
  ENDIF.

  zfmdatasave2 'R'.

ENDFUNCTION.

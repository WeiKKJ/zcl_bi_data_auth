FUNCTION zfm_bi_data_table_get .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TABNAME) TYPE  TABNAME
*"     VALUE(WHERESTR) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(OUT_JSON) TYPE  STRING
*"     VALUE(OUT_MAPPING_JSON) TYPE  STRING
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_BI_DATA_TABLE_GET'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  IF tabname IS INITIAL.
    rtype = 'E'.
    rtmsg = 'tabname不能为空'.
    zfmdatasave2 'R'.
    EXIT.
  ENDIF.
  DATA:ls_data   TYPE REF TO data,
       dfies_tab TYPE TABLE OF dfies,
       tablename TYPE ddobjname,
       BEGIN OF gs_mapping,
         fieldname TYPE lvc_fname,
         scrtext_l TYPE scrtext_l,
         scrtext_m TYPE scrtext_m,
         scrtext_s TYPE scrtext_s,
       END OF gs_mapping,
       gt_mapping LIKE TABLE OF gs_mapping.
  FIELD-SYMBOLS:<tab> TYPE STANDARD TABLE.
  DATA:ld_max_row TYPE sy-dbcnt.

  AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TABLE' FIELD tabname.
*  AUTHORITY-CHECK OBJECT 'ZBI_AUTH_C' ID 'ZE_TABLE' FIELD tabname.
  IF sy-subrc NE 0.
    rtmsg = |你没有底表{ tabname }的权限|.
    fillmsg 'E' rtmsg.
  ENDIF.

  CLEAR:out_json.
  SELECT COUNT(*) FROM dd02l WHERE tabname = @tabname AND as4local = 'A'.
  IF sy-subrc NE 0.
    rtmsg = |表名{ tabname }不存在或处于未激活状态|.
    fillmsg 'E' rtmsg.
  ENDIF.
  CLEAR:ls_data.
  UNASSIGN:<tab>.
  CREATE DATA ls_data TYPE STANDARD TABLE OF (tabname).
  IF ls_data IS BOUND.
    ASSIGN ls_data->* TO <tab>.
  ELSE.
    rtmsg = |表名{ tabname }创建的对象未被有效引用|.
    fillmsg 'E' rtmsg.
  ENDIF.

  IF <tab> IS ASSIGNED.
    TRY .
        SELECT * FROM (tabname)
          WHERE (wherestr)
          ORDER BY PRIMARY KEY
          INTO CORRESPONDING FIELDS OF TABLE @<tab>.
      CATCH: cx_sy_dynamic_osql_semantics,cx_sy_dynamic_osql_syntax,cx_sy_open_sql_db.
        rtmsg = |表名{ tabname }查询出错|.
        fillmsg 'E' rtmsg.
    ENDTRY.
    rtype = 'S'.
    IF <tab> IS NOT INITIAL.
      rtmsg = |成功获取{ tabname }的数据|.
      out_json = /ui2/cl_json=>serialize( data = <tab>  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
      " 添加底表字段映射信息  13.03.2024 22:34:58 by kkw
      tablename = tabname.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = tablename
          langu          = sy-langu
        TABLES
          dfies_tab      = dfies_tab
        EXCEPTIONS
          not_found      = 1
          internal_error = 2
          OTHERS         = 3.
      LOOP AT dfies_tab ASSIGNING FIELD-SYMBOL(<dfies_tab>).
        INSERT INITIAL LINE INTO TABLE gt_mapping ASSIGNING FIELD-SYMBOL(<gt_mapping>).
        <gt_mapping>-fieldname = to_lower( <dfies_tab>-fieldname ).
        <gt_mapping>-scrtext_l = <dfies_tab>-fieldtext.
        <gt_mapping>-scrtext_m = <dfies_tab>-scrtext_m.
        <gt_mapping>-scrtext_s = <dfies_tab>-scrtext_s.
      ENDLOOP.
      out_mapping_json = /ui2/cl_json=>serialize( data = gt_mapping  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ELSE.
      rtmsg = |{ tabname }空数据|.
    ENDIF.
    UNASSIGN <tab>.
  ELSE.
    rtmsg = |表名{ tabname }所属内表未被成功创建|.
    fillmsg 'E' rtmsg.
  ENDIF.

  zfmdatasave2 'R'.
ENDFUNCTION.

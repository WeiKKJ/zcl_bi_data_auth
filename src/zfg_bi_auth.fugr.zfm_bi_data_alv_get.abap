FUNCTION zfm_bi_data_alv_get.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TCODE) TYPE  SY-TCODE
*"     VALUE(IN_JSON) TYPE  STRING OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(OUT_JSON) TYPE  STRING
*"     VALUE(OUT_MAPPING_JSON) TYPE  STRING
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_BI_DATA_ALV_GET'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  DATA:ls_data         TYPE REF TO data,
       seltab          TYPE TABLE OF rsparams,
       pri_params      TYPE pri_params,
       gt_rsparams     TYPE TABLE OF rsparams,
       gt_rsparams_255 TYPE TABLE OF rsparamsl_255,
       sign_range      TYPE RANGE OF rsparams-sign,
       option_range    TYPE RANGE OF rsparams-option,
       oexcp           TYPE REF TO cx_root,
       etext           TYPE string.
  DATA:BEGIN OF gs_mapping,
         fieldname TYPE lvc_fname,
         scrtext_l TYPE scrtext_l,
         scrtext_m TYPE scrtext_m,
         scrtext_s TYPE scrtext_s,
       END OF gs_mapping,
       gt_mapping LIKE TABLE OF gs_mapping.
  CLEAR:rtype,rtmsg,out_json,out_mapping_json.
  IF tcode IS INITIAL.
    rtmsg = 'tcode不能为空'.
    fillmsg 'E' rtmsg.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TCD' FIELD tcode.
*  AUTHORITY-CHECK OBJECT 'ZBI_AUTH_C' ID 'ZE_TCODE' FIELD tcode.
  IF sy-subrc NE 0.
    rtmsg = |你没有事务码{ tcode }的权限|.
    fillmsg 'E' rtmsg.
  ENDIF.
  SELECT SINGLE * FROM tstc
    WHERE tcode = @tcode
    INTO @DATA(wa_tstc)
          .
  IF sy-subrc NE 0.
    rtmsg = |tcode:{ tcode }不存在|.
    fillmsg 'E' rtmsg.
  ENDIF.
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = wa_tstc-pgmna
* IMPORTING
*     SP              =
    TABLES
      selection_table = gt_rsparams
*     selection_table_255 = gt_rsparams_255
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    rtmsg = |tcode:{ tcode }获取选择屏幕参数出现问题|.
    fillmsg 'E' rtmsg.
  ENDIF.
  sign_range = VALUE #( sign = 'I' option = 'EQ'
  ( low = 'I' )
  ( low = 'E' )
    ).
  option_range = VALUE #( sign = 'I' option = 'EQ'
  ( low = 'EQ' )
  ( low = 'NE' )
  ( low = 'GT' )
  ( low = 'LT' )
  ( low = 'GE' )
  ( low = 'LE' )
  ( low = 'CP' )
  ( low = 'NP' )
  ( low = 'BT' )
  ( low = 'NB' )
    ).

  CLEAR seltab.
  /ui2/cl_json=>deserialize( EXPORTING json = in_json CHANGING data = seltab ).
  " BODY传递了内容但是匹配不到内表不允许往下执行了  16.04.2024 18:18:10 by kkw
  DELETE seltab WHERE selname IS INITIAL.
  IF in_json IS NOT INITIAL AND seltab IS INITIAL.
    rtmsg = 'BODY内容无法匹配选择屏幕参数'.
    fillmsg 'E' rtmsg.
  ENDIF.

  "校验屏幕参数
  LOOP AT seltab ASSIGNING FIELD-SYMBOL(<seltab>) GROUP BY ( selname = <seltab>-selname kind = <seltab>-kind
    index = GROUP INDEX size = GROUP SIZE
    ) ASSIGNING FIELD-SYMBOL(<group>).
    READ TABLE gt_rsparams ASSIGNING FIELD-SYMBOL(<gt_rsparams>) WITH KEY selname = <group>-selname kind = <group>-kind.
    IF sy-subrc NE 0.
      rtmsg = |tcode:{ tcode }选择屏幕参数:{ <group>-selname }，类型:{ <group>-kind }，不存在'|.
      fillmsg 'E' rtmsg.
    ELSE.
      IF <group>-kind = 'P' AND <group>-size NE 1.
        rtmsg = |tcode:{ tcode }选择屏幕参数:{ <group>-selname }，类型:{ <group>-kind }，不能赋值{ <group>-size }次|.
        fillmsg 'E' rtmsg.
      ENDIF.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
        "判断sign和option赋值是否正确
        IF <mem>-sign NOT IN  sign_range.
          rtmsg = |tcode:{ tcode }选择屏幕参数:{ <group>-selname }的SIGN值:{ <mem>-sign }配置不正确|.
          fillmsg 'E' rtmsg.
        ENDIF.
        IF <mem>-option NOT IN option_range.
          rtmsg = |tcode:{ tcode }选择屏幕参数:{ <group>-selname }的OPTION值:{ <mem>-option }配置不正确|.
          fillmsg 'E' rtmsg.
        ENDIF.
        "判断low和high赋值是否正确
        CASE <group>-kind.
          WHEN 'P'.
            CLEAR:<mem>-high.
          WHEN 'S'.
            IF <mem>-high IS NOT INITIAL AND <mem>-low IS NOT INITIAL.
              IF <mem>-high LT <mem>-low.
                rtmsg = |tcode:{ tcode }选择屏幕参数:{ <group>-selname }的上限值:{ <mem>-high }不能小于下限值:{ <mem>-low }|.
                fillmsg 'E' rtmsg.
              ENDIF.
            ELSEIF <mem>-high IS NOT INITIAL AND <mem>-low IS INITIAL.

            ELSEIF <mem>-high IS INITIAL AND  <mem>-low IS NOT INITIAL.

            ELSE.

            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_true data = abap_true ).
  SUBMIT (wa_tstc-pgmna) TO SAP-SPOOL SPOOL PARAMETERS pri_params WITHOUT SPOOL DYNPRO
  WITH SELECTION-TABLE seltab
  AND RETURN
    .
  TRY.
      DATA(gget) = cl_salv_bs_runtime_info=>get( ).
    CATCH cx_salv_bs_sc_runtime_info.

  ENDTRY.
  "获取报表数据
  TRY .
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      cl_salv_bs_runtime_info=>clear_all( ).
      rtype = 'E'.
      rtmsg = |无法获取{ tcode }的数据|.
      zfmdatasave2 'R'.
      EXIT.
  ENDTRY.
  "获取字段名及其描述
  TRY .
      DATA(metadata) = cl_salv_bs_runtime_info=>get_metadata( ).
    CATCH cx_salv_bs_sc_runtime_info.
      cl_salv_bs_runtime_info=>clear_all( ).
  ENDTRY.
  cl_salv_bs_runtime_info=>clear_all( ).
  ASSIGN ls_data->* TO FIELD-SYMBOL(<fs_tab>).
  IF metadata-t_fcat IS NOT INITIAL.
    LOOP AT metadata-t_fcat ASSIGNING FIELD-SYMBOL(<dfies_tab>).
      INSERT INITIAL LINE INTO TABLE gt_mapping ASSIGNING FIELD-SYMBOL(<gt_mapping>).
      <gt_mapping>-fieldname = to_lower( <dfies_tab>-fieldname ).
      <gt_mapping>-scrtext_l = <dfies_tab>-scrtext_l.
      IF <gt_mapping>-scrtext_l IS INITIAL.
        <gt_mapping>-scrtext_l = <dfies_tab>-seltext.
      ENDIF.
      <gt_mapping>-scrtext_m = <dfies_tab>-scrtext_m.
      <gt_mapping>-scrtext_s = <dfies_tab>-scrtext_s.
    ENDLOOP.
    out_mapping_json = /ui2/cl_json=>serialize( data = gt_mapping  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
  ENDIF.

  IF <fs_tab> IS ASSIGNED.
    rtype = 'S'.
    IF <fs_tab> IS NOT INITIAL.
      rtmsg = |成功获取{ tcode }的数据|.
      out_json = /ui2/cl_json=>serialize( data = <fs_tab>  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    ELSE.
      rtmsg = |{ tcode }空数据|.
    ENDIF.
    UNASSIGN <fs_tab>.
  ELSE.
    rtype = 'S'.
    rtmsg = |{ tcode }无数据|.
  ENDIF.

  zfmdatasave2 'R'.
ENDFUNCTION.

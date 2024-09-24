CLASS zcl_bi_data_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    CLASS-METHODS exec_rfc
      IMPORTING
        VALUE(funcname)  TYPE tfdir-funcname
        VALUE(interface) TYPE string
      EXPORTING
        VALUE(out_json)  TYPE string
        VALUE(rtype)     TYPE bapi_mtype
        VALUE(rtmsg)     TYPE bapi_msg .
    CLASS-METHODS exec_rfc_crtdata
      IMPORTING
        VALUE(funcname)  TYPE tfdir-funcname
        VALUE(interface) TYPE string
      EXPORTING
        VALUE(out_json)  TYPE string
        VALUE(rtype)     TYPE bapi_mtype
        VALUE(rtmsg)     TYPE bapi_msg .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA my_service TYPE string .
    DATA my_url TYPE string .
    DATA my_params TYPE tihttpnvp .
    DATA:
      datatypescont TYPE TABLE OF rfc_metadata_ddic .

    METHODS get_rsparams
      IMPORTING
        VALUE(tcode)    TYPE sy-tcode
      RETURNING
        VALUE(rsparams) TYPE string
      EXCEPTIONS
        tcode_not_found .
    METHODS get_interface
      IMPORTING
        VALUE(tabname)   TYPE tabname
        VALUE(fieldname) TYPE fieldname OPTIONAL
      RETURNING
        VALUE(out_json)  TYPE string .
    METHODS get_query
      IMPORTING
        VALUE(query_string) TYPE string
        VALUE(key)          TYPE string
      EXPORTING
        VALUE(value)        TYPE string .
    METHODS get_params
      IMPORTING
        VALUE(params)    TYPE string
      RETURNING
        VALUE(my_params) TYPE tihttpnvp .
    METHODS notes
      RETURNING
        VALUE(text) TYPE string .
    METHODS notes2
      RETURNING
        VALUE(text) TYPE string .
ENDCLASS.



CLASS ZCL_BI_DATA_AUTH IMPLEMENTATION.


  METHOD get_params.
    DATA:lt_kv_tab TYPE TABLE OF string,
         wa_kv     TYPE ihttpnvp.
    CLEAR:lt_kv_tab,wa_kv,my_params.
    SPLIT params AT '&' INTO TABLE lt_kv_tab.
    LOOP AT lt_kv_tab ASSIGNING FIELD-SYMBOL(<lt_kv_tab>).
      CLEAR wa_kv.
      SPLIT <lt_kv_tab> AT '=' INTO wa_kv-name wa_kv-value.
      APPEND wa_kv TO my_params.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_query.
    DATA:lt_kv_tab TYPE TABLE OF string,
         BEGIN OF wa_kv,
           key   TYPE string,
           value TYPE string,
         END OF wa_kv,
         lt_kv LIKE TABLE OF wa_kv.
    CLEAR:lt_kv_tab,wa_kv,lt_kv,value.
    SPLIT query_string AT '&' INTO TABLE lt_kv_tab.

    LOOP AT lt_kv_tab ASSIGNING FIELD-SYMBOL(<lt_kv_tab>).
      CLEAR wa_kv.
      SPLIT <lt_kv_tab> AT '=' INTO wa_kv-key wa_kv-value.
      APPEND wa_kv TO lt_kv.
    ENDLOOP.
    READ TABLE lt_kv INTO wa_kv WITH KEY key = key.
    IF sy-subrc EQ 0.
      value = wa_kv-value.
    ENDIF.
  ENDMETHOD.


  METHOD get_rsparams.
    DATA: gt_rsparams TYPE TABLE OF rsparams.
    SELECT SINGLE * FROM tstc
      WHERE tcode = @tcode
      INTO @DATA(wa_tstc)
            .
    IF sy-subrc NE 0.
      RAISE tcode_not_found.
    ENDIF.
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = wa_tstc-pgmna
* IMPORTING
*       SP              =
      TABLES
        selection_table = gt_rsparams
*       selection_table_255 = gt_rsparams_255
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc NE 0.
      RAISE tcode_not_found.
    ENDIF.
    rsparams = /ui2/cl_json=>serialize( data = gt_rsparams  compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
********************************************************************************
* The KKW License (KKW)
*
* Copyright (c) 2024 kkw
*
********************************************************************************

    DATA:lt_header        TYPE tihttpnvp,
         json             TYPE string,
         out_mapping_json TYPE string,
         rtype            TYPE bapi_mtype,
         rtmsg            TYPE bapi_msg,
         tcode            TYPE sy-tcode,
         tabname          TYPE tabname,
         funcname         TYPE rs38l_fnam,
         proto            TYPE string,
         host             TYPE string,
         port             TYPE string,
         action           TYPE string.
    DATA:functionnames   TYPE TABLE OF rfcfunctionname,
         datatypes       TYPE TABLE OF rfc_md_ddic_name,
         known_datatypes TYPE TABLE OF rfc_md_ddic_name,
         parameters      TYPE TABLE OF rfc_metadata_params,
*         datatypescont   TYPE TABLE OF rfc_metadata_ddic,
         indirecttypes   TYPE TABLE OF rfc_metadata_ddic_indirect,
         func_errors     TYPE TABLE OF rfc_func_error,
         dd_errors       TYPE TABLE OF rfc_dd_error.
    DATA:json_meta      TYPE string,
         json_meta_full TYPE string.
    " 1xx：信息响应类，表示接收到请求并且继续处理
    " 2xx：处理成功响应类，表示动作被成功接收、理解和接受
    " 3xx：重定向响应类，为了完成指定的动作，必须接受进一步处理
    " 4xx：客户端错误，客户请求包含语法错误或者是不能正确执行
    " 5xx：服务端错误，服务器不能正确执行一个正确的请求
    "
    " 100——客户必须继续发出请求
    " 101——客户要求服务器根据请求转换HTTP协议版本
    "
    " 200——交易成功
    " 201——提示知道新文件的URL
    " 202——接受和处理、但处理未完成
    " 203——返回信息不确定或不完整
    " 204——请求收到，但返回信息为空
    " 205——服务器完成了请求，用户代理必须复位当前已经浏览过的文件
    " 206——服务器已经完成了部分用户的GET请求
    "
    " 300——请求的资源可在多处得到
    " 301——删除请求数据
    " 302——在其他地址发现了请求数据
    " 303——建议客户访问其他URL或访问方式
    " 304——客户端已经执行了GET，但文件未变化
    " 305——请求的资源必须从服务器指定的地址得到
    " 306——前一版本HTTP中使用的代码，现行版本中不再使用
    " 307——申明请求的资源临时性删除
    "
    " 400——错误请求，如语法错误
    " 401——请求授权失败
    " 402——保留有效ChargeTo头响应
    " 403——请求不允许
    " 404——没有发现文件、查询或URl
    " 405——用户在Request-Line字段定义的方法不允许
    " 406——根据用户发送的Accept拖，请求资源不可访问
    " 407——类似401，用户必须首先在代理服务器上得到授权
    " 408——客户端没有在用户指定的饿时间内完成请求
    " 409——对当前资源状态，请求不能完成
    " 410——服务器上不再有此资源且无进一步的参考地址
    " 411——服务器拒绝用户定义的Content-Length属性请求
    " 412——一个或多个请求头字段在当前请求中错误
    " 413——请求的资源大于服务器允许的大小
    " 414——请求的资源URL长于服务器允许的长度
    " 415——请求资源不支持请求项目格式
    " 416——请求中包含Range请求头字段，在当前请求资源范围内没有range指示值，请求也不包含If-Range请求头字段
    " 417——服务器不满足请求Expect头字段指定的期望值，如果是代理服务器，可能是下一级服务器不能满足请求
    "
    " 500——服务器产生内部错误
    " 501——服务器不支持请求的函数
    " 502——服务器暂时不可用，有时是为了防止发生系统过载
    " 503——服务器过载或暂停维修
    " 504——关口过载，服务器使用另一个关口或服务来响应用户，等待时间设定值较长
    " 505——服务器不支持或拒绝支请求头中指定的HTTP版本

*返回权限检查消息
    DEFINE http_msg.
      "   &1   消息类型
      "   &2   http status code
      "   &3   status text
      "   &4   rtypw
      "   &5   rtmsg
      "   &6   mapping
      "   &7   data
      "   &8   rsparams
      server->response->set_header_field( name = 'Content-Type' value = 'application/json;charset=utf-8' ).
*      server->response->set_status( code = &2  reason = &3 ).
      "不知道为啥有的环境设置非200的http status code就不能正常返回消息
      IF &2 IS NOT INITIAL.
        server->response->set_status( code = 200  reason = CONV string( &3 ) ).
      ENDIF.
      CASE &1.
        WHEN 'DATA'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `", "mapping":` &6 `, "data":` &7 `}` INTO json.
        WHEN 'AUTH'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `"}` INTO json.
        WHEN 'RSPARAMS'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `", "rsparams":` &8 `}` INTO json.
        WHEN 'INTERFACE'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `", "interface":` &8 `}` INTO json.
      ENDCASE.
      server->response->set_cdata( EXPORTING data   = json ).
      RETURN.
    END-OF-DEFINITION.

    CLEAR:lt_header,json,tcode,tabname,out_mapping_json.
*获取服务器接口信息
    server->get_location( IMPORTING host = host port = port out_protocol = proto ).
    CONCATENATE proto '://' host ':' port INTO me->my_url.

*获取header信息
    server->request->get_header_fields( CHANGING fields = lt_header ).
    READ TABLE lt_header INTO DATA(wa_header) WITH KEY name = '~script_name' .
    IF sy-subrc EQ 0.
      me->my_service = wa_header-value.
    ENDIF.

    CLEAR wa_header.
    READ TABLE lt_header INTO wa_header WITH KEY name = '~request_method' .
    CASE wa_header-value.
      WHEN 'GET'.
        server->response->set_header_field( name = 'Content-Type'  value = 'text/html' ).
        server->response->set_status( code = 200 reason = 'OK' ).
        json = me->notes2( ).
        server->response->set_cdata( json ).
      WHEN 'POST'.
        READ TABLE lt_header INTO wa_header WITH KEY name = '~query_string' .
        IF sy-subrc NE 0.
          http_msg 'DATA' 404 'Missing Params' 'E' '缺少Params' '[]' '[]' ''.
        ENDIF.
*获取Params，取报表还是取底表
        my_params = me->get_params( EXPORTING params = wa_header-value ).
        READ TABLE my_params INTO DATA(my_param) WITH KEY name = 'tcode' .
        IF sy-subrc EQ 0.
          tcode = to_upper( my_param-value ).
        ENDIF.
        READ TABLE my_params INTO my_param WITH KEY name = 'tabname' .
        IF sy-subrc EQ 0.
          tabname = to_upper( my_param-value ).
        ENDIF.
        READ TABLE my_params INTO my_param WITH KEY name = 'funcname' .
        IF sy-subrc EQ 0.
          funcname = to_upper( my_param-value ).
        ENDIF.
        IF tcode IS NOT INITIAL."报表相关
          AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TCD' FIELD tcode.
          IF sy-subrc NE 0.
            rtmsg = |你没有事务码[{ tcode }]的权限|.
            http_msg 'AUTH' 403 'Not authorized' 'E' rtmsg '' '' ''.
          ENDIF.
          action = server->request->get_form_field( 'action' ).
          IF to_lower( action ) = 'rsparams'.
            CALL METHOD me->get_rsparams
              EXPORTING
                tcode           = tcode
              RECEIVING
                rsparams        = DATA(rsparams)
              EXCEPTIONS
                tcode_not_found = 1
                OTHERS          = 2.
            IF sy-subrc <> 0.
              rtmsg = |事务码[{ tcode }]不存在|.
              http_msg 'RSPARAMS' 404 'Tcode not found' 'E' rtmsg '' '' '[]'.
            ELSE.
              rtmsg = |成功获取事务码[{ tcode }]的选择屏幕参数|.
              http_msg 'RSPARAMS' 200 'OK' 'S' rtmsg '' '' rsparams.
            ENDIF.
          ELSE.
            json = server->request->if_http_entity~get_cdata( ).
            CALL FUNCTION 'ZFM_BI_DATA_ALV_GET'
              EXPORTING
                tcode            = tcode
                in_json          = json
              IMPORTING
                rtype            = rtype
                rtmsg            = rtmsg
                out_json         = json
                out_mapping_json = out_mapping_json.
          ENDIF.
        ELSEIF tabname IS NOT INITIAL."底表相关
          AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TABLE' FIELD tabname.
          IF sy-subrc NE 0.
            rtmsg = |你没有底表[{ tabname }]的权限|.
            http_msg 'AUTH' 403 'Not authorized' 'E' rtmsg '' '' ''.
          ENDIF.
          json = server->request->if_http_entity~get_cdata( ).
          CALL FUNCTION 'ZFM_BI_DATA_TABLE_GET'
            EXPORTING
              tabname              = tabname
              where_json           = json
            IMPORTING
              rtype                = rtype
              rtmsg                = rtmsg
              out_json             = json
              out_mapping_json     = out_mapping_json
            EXCEPTIONS
              table_not_available  = 1
              table_without_data   = 2
              option_not_valid     = 3
              field_not_valid      = 4
              not_authorized       = 5
              data_buffer_exceeded = 6
              OTHERS               = 7.
          IF sy-subrc NE 0.
            CLEAR:json,out_mapping_json.
            rtype = 'E'.
            CASE sy-subrc.
              WHEN 1.
                rtmsg = |table_not_available|.
              WHEN 2.
                rtmsg = |table_without_data|.
              WHEN 3.
                rtmsg = |option_not_valid|.
              WHEN 4.
                rtmsg = |field_not_valid|.
              WHEN 5.
                rtmsg = |not_authorized|.
              WHEN 6.
                rtmsg = |data_buffer_exceeded|.
              WHEN 7.
                rtmsg = |other error|.
            ENDCASE.
          ENDIF.
        ELSEIF funcname IS NOT INITIAL."函数模块相关
*          rtmsg = |该方法尚在开发中|.
*          http_msg 'INTERFACE' 400 'method not active' 'E' rtmsg '' '' '[]'.
          AUTHORITY-CHECK OBJECT 'ZBI_AUTH_F' ID 'RFC_NAME' FIELD funcname.
          IF sy-subrc NE 0.
            rtmsg = |你没有函数模块[{ funcname }]的权限|.
            http_msg 'AUTH' 403 'Not authorized' 'E' rtmsg '' '' ''.
          ENDIF.
          " 添加对RFC的HTTP调用  06.09.2024 20:05:10 by kkw
          INSERT INITIAL LINE INTO TABLE functionnames ASSIGNING FIELD-SYMBOL(<functionnames>).
          <functionnames>-functionname = to_upper( funcname ).
          CALL FUNCTION 'RFC_METADATA_GET'
            EXPORTING
              deep            = 'X'
*             LANGUAGE        = SY-LANGU
*             GET_CLIENT_DEP_FIELDS             =
*             GET_TIMESTAMPS  =
*             EVALUATE_LINKS  =
*             DO_NOT_RESOLVE_SIMPLE_TYPES       =
            TABLES
              functionnames   = functionnames
              datatypes       = datatypes
              known_datatypes = known_datatypes
              parameters      = parameters
              datatypescont   = datatypescont
              indirecttypes   = indirecttypes
              func_errors     = func_errors
              dd_errors       = dd_errors
            EXCEPTIONS
              invalid_mode    = 1
              internal_error  = 2
              OTHERS          = 3.
          IF sy-subrc <> 0.
            rtmsg = |获取函数模块[{ funcname }]信息发生了异常，异常码[{ sy-subrc }]|.
            http_msg 'INTERFACE' 404 'get funcname info error' 'E' rtmsg '' '' '[]'.
          ELSEIF func_errors IS NOT INITIAL.
            READ TABLE func_errors ASSIGNING FIELD-SYMBOL(<func_errors>) INDEX 1.
            rtmsg = |获取函数模块[{ funcname }]信息发生了异常，[{ <func_errors>-exception }]，[{ <func_errors>-exception_text }]|.
            http_msg 'INTERFACE' 404 <func_errors>-exception 'E' rtmsg '' '' '[]'.
          ENDIF.

          action = server->request->get_form_field( 'action' ).
          IF to_lower( action ) = 'interface'.
            CLEAR json_meta_full.
            json_meta_full = `{`.
            LOOP AT parameters ASSIGNING FIELD-SYMBOL(<parameters>) GROUP BY ( paramclass = <parameters>-paramclass
              size = GROUP SIZE index = GROUP INDEX
              ) ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
              json_meta_full = json_meta_full && |"{ <group>-paramclass }":| && `{`.
              LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
                CLEAR json_meta.
                CALL METHOD me->get_interface
                  EXPORTING
                    tabname   = <mem>-tabname
                    fieldname = <mem>-fieldname
                  RECEIVING
                    out_json  = json_meta.

                CASE <mem>-paramclass.
                  WHEN 'T'.
                    json_meta = |"{ <mem>-parameter }":[| && `{` && |{ json_meta }| && `}]`.
                  WHEN OTHERS.
                    CASE <mem>-exid.
                      WHEN 'h'.
                        json_meta = |"{ <mem>-parameter }":[| && `{` && |{ json_meta }| && `}]`.
                      WHEN 'v' OR 'u'.
                        json_meta = |"{ <mem>-parameter }":| && `{` && |{ json_meta }| && `}`.
                      WHEN OTHERS.
                        json_meta = |"{ <mem>-parameter }":"{ <mem>-paramtext }"|.
                    ENDCASE.
                ENDCASE.
                json_meta_full = json_meta_full && json_meta && ',' .
              ENDLOOP.
              SHIFT json_meta_full RIGHT DELETING TRAILING ','.
              json_meta_full = json_meta_full && `},`.
            ENDLOOP.
            SHIFT json_meta_full RIGHT DELETING TRAILING ','.
            json_meta_full = json_meta_full && `}` .
*            CONDENSE json_meta_full NO-GAPS.
            rtmsg = |成功获取函数模块[{ funcname }]的参数|.
            http_msg 'INTERFACE' 200 'OK' 'S' rtmsg '' '' json_meta_full.
          ELSE.
            json = server->request->if_http_entity~get_cdata( ).
            " 添加对入参的日志记录  24.09.2024 10:00:04 by kkw
            CALL METHOD zcl_dingtalk_callback=>add_log
              EXPORTING
                name       = funcname
                eventtype  = CONV rs38l_par_( 'HTTP2RFC' )
                detail_ori = json
*               detail     = res
                secds      = 0.
            CALL METHOD me->exec_rfc_crtdata
              EXPORTING
                funcname  = funcname
                interface = json
              IMPORTING
                out_json  = DATA(out_json)
                rtype     = rtype
                rtmsg     = rtmsg.
            http_msg 'INTERFACE' 200 'OK' rtype rtmsg '' '' out_json.
          ENDIF.
        ELSE.
          http_msg 'DATA' 412 'Params error' 'E' 'Params键只能是tcode或tabname或funcname' '[]' '[]' ''.
        ENDIF.
        IF json IS INITIAL.
          json = '[]'.
        ENDIF.
        IF out_mapping_json IS INITIAL.
          out_mapping_json = '[]'.
        ENDIF.
        IF rtype = 'S'.
          http_msg 'DATA' 200 'OK' rtype rtmsg out_mapping_json json ''.
        ELSE.
          http_msg 'DATA' 400 'ERROR' rtype rtmsg out_mapping_json json ''.
        ENDIF.
      WHEN OTHERS.
        http_msg 'DATA' 403 'Request method not allowed' 'E' '方法只允许GET或者POST' '[]' '[]' ''.
    ENDCASE.
  ENDMETHOD.


  METHOD notes.
    CLEAR text.
    CONCATENATE
`<!DOCTYPE html>`
`<html>`
``
`<head>`
`  <meta charset="utf-8">`
`  <meta name="viewport" content="width=device-width, initial-scale=1.0">`
`  <title>ZCL_BI_DATA_AUTH</title>`
`  <link rel="stylesheet" href="https://stackedit.io/style.css" />`
`</head>`
``
`<body class="stackedit">`
`  <div class="stackedit__html"><h1 id="关于这项服务的使用说明">关于这项服务的使用说明</h1>`
`<p>这是一个获取SAP的ALV报表和底表数据的HTTP接口服务。</p>`
`<p>它以 ICF 服务的形式提供此接口，在此系统中，此服务已分配给 ICF 服务<a href="` me->my_url me->my_service `?sap-client=` sy-mandt `" title="调用地址">` me->my_service `</a>。</p>`
`<h2 id="权限检查">权限检查</h2>`
`<p>该服务包含一个对名为 ZBI_AUTH 的自定义授权对象的 AUTHORITY-CHECK OBJECT调用，用于验证用户是否可以访问事务码或者底表,需要为每个访问该服务的用户都创建一个单独的角色，其中只有允许他访问的功能。<br>`
`<strong>权限对象</strong>：<br>`
`<img src="https://s21.ax1x.com/2024/03/16/pF2QXjg.jpg" alt="pF2QXjg.jpg"><br>`
`<strong>权限角色</strong>：<br>`
`<img src="https://s21.ax1x.com/2024/03/16/pF2lLI1.jpg" alt="pF2lLI1.jpg"></p>`
`<p>该服务提供了三类方法：获取ALV报表选择屏幕参数、获取ALV报表数据、获取底表数据，具体请求示例请查看下面的详细介绍。</p>`
`<h2 id="获取alv报表选择屏幕参数">获取ALV报表选择屏幕参数</h2>`
`<p>请求地址：<a href="` me->my_url me->my_service `?sap-client=` sy-mandt `&tcode=%5Btcode%5D%5D" title="获取ALV报表选择屏幕参数">` me->my_url me->my_service `?sap-client=` sy-mandt `&tcode=[tcode]</a><br>`
`请求方法：POST<br>`
`Content-Type: "application/x-www-form-urlencoded"<br>`
`请求体: action: "rsparams"<br>`
`返回消息结构：</p>`
`<pre><code>{`
`	"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`	"rtmsg": "消息文本",`
`	"rsparams": [`
`   {`
`     "selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",`
`     "kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",`
`     "sign": "ABAP/4: ID: I/E (包括/不包括值)",`
`     "option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",`
`     "low": "ABAP：选择值（LOW 值，外部格式）",`
`     "high": "ABAP：选择值（HIGH 值，外部格式）"`
`   }`
`	]`
`}`
`</code></pre>`
`<h2 id="获取alv报表数据">获取ALV报表数据</h2>`
`<p>请求地址：<a href="` me->my_url me->my_service `?sap-client=` sy-mandt `&tcode=%5Btcode%5D%5D" title="获取ALV报表数据">` me->my_url me->my_service `?sap-client=` sy-mandt `&tcode=[tcode]</a><br>`
`请求方法：POST<br>`
`content-type: "application/json;charset=utf-8"<br>`
`请求体:</p>`
`<pre><code>[`
`	{`
`   "selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",`
`   "kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",`
`   "sign": "ABAP/4: ID: I/E (包括/不包括值)",`
`   "option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",`
`   "low": "ABAP：选择值（LOW 值，外部格式）",`
`   "high": "ABAP：选择值（HIGH 值，外部格式）"`
`	}`
`]`
`</code></pre>`
`<p>返回消息结构：</p>`
`<pre><code>{`
`	"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`	"rtmsg": "消息文本",`
`	"mapping": [`
`   {`
`     "fieldname": "字段名",`
`     "scrtext_s": "短字段描述",`
`     "scrtext_m": "中字段描述",`
`     "scrtext_l": "长字段描述"`
`   }`
`	],`
`	"data": [`
`   {`
`     "字段名": "字段值"`
`   }`
`	]`
`}`
`</code></pre>`
`<h2 id="获取底表数据">获取底表数据</h2>`
`<p>请求地址：<a href="` me->my_url me->my_service `?sap-client=` sy-mandt `&tabname=%5Btabname%5D%5D" title="获取底表数据">` me->my_url me->my_service `?sap-client=` sy-mandt `&tabname=[tabname]</a><br>`
`请求方法：POST<br>`
`content-type: "application/json;charset=utf-8" <br>`
`请求体(可选):</p>`
`<pre><code>{`
`	"wherestr": "SQL查询条件，不用加where"`
`}`
`</code></pre>`
`返回消息结构：</p>`
`<pre><code>{`
`	"rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`	"rtmsg": "消息文本",`
`	"mapping": [`
`   {`
`     "fieldname": "字段名",`
`     "scrtext_s": "短字段描述",`
`     "scrtext_m": "中字段描述",`
`     "scrtext_l": "长字段描述"`
`   }`
`	],`
`	"data": [`
`   {`
`     "字段名": "字段值"`
`   }`
`	]`
`}`
`</code></pre>`
`<h2 id="后续计划">后续计划</h2>`
`<ol>`
`<li><s>取底表数据添加OPEN SQL查询条件</s></li>`
`<li>扩展下支持通过http调用RFC接口（完善<a href="https://github.com/cesar-sap/abap_fm_json" title="abap_fm_json">cesar-sap/abap_fm_json</a>不支持全部参数的缺陷），但是感觉这样做意义不是太大。</li>`
`<li>想得到再说吧</li>`
`</ol>`
`<h2 id="联系我">联系我</h2>`
`<p>邮箱：<a href="mailto:weikj@foxmail.com" title="kkw">weikj@foxmail.com</a></p>`
`</div>`
`</body>`
``
`</html>`

    INTO text  RESPECTING BLANKS.
  ENDMETHOD.


  METHOD get_interface.
    DATA:json_meta_sub  TYPE string,
         json_meta_sub1 TYPE string.
    CLEAR out_json.
    LOOP AT datatypescont ASSIGNING FIELD-SYMBOL(<datatypescont>) WHERE typename = tabname ."AND fieldname = p_fieldname.
      CLEAR json_meta_sub1.
      CASE <datatypescont>-comptype.
        WHEN 'S'."结构
          CALL METHOD me->get_interface
            EXPORTING
              tabname   = <datatypescont>-fieldtype
              fieldname = ''
            RECEIVING
              out_json  = json_meta_sub1.
          SHIFT json_meta_sub1 RIGHT DELETING TRAILING ','.
          json_meta_sub = |{ json_meta_sub }"{ <datatypescont>-fieldname }":| && `{` && |{ json_meta_sub1 }| && `},` .
        WHEN 'T'." 表则返回对应的结构的数据元素
          READ TABLE datatypescont ASSIGNING FIELD-SYMBOL(<datatypescontl1>) WITH KEY typename = <datatypescont>-fieldtype.
          IF sy-subrc EQ 0.
            CALL METHOD me->get_interface
              EXPORTING
                tabname   = <datatypescontl1>-typename
                fieldname = ''
              RECEIVING
                out_json  = json_meta_sub1.
            SHIFT json_meta_sub1 RIGHT DELETING TRAILING ','.
            json_meta_sub = |{ json_meta_sub }{ json_meta_sub1 },|.
          ENDIF.
        WHEN 'L'."表类型
          READ TABLE datatypescont ASSIGNING <datatypescontl1> WITH KEY typename = <datatypescont>-fieldtype.
          IF sy-subrc EQ 0.
            READ TABLE datatypescont ASSIGNING FIELD-SYMBOL(<datatypescontl2>) WITH KEY typename = <datatypescontl1>-fieldtype.
            IF sy-subrc EQ 0.
              CALL METHOD me->get_interface
                EXPORTING
                  tabname   = <datatypescontl2>-typename
                  fieldname = ''
                RECEIVING
                  out_json  = json_meta_sub1.
              SHIFT json_meta_sub1 RIGHT DELETING TRAILING ','.
              json_meta_sub = |{ json_meta_sub }"{ <datatypescont>-fieldname }":| && `[{` && |{ json_meta_sub1 }| && `}],`.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          json_meta_sub = |{ json_meta_sub }"{ <datatypescont>-fieldname }":"{ <datatypescont>-description }",|.
      ENDCASE.
    ENDLOOP.

    SHIFT json_meta_sub RIGHT DELETING TRAILING ','.
    out_json = json_meta_sub.
  ENDMETHOD.


  METHOD exec_rfc.
    DATA:t_params_p TYPE TABLE OF rfc_fint_p,
         params_p   TYPE rfc_fint_p,
         paramtab   TYPE abap_func_parmbind_tab,
         paramline  TYPE LINE OF abap_func_parmbind_tab,
         dataname   TYPE string.
    DATA:dref_int TYPE REF TO data,
         dref     TYPE REF TO data.
    DATA:table_type TYPE REF TO cl_abap_tabledescr,
         data_type  TYPE REF TO cl_abap_datadescr.
    DATA:paramclass TYPE rs38l_kind.
    DATA:json_meta      TYPE string,
         json_meta_full TYPE string.
    CLEAR:rtype,rtmsg,out_json.
    CALL FUNCTION 'RFC_GET_FUNCTION_INTERFACE_P'
      EXPORTING
        funcname      = funcname
      TABLES
        params_p      = t_params_p
      EXCEPTIONS
        fu_not_found  = 1
        nametab_fault = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      rtype = 'E'.
      rtmsg = |调用RFC：{ funcname }发生了异常，状态码：{ sy-subrc }|.
    ENDIF.
    /ui2/cl_json=>deserialize( EXPORTING json = interface  pretty_name = /ui2/cl_json=>pretty_mode-low_case CHANGING data = dref_int ).
    CLEAR:paramtab.
    LOOP AT t_params_p INTO params_p WHERE paramclass = 'I' OR paramclass = 'E' OR paramclass = 'C' OR paramclass = 'T'.
      CLEAR:paramline,dataname.
      CASE params_p-paramclass.
        WHEN 'I' OR 'E' OR 'C' OR 'T'.
          paramline-name = params_p-parameter.
          IF params_p-paramclass = 'E'.
            paramline-kind = abap_func_importing.
          ELSEIF params_p-paramclass = 'I'.
            paramline-kind = abap_func_exporting.
          ELSEIF params_p-paramclass = 'C'.
            paramline-kind = abap_func_changing.
          ELSE.
            paramline-kind = abap_func_tables.
          ENDIF.
          IF params_p-fieldname IS INITIAL.
            dataname = params_p-tabname.
          ELSE.
            CONCATENATE params_p-tabname params_p-fieldname INTO dataname SEPARATED BY '-'.
          ENDIF.
          CLEAR:dref.
          TRY.
              data_type ?= cl_abap_datadescr=>describe_by_name( p_name = dataname ).
            CATCH cx_sy_move_cast_error.
              CONTINUE.
          ENDTRY.
          CLEAR table_type.
          IF params_p-paramclass = 'T'.
            IF data_type->kind = 'S'.
              table_type ?= cl_abap_tabledescr=>create( p_line_type = data_type ).
            ELSE.
              table_type ?= data_type.
            ENDIF.
            CREATE DATA dref TYPE HANDLE table_type.
          ELSE.
            CREATE DATA dref TYPE HANDLE data_type .
          ENDIF.
          ASSIGN dref->* TO FIELD-SYMBOL(<dref_value>).
          DATA(value) = |DREF_INT->{ params_p-paramclass }->{ params_p-parameter }|.
          ASSIGN (value) TO FIELD-SYMBOL(<value>).
          IF NOT <value> IS ASSIGNED.
            IF params_p-default IS NOT INITIAL.
              DATA(len) = strlen( params_p-default ) - 2.
              IF len GT 0.
                <dref_value> = params_p-default+1(len).
              ENDIF.
            ENDIF.
          ELSE.
            DATA(value_json) = /ui2/cl_json=>serialize( data = <value> compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-none ).
            /ui2/cl_json=>deserialize( EXPORTING json = value_json pretty_name = /ui2/cl_json=>pretty_mode-none CHANGING data = <dref_value> ).
          ENDIF.
          paramline-value = dref.
          INSERT paramline INTO TABLE paramtab.
          UNASSIGN:<value>,<dref_value>.
        WHEN OTHERS.
*          RAISE unsupported_param_type.
      ENDCASE.
    ENDLOOP.
    TRY.
        CALL FUNCTION funcname
          PARAMETER-TABLE
          paramtab.
        json_meta_full = `{`.
        LOOP AT paramtab ASSIGNING FIELD-SYMBOL(<paramtab>) GROUP BY ( kind = <paramtab>-kind
          size = GROUP SIZE index = GROUP INDEX
          ) ASCENDING ASSIGNING FIELD-SYMBOL(<group>).
          CLEAR paramclass.
          CASE <group>-kind.
            WHEN abap_func_importing.
              paramclass = 'E'.
            WHEN abap_func_exporting.
              paramclass = 'I'.
            WHEN abap_func_changing.
              paramclass = 'C'.
            WHEN abap_func_tables.
              paramclass = 'T'.
          ENDCASE.
          json_meta_full = json_meta_full && |"{ paramclass }":| && `{`.
          LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
            DATA(json) = /ui2/cl_json=>serialize( data = <mem>-value compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-none ).
            json_meta = |"{ <mem>-name }":{ json }| .
            json_meta_full = json_meta_full && json_meta && ',' .
          ENDLOOP.
          SHIFT json_meta_full RIGHT DELETING TRAILING ','.
          json_meta_full = json_meta_full && `},`.
        ENDLOOP.
        SHIFT json_meta_full RIGHT DELETING TRAILING ','.
        json_meta_full = json_meta_full && `}` .
        rtype = 'S'.
        rtmsg = |执行函数{ funcname }未发生异常，执行结果请查看interface节点参数|.
        out_json = json_meta_full.
      CATCH cx_root INTO DATA(exc).
        rtype = 'E'.
        rtmsg = |执行函数{ funcname }发生了异常：{ exc->get_text( ) }|.
        out_json = `{}`.
    ENDTRY.

  ENDMETHOD.


  METHOD notes2.
    CLEAR text.
    CONCATENATE

`<!DOCTYPE html>                                                                                                                                                                                          `
`<html>                                                                                                                                                                                                   `
`                                                                                                                                                                                                         `
`<head>                                                                                                                                                                                                   `
`  <meta charset="utf-8">                                                                                                                                                                                 `
`  <meta name="viewport" content="width=device-width, initial-scale=1.0">                                                                                                                                 `
`  <title>通用取数接口服务</title>                                                                                                                                                                        `
`  <link rel="stylesheet" href="https://stackedit.io/style.css" />                                                                                                                                        `
`</head>                                                                                                                                                                                                  `
`                                                                                                                                                                                                         `
`<body class="stackedit">                                                                                                                                                                                 `
`  <div class="stackedit__left">                                                                                                                                                                          `
`    <div class="stackedit__toc">                                                                                                                                                                         `
`                                                                                                                                                                                                         `
`<ul>                                                                                                                                                                                                     `
`<li><a href="#关于这项服务的使用说明">关于这项服务的使用说明</a>                                                                                                                                                               `
`<ul>                                                                                                                                                                                                     `
`<li><a href="#权限检查">权限检查</a></li>                                                                                                                                                                        `
`<li><a href="#获取alv报表选择屏幕参数">获取ALV报表选择屏幕参数</a></li>                                                                                                                                                      `
`<li><a href="#获取alv报表数据">获取ALV报表数据</a></li>                                                                                                                                                              `
`<li><a href="#获取底表数据">获取底表数据</a></li>                                                                                                                                                                    `
`<li><a href="#获取rfc出入参">获取RFC出入参</a></li>                                                                                                                                                                `
`<li><a href="#调用rfc">调用RFC</a></li>                                                                                                                                                                      `
`<li><a href="#后续计划">后续计划</a></li>                                                                                                                                                                        `
`<li><a href="#联系我">联系我</a></li>                                                                                                                                                                          `
`</ul>                                                                                                                                                                                                    `
`</li>                                                                                                                                                                                                    `
`</ul>                                                                                                                                                                                                    `
`                                                                                                                                                                                                         `
`    </div>                                                                                                                                                                                               `
`  </div>                                                                                                                                                                                                 `
`  <div class="stackedit__right">                                                                                                                                                                         `
`    <div class="stackedit__html">                                                                                                                                                                        `
`      <h1 id="关于这项服务的使用说明">关于这项服务的使用说明</h1>                                                                                                                                                              `
`<p>这是一个获取SAP的ALV报表、底表和RFC数据的HTTP接口服务。</p>                                                                                                                                                                `
`<p>它以 ICF 服务的形式提供此接口，在此系统中，此服务已分配给 ICF 服务<a href="` me->my_url me->my_service `?sap-client=` sy-mandt `" title="调用地址">` me->my_service `</a>。</p>                                                                       `
`<h2 id="权限检查">权限检查</h2>                                                                                                                                                                                  `
`<p>该服务包含一个对名为 ZBI_AUTH 的自定义授权对象的AUTHORITY_CHECK调用，用于验证用户是否可以访问事务码或者底表,需要为每个访问该服务的用户都创建一个单独的角色，其中只有允许他访问的功能。<br>                                                                                          `
`<strong>权限对象</strong>：<br>                                                                                                                                                                               `
`<img src="https://s21.ax1x.com/2024/03/16/pF2QXjg.jpg" alt="pF2QXjg.jpg"><br>                                                                                                                            `
`<strong>权限角色</strong>：<br>                                                                                                                                                                               `
`<img src="https://s21.ax1x.com/2024/03/16/pF2lLI1.jpg" alt="pF2lLI1.jpg"></p>                                                                                                                            `
`<p>该服务提供了五类方法：获取ALV报表选择屏幕参数、获取ALV报表数据、获取底表数据、获取RFC出入参和调用RFC，具体请求示例请查看下面的详细介绍。</p>                                                                                                                        `
`<h2 id="获取alv报表选择屏幕参数">获取ALV报表选择屏幕参数</h2>                                                                                                                                                                `
`<p>cURL请求：</p>                                                                                                                                                                                           `
`<pre><code>curl --location '` me->my_url me->my_service `?sap-client=` sy-mandt `&amp;tcode=[事务码]' \`
`--header 'Content-Type: application/x-www-form-urlencoded' \`
`--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \`
`--data-urlencode 'action=rsparams'`
`</code></pre>`
`<p>返回消息结构：</p>`
`<pre><code>{`
`  "rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`  "rtmsg": "消息文本",`
`  "rsparams": [`
`    {`
`      "selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",`
`      "kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",`
`      "sign": "ABAP/4: ID: I/E (包括/不包括值)",`
`      "option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",`
`      "low": "ABAP：选择值（LOW 值，外部格式）",`
`      "high": "ABAP：选择值（HIGH 值，外部格式）"`
`    }`
`  ]`
`}`
`</code></pre>`
`<h2 id="获取alv报表数据">获取ALV报表数据</h2>                                                                                                                                                                        `
`<p>cURL请求：</p>                                                                                                                                                                                           `
`<pre><code>curl --location '` me->my_url me->my_service `?sap-client=` sy-mandt `&amp;tcode=[事务码]' \`
`--header 'Content-Type: application/json' \`
`--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \`
`--data '[`
`  {`
`    "selname": "ABAP：SELECT-OPTION/PARAMETER 的名称",`
`    "kind": "ABAP/4: 选择类型，单选字段配置'P'，多选字段配置'S'",`
`    "sign": "ABAP/4: ID: I/E (包括/不包括值)",`
`    "option": "ABAP/4: 选择选项 (EQ/BT/CP/...)",`
`    "low": "ABAP：选择值（LOW 值，外部格式）",`
`    "high": "ABAP：选择值（HIGH 值，外部格式）"`
`  }`
`]'`
`</code></pre>`
`<p>返回消息结构：</p>                                                                                                                                                                                           `
`<pre><code>{`
`  "rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`  "rtmsg": "消息文本",`
`  "mapping": [`
`    {`
`      "fieldname": "字段名",`
`      "scrtext_s": "短字段描述",`
`      "scrtext_m": "中字段描述",`
`      "scrtext_l": "长字段描述"`
`    }`
`  ],`
`  "data": [`
`    {`
`      "字段名": "字段值"`
`    }`
`  ]`
`}`
`</code></pre>`
`<h2 id="获取底表数据">获取底表数据</h2>                                                                                                                                                                              `
`<p>cURL请求：</p>                                                                                                                                                                                           `
`<pre><code>curl --location '` me->my_url me->my_service `?sap-client=` sy-mandt `&amp;tabname=[透明表表名]' \`
`--header 'Content-Type: application/json' \`
`--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \`
`--data '{"wherestr":"SQL查询条件，不用加where"}'`
`</code></pre>`
`<p>返回消息结构：</p>                                                                                                                                                                                           `
`<pre><code>{`
`  "rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`  "rtmsg": "消息文本",`
`  "mapping": [`
`    {`
`      "fieldname": "字段名",`
`      "scrtext_s": "短字段描述",`
`      "scrtext_m": "中字段描述",`
`      "scrtext_l": "长字段描述"`
`    }`
`  ],`
`  "data": [`
`    {`
`      "字段名": "字段值"`
`    }`
`  ]`
`}`
`</code></pre>`
`<h2 id="获取rfc出入参">获取RFC出入参</h2>                                                                                                                                                                          `
`<p>cURL请求：</p>                                                                                                                                                                                           `
`<pre><code>curl --location '` me->my_url me->my_service `?sap-client=` sy-mandt `&amp;funcname=[RFC名]' \`
`--header 'Content-Type: application/x-www-form-urlencoded' \`
`--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \`
`--data-urlencode 'action=interface'`
`</code></pre>`
`<p>返回消息结构：</p>                                                                                                                                                                                           `
`<pre><code>{`
`  "rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`  "rtmsg": "消息文本",`
`  "interface": {`
`    "I": {"导入参数名称":"导入参数JSON结构体"},`
`    "E": {"导出参数名称":"导出参数JSON结构体"},`
`    "C": {"更改参数名称":"更改参数JSON结构体"},`
`    "T": {"表参数名称":"表参数JSON结构体"}`
`  }`
`}`
`</code></pre>`
`<h2 id="调用rfc">调用RFC</h2>                                                                                                                                                                                `
`<p>cURL请求：</p>                                                                                                                                                                                           `
`<pre><code>curl --location '` me->my_url me->my_service `?sap-client=` sy-mandt `&amp;funcname=[RFC名]' \`
`--header 'Content-Type: application/json' \`
`--header 'Authorization: Basic dW5hbWU6cGFzc3dk' \`
`--data '{`
`  "I": {`
`    "导入参数名称": "导入参数JSON结构体"`
`  },`
`  "E": {`
`    "导出参数名称": "导出参数JSON结构体"`
`  },`
`  "C": {`
`    "更改参数名称": "更改参数JSON结构体"`
`  },`
`  "T": {`
`    "表参数名称": "表参数JSON结构体"`
`  }`
`}'`
`</code></pre>`
`<p>返回消息结构：</p>                                                                                                                                                                                           `
`<pre><code>{`
`  "rtype": "消息类型: S 成功,E 错误,W 警告,I 信息,A 中断",`
`  "rtmsg": "消息文本",`
`  "interface": {`
`    "I": {"导入参数名称":"导入参数JSON结构体"},`
`    "E": {"导出参数名称":"导出参数JSON结构体"},`
`    "C": {"更改参数名称":"更改参数JSON结构体"},`
`    "T": {"表参数名称":"表参数JSON结构体"}`
`  }`
`}`
`</code></pre>`
`<h2 id="后续计划">后续计划</h2>                                                                                                                                                                                  `
`<ol>                                                                                                                                                                                                     `
`<li><s>取底表数据添加OPEN SQL查询条件</s> 202405</li>                                                                                                                                                               `
`<li><s>扩展下支持通过http调用RFC接口（完善<a href="https://github.com/cesar-sap/abap_fm_json">cesar-sap/abap_fm_json</a>不支持全部参数的缺陷）</s> 202409</li>                                                                    `
`<li>想到了再说&#128514;</li>                                                                                                                                                                                          `
`</ol>                                                                                                                                                                                                    `
`<h2 id="联系我">联系我</h2>                                                                                                                                                                                    `
`<p>邮箱：<a href="mailto:weikj@foxmail.com" title="kkw">weikj@foxmail.com</a></p>                                                                                                                           `
`                                                                                                                                                                                                         `
`    </div>                                                                                                                                                                                               `
`  </div>                                                                                                                                                                                                 `
`</body>                                                                                                                                                                                                  `
`                                                                                                                                                                                                         `
`</html>`


    INTO text  RESPECTING BLANKS.
  ENDMETHOD.


  METHOD exec_rfc_crtdata.
    DATA:t_params_p TYPE TABLE OF rfc_fint_p,
         paramtab   TYPE abap_func_parmbind_tab,
         paramline  TYPE LINE OF abap_func_parmbind_tab,
         dataname   TYPE string.
    DATA:dref     TYPE REF TO data,
         dref_req TYPE REF TO data.
    DATA:table_type TYPE REF TO cl_abap_tabledescr,
         data_type  TYPE REF TO cl_abap_datadescr.
    DATA:component_table      TYPE abap_component_tab,
         componentclass_table TYPE abap_component_tab.
    CLEAR:rtype,rtmsg,out_json.
    CALL FUNCTION 'RFC_GET_FUNCTION_INTERFACE_P'
      EXPORTING
        funcname      = funcname
      TABLES
        params_p      = t_params_p
      EXCEPTIONS
        fu_not_found  = 1
        nametab_fault = 2
        OTHERS        = 3.
    IF sy-subrc <> 0.
      rtype = 'E'.
      rtmsg = |获取RFC：{ funcname }参数发生了异常，状态码：{ sy-subrc }|.
    ENDIF.
    IF t_params_p IS NOT INITIAL.
      CLEAR:paramtab.
      LOOP AT t_params_p ASSIGNING FIELD-SYMBOL(<params_p>)
       " WHERE paramclass = 'I' OR paramclass = 'E' OR paramclass = 'C' OR paramclass = 'T'
        GROUP BY ( paramclass = <params_p>-paramclass
        index = GROUP INDEX size = GROUP SIZE
        ) ASSIGNING FIELD-SYMBOL(<groupcrt>).
        CLEAR:componentclass_table,paramline.
        LOOP AT GROUP <groupcrt> ASSIGNING FIELD-SYMBOL(<memcrt>).
          CLEAR:paramline,dataname.
          CASE <groupcrt>-paramclass.
            WHEN 'I' OR 'E' OR 'C' OR 'T'.
              paramline-name = <memcrt>-parameter.
              IF <groupcrt>-paramclass = 'E'.
                paramline-kind = abap_func_importing.
              ELSEIF <groupcrt>-paramclass = 'I'.
                paramline-kind = abap_func_exporting.
              ELSEIF <groupcrt>-paramclass = 'C'.
                paramline-kind = abap_func_changing.
              ELSE.
                paramline-kind = abap_func_tables.
              ENDIF.
              IF <memcrt>-fieldname IS INITIAL.
                dataname = <memcrt>-tabname.
              ELSE.
                CONCATENATE <memcrt>-tabname <memcrt>-fieldname INTO dataname SEPARATED BY '-'.
              ENDIF.
              CLEAR data_type.
              TRY.
                  data_type ?= cl_abap_datadescr=>describe_by_name( p_name = dataname ).
                CATCH cx_root INTO DATA(excd).
                  DATA(exc_msg) = excd->get_text( ).
                  CONTINUE.
              ENDTRY.
              CLEAR:table_type,dref.
              IF <groupcrt>-paramclass = 'T'.
                IF data_type->kind = 'S'.
                  table_type ?= cl_abap_tabledescr=>create( p_line_type = data_type ).
                ELSE.
                  table_type ?= data_type.
                ENDIF.
                CREATE DATA dref TYPE HANDLE table_type.
              ELSE.
                CREATE DATA dref TYPE HANDLE data_type.
              ENDIF.
              paramline-value = dref.
              IF <memcrt>-default IS NOT INITIAL.
                ASSIGN dref->* TO FIELD-SYMBOL(<dref_value>).
                DATA(len) = strlen( <memcrt>-default ) - 2.
                IF len GT 0.
                  <dref_value> = <memcrt>-default+1(len).
                ENDIF.
              ENDIF.
              INSERT paramline INTO TABLE paramtab.

              INSERT INITIAL LINE INTO TABLE componentclass_table ASSIGNING FIELD-SYMBOL(<ctclass>).
              <ctclass>-name         = <memcrt>-parameter.
              <ctclass>-as_include   = ''.
              <ctclass>-suffix       = ''.
              <ctclass>-type ?= cl_abap_typedescr=>describe_by_data_ref( p_data_ref = dref ).
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.
        IF componentclass_table IS NOT INITIAL.
          INSERT INITIAL LINE INTO TABLE component_table ASSIGNING FIELD-SYMBOL(<ct>).
          <ct>-name         = <groupcrt>-paramclass.
          <ct>-as_include   = ''.
          <ct>-suffix       = ''.
          <ct>-type ?= cl_abap_structdescr=>create( componentclass_table ).
        ENDIF.
      ENDLOOP.
      DATA(result) = cl_abap_structdescr=>create( component_table ).
      CREATE DATA dref TYPE HANDLE result.
      /ui2/cl_json=>deserialize( EXPORTING json = interface  CHANGING data = dref ).
      /ui2/cl_json=>deserialize( EXPORTING json = interface  CHANGING data = dref_req ).
      ASSIGN dref->* TO FIELD-SYMBOL(<dref>).

      LOOP AT paramtab ASSIGNING FIELD-SYMBOL(<paramline>).
        CASE <paramline>-kind.
          WHEN abap_func_importing.
            DATA(kind) = 'E'.
          WHEN abap_func_exporting.
            kind = 'I'.
          WHEN abap_func_changing.
            kind = 'C'.
          WHEN abap_func_tables.
            kind = 'T'.
        ENDCASE.
        DATA(dref_input_name) = to_upper( |<dref>-{ kind }-{ <paramline>-name }| ).
        ASSIGN (dref_input_name) TO FIELD-SYMBOL(<dref_input_value>).
        IF sy-subrc NE 0.
          UNASSIGN <dref_input_value>.
          CONTINUE.
        ENDIF.
        ASSIGN <paramline>-value->* TO FIELD-SYMBOL(<value>).
        IF kind = 'I' OR kind = 'C'.
          DATA(dref_req_name) = to_upper( |dref_req->{ kind }->{ <paramline>-name }->*| ).
          ASSIGN (dref_req_name) TO FIELD-SYMBOL(<dref_req_value>).
          IF <dref_req_value> IS ASSIGNED.
            <value> = <dref_input_value>.
            UNASSIGN <dref_req_value>.
          ENDIF.
        ELSEIF kind = 'T'.
          <value> = <dref_input_value>.
        ENDIF.
        UNASSIGN <dref_input_value>.
      ENDLOOP.
    ENDIF.
    TRY.
        CALL FUNCTION funcname
          PARAMETER-TABLE
          paramtab.
        CLEAR kind.
        LOOP AT paramtab ASSIGNING <paramline> WHERE kind = abap_func_importing OR kind = abap_func_changing OR kind = abap_func_tables.
          CASE <paramline>-kind.
            WHEN abap_func_importing.
              kind = 'E'.
            WHEN abap_func_exporting.
              kind = 'I'.
            WHEN abap_func_changing.
              kind = 'C'.
            WHEN abap_func_tables.
              kind = 'T'.
          ENDCASE.
          DATA(dref_iput_namen) = to_upper( |<dref>-{ kind }-{ <paramline>-name }| ).
          ASSIGN (dref_iput_namen) TO FIELD-SYMBOL(<dref_iput_valuen>).
          IF sy-subrc NE 0.
            UNASSIGN <dref_iput_valuen>.
          ENDIF.
          IF <dref_iput_valuen> IS ASSIGNED.
            ASSIGN <paramline>-value->* TO FIELD-SYMBOL(<valuen>).
            <dref_iput_valuen> = <valuen>.
          ENDIF.
        ENDLOOP.
        out_json = /ui2/cl_json=>serialize( data = <dref> compress = abap_false pretty_name = /ui2/cl_json=>pretty_mode-none ).
        rtype = 'S'.
        rtmsg = |执行函数{ funcname }未发生异常，执行结果请查看interface节点参数|.
      CATCH cx_root INTO DATA(exc).
        rtype = 'E'.
        rtmsg = |执行函数{ funcname }发生了异常：{ exc->get_text( ) }|.
        out_json = `{}`.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS zcl_bi_data_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_extension .

    METHODS get_rsparams
      IMPORTING
        VALUE(tcode)    TYPE sy-tcode
      RETURNING
        VALUE(rsparams) TYPE string
      EXCEPTIONS
        tcode_not_found .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA my_service TYPE string .
    DATA my_url TYPE string .
    DATA my_params TYPE tihttpnvp .

    METHODS get_query
      IMPORTING
        VALUE(query_string) TYPE string
        VALUE(key)          TYPE string
      EXPORTING
        VALUE(value)        TYPE string .
    METHODS get_params
      IMPORTING
        !params          TYPE string
      RETURNING
        VALUE(my_params) TYPE tihttpnvp .
    METHODS notes
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
         proto            TYPE string,
         host             TYPE string,
         port             TYPE string,
         action           TYPE string.

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
        server->response->set_status( code = 200  reason = &3 ).
      ENDIF.
      CASE &1.
        WHEN 'DATA'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `", "mapping":` &6 `, "data":` &7 `}` INTO json.
        WHEN 'AUTH'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `"}` INTO json.
        WHEN 'RSPARAMS'.
          CONCATENATE `{"rtype":"` &4 `", "rtmsg":"` &5 `", "rsparams":` &8 `}` INTO json.
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
        json = me->notes( ).
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
        IF tcode IS NOT INITIAL.
          AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TCD' FIELD tcode.
*          AUTHORITY-CHECK OBJECT 'ZBI_AUTH_C' ID 'ZE_TCODE' FIELD tcode.
          IF sy-subrc NE 0.
            rtmsg = `你没有事务码` && tcode && `的权限`.
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
              rtmsg = `事务码` && tcode && `不存在`.
              http_msg 'RSPARAMS' 404 'Tcode not found' 'E' rtmsg '' '' '[]'.
            ELSE.
              rtmsg = `成功获取事务码` && tcode && `的选择屏幕参数`.
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
        ELSEIF tabname IS NOT INITIAL.
          AUTHORITY-CHECK OBJECT 'ZBI_AUTH' ID 'TABLE' FIELD tabname.
*          AUTHORITY-CHECK OBJECT 'ZBI_AUTH_C' ID 'ZE_TABLE' FIELD tabname.
          IF sy-subrc NE 0.
            rtmsg = `你没有底表` && tabname && `的权限` .
            http_msg 'AUTH' 403 'Not authorized' 'E' rtmsg '' '' ''.
          ENDIF.
          CALL FUNCTION 'ZFM_BI_DATA_TABLE_GET'
            EXPORTING
              tabname          = tabname
*             wherestr         =
            IMPORTING
              rtype            = rtype
              rtmsg            = rtmsg
              out_json         = json
              out_mapping_json = out_mapping_json.
        ELSE.
          http_msg 'DATA' 412 'Params error' 'E' 'Params键只能是tcode或tabname' '[]' '[]' ''.
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
`<li>取底表数据添加OPEN SQL查询条件</li>`
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
ENDCLASS.

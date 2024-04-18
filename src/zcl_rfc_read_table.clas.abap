CLASS zcl_rfc_read_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ts_out_str TYPE sdti_result .
    TYPES tt_out_str TYPE sdti_result_tab .
    TYPES:
      tt_fields TYPE STANDARD TABLE OF rfc_db_fld .
    TYPES:
      tt_fields_k TYPE STANDARD TABLE OF rfc_db_fld WITH KEY fieldname .
    TYPES:
      BEGIN OF ts_fields_int,
        fieldname  TYPE dfies-fieldname,
        type       TYPE dfies-inttype,
        rollname   TYPE dfies-rollname,
        decimals   TYPE dfies-decimals,
        length_src TYPE dfies-intlen,
        length_dst TYPE dfies-leng,
        offset_src TYPE dfies-offset,
        offset_dst TYPE dfies-offset,
      END OF ts_fields_int .
    TYPES:
      tt_fields_int TYPE STANDARD TABLE OF ts_fields_int WITH KEY fieldname .
    TYPES:
      tt_sele TYPE STANDARD TABLE OF rfc_db_opt WITH KEY text .
    TYPES:
      tt_512 TYPE STANDARD TABLE OF tab512 WITH KEY wa .

    METHODS constructor
      IMPORTING
        !id_table           TYPE tabname
        !id_block_list_tabl TYPE sldw_name OPTIONAL
        !id_block_list_call TYPE sldw_name OPTIONAL
        !id_delim           TYPE sonv-flag OPTIONAL
        !id_no_data         TYPE sonv-flag OPTIONAL
        !id_sort            TYPE boole_d OPTIONAL
      RAISING
        cx_sais_raise_events .
    METHODS get_table_content
      IMPORTING
        !id_skip_rows TYPE soid-accnt OPTIONAL
        !id_max_row   TYPE soid-accnt OPTIONAL
        !it_fields    TYPE tt_fields OPTIONAL
        !it_selopt    TYPE tt_sele OPTIONAL
      EXPORTING
        !et_data      TYPE tt_out_str
        !et_fields    TYPE tt_fields
      RAISING
        cx_sais_raise_events .
    METHODS get_table_content_512
      IMPORTING
        !id_skip_rows TYPE soid-accnt OPTIONAL
        !id_max_row   TYPE soid-accnt OPTIONAL
        !it_fields    TYPE tt_fields OPTIONAL
        !it_selopt    TYPE tt_sele OPTIONAL
      EXPORTING
        !et_data      TYPE tt_512
        !et_fields    TYPE tt_fields
      RAISING
        cx_sais_raise_events .
    CLASS-METHODS convert_to_exception
      RETURNING
        VALUE(es_evt) TYPE scx_t100key .
    METHODS get_table_dref
      IMPORTING
        !id_skip_rows TYPE soid-accnt OPTIONAL
        !id_max_row   TYPE soid-accnt OPTIONAL
        !it_fields    TYPE tt_fields OPTIONAL
        !it_selopt    TYPE tt_sele OPTIONAL
      EXPORTING
        !et_data      TYPE REF TO data
        !et_fields    TYPE tt_fields
      RAISING
        cx_sais_raise_events .
  PROTECTED SECTION.

    METHODS convert_lines
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(et_data) TYPE tt_out_str .
    METHODS convert_lines_512
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(et_data) TYPE tt_512
      RAISING
        cx_sais_raise_events .
    METHODS compute_field_list
      IMPORTING
        !it_fields       TYPE tt_fields
      RETURNING
        VALUE(et_fields) TYPE tt_fields_k
      RAISING
        cx_sais_raise_events .
    METHODS compute_sel_criteria
      IMPORTING
        !it_sel_crit       TYPE tt_sele
      RETURNING
        VALUE(ed_sel_crit) TYPE tt_sele
      RAISING
        cx_sais_raise_events .
  PRIVATE SECTION.

    DATA gd_table TYPE tabname .
    DATA gd_delim TYPE sonv-flag .
    DATA gd_no_data TYPE sonv-flag .
    DATA gd_sorted TYPE boole_d .
    DATA gt_fields TYPE tt_fields .
    DATA gt_fields_int TYPE tt_fields_int .
    DATA gd_sel_crit TYPE tt_sele .
    DATA gd_sel_fields TYPE string .
    DATA gd_out_pattern TYPE string .
    DATA gd_mode TYPE char1 .
    DATA gd_block_list TYPE sldw_name .
    DATA gd_block_list_call TYPE sldw_name .

    METHODS get_from_db
      IMPORTING
        !id_skip_rows  TYPE soid-accnt
        !id_max_row    TYPE soid-accnt
      RETURNING
        VALUE(er_data) TYPE REF TO data
      RAISING
        cx_sais_raise_events .
ENDCLASS.



CLASS ZCL_RFC_READ_TABLE IMPLEMENTATION.


  METHOD compute_field_list.
    DATA:
      ld_buf         TYPE string,
      ls_fields      TYPE rfc_db_fld,
      ld_delim_leng  TYPE i,
      ld_fld_lst     TYPE boole_d,
      lt_dfies       TYPE STANDARD TABLE OF dfies,
      ld_tab_typ     TYPE dd02v-tabclass,
      ls_fld_int     TYPE ts_fields_int,
      ld_placeholder TYPE string,
      ld_offset      TYPE i.
    FIELD-SYMBOLS:
      <fs_dfies> TYPE dfies.
    "-----------------------------------------------------------------------
    CLEAR et_fields.
    ld_delim_leng = strlen( gd_delim ).

    IF it_fields IS INITIAL.
      gd_sel_fields = '*'.
    ENDIF.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = gd_table
      IMPORTING
        ddobjtype      = ld_tab_typ
      TABLES
        dfies_tab      = lt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE e131(da) WITH gd_table INTO ld_buf.
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ELSEIF ld_tab_typ = 'INTTAB' OR lt_dfies IS INITIAL.
      MESSAGE e718(ad) WITH gd_table INTO ld_buf.      "raise table_without_data.
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDIF.

    IF it_fields IS NOT INITIAL.
      ld_fld_lst = abap_true.
    ENDIF.

    LOOP AT lt_dfies ASSIGNING <fs_dfies>.
      IF ld_fld_lst = abap_true.
        READ TABLE it_fields WITH KEY fieldname = <fs_dfies>-fieldname TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        IF gd_sel_fields IS INITIAL.
          gd_sel_fields = <fs_dfies>-fieldname.
        ELSE.
          CONCATENATE gd_sel_fields <fs_dfies>-fieldname INTO gd_sel_fields SEPARATED BY ', '.
        ENDIF.
      ENDIF.
      ls_fld_int-fieldname = <fs_dfies>-fieldname.

      IF gd_mode = 'C'.
        IF <fs_dfies>-inttype = 'P'.
          ld_placeholder  = |{ gd_delim WIDTH = ( <fs_dfies>-leng + 2 ) ALIGN = RIGHT }|.
          CONCATENATE gd_out_pattern ld_placeholder INTO gd_out_pattern  RESPECTING BLANKS.
        ELSE.
          ld_placeholder  = |{ gd_delim WIDTH = ( <fs_dfies>-leng + 1 ) ALIGN = RIGHT }|.
          CONCATENATE gd_out_pattern ld_placeholder INTO gd_out_pattern  RESPECTING BLANKS.
        ENDIF.
      ENDIF.

      ls_fld_int-rollname   = <fs_dfies>-rollname.
      ls_fld_int-type       = <fs_dfies>-inttype.

      CASE  <fs_dfies>-inttype.
        WHEN 'P'.
          ls_fld_int-length_src = <fs_dfies>-intlen.
          ls_fld_int-length_dst = <fs_dfies>-leng + 1.
          ls_fld_int-offset_src = <fs_dfies>-offset + 2.
          ls_fld_int-offset_dst = ld_offset.
          ls_fld_int-decimals   = <fs_dfies>-decimals.

        WHEN 'y' OR 'g'.
          IF gd_mode = 'C'.
            CONTINUE.   "ignore STRING and XSTRING fields in 512 char per line mode
          ENDIF.

        WHEN OTHERS.
          ls_fld_int-length_src = <fs_dfies>-intlen.
          ls_fld_int-length_dst = <fs_dfies>-leng.
          ls_fld_int-offset_src = <fs_dfies>-offset.
          ls_fld_int-offset_dst = ld_offset.
          ls_fld_int-decimals   = <fs_dfies>-decimals.
      ENDCASE.

      APPEND ls_fld_int TO gt_fields_int.

      IF <fs_dfies>-inttype = 'P'.
        ld_offset = ld_offset + 1.
      ENDIF.
      ld_offset = ld_offset + <fs_dfies>-leng + ld_delim_leng.


      ls_fields-fieldname = <fs_dfies>-fieldname.
      ls_fields-fieldtext = <fs_dfies>-fieldtext.
      ls_fields-type      = <fs_dfies>-inttype.
      ls_fields-length    = <fs_dfies>-leng.
      ls_fields-offset    =  ls_fld_int-offset_dst.
      APPEND ls_fields TO et_fields.

    ENDLOOP.

    IF et_fields IS INITIAL.
      MESSAGE e718(ad) WITH gd_table INTO ld_buf.
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD compute_sel_criteria.
    DATA:
      ld_finding  TYPE boole_d,
      ld_sel_crit TYPE string.
    " convert selection criteria into one string,
    " just for security ...
    " - only simple queries <field> = 'value' AND/OR <field> = 'value'
    " - Dictionary keywords like JOIN , UPDATE, SELECT , WHERE, LEFT, INNER  ... are not allowed in where clause
    " - all used fieldnames must exist insidethe given table
    " - all fieldvalues must be quoted
    " - only ( ) are allowed, the count of left and right must be identic
    " - especially {}|*~+&`  are forbidden  ... % is needed for LIKE
    "---------------------------------------------------------------------
    CLEAR ed_sel_crit.

    LOOP AT it_sel_crit REFERENCE INTO DATA(lr_crit).
      CONCATENATE ld_sel_crit lr_crit->text INTO ld_sel_crit. " to be downward compatible avoid the SPACE between lines
    ENDLOOP.

    "check for critical key words or char
    FIND ALL OCCURRENCES OF REGEX ' SELECT| UPDATE| MODIFY| INSERT| DELETE| JOIN| LEFT| RIGHT| EXISTS'
      IN TABLE it_sel_crit
      IGNORING CASE            " not case sensitive
      RESULTS DATA(lt_err_01). " findings
    IF sy-subrc = 0.
      ld_finding = abap_true.
    ENDIF.

    "check for critical characters
    FIND FIRST OCCURRENCE OF REGEX '\s(!|\$|&|\?)' IN ld_sel_crit. " REGEX: Space followed by either one of these: ! $ & ?
    IF sy-subrc = 0.
    ELSEIF ld_sel_crit CA '{}|'. "*~&`'.
      ld_finding = abap_true.
    ENDIF.

    "check for logic
    FIND ALL OCCURRENCES OF '(' IN ld_sel_crit IGNORING CASE MATCH COUNT DATA(ld_left).
    FIND ALL OCCURRENCES OF ')' IN ld_sel_crit IGNORING CASE MATCH COUNT DATA(ld_right).
    IF ld_left <> ld_right.
      ld_finding = abap_true.
    ENDIF.


    IF ld_finding = abap_true.
      cl_sal_write_event=>write_sql_query( EXPORTING id_event = 'EUU'
                                                     id_table = gd_table
                                                     id_var1  = ld_sel_crit ).

      cl_sldw_api=>get_wlist_header( EXPORTING id_name     = gd_block_list
                                     IMPORTING es_header_a = DATA(ls_head) ).
      IF ls_head IS NOT INITIAL.
        MESSAGE e000(sais) WITH 'RFC_READ_TABLE with suspect WHERE-Clause'(100) gd_table ' - ' ld_sel_crit INTO DATA(ld_buf).
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_sais_moni=>convert_to_exception( ).
      ENDIF.

    ENDIF.
    ed_sel_crit[] = it_sel_crit[].

  ENDMETHOD.


  METHOD constructor.
    DATA:
      ld_buf        TYPE string.
    "-----------------------------------------------------------------------
    gd_table = id_table.
    "cl_sais_table_auth=>get_tech_attr_table( id_tabname ).
    CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
      EXPORTING
        view_action                    = 'S'
        view_name                      = gd_table
      EXCEPTIONS
        no_authority                   = 2
        no_clientindependent_authority = 2
        no_linedependent_authority     = 2
        OTHERS                         = 1.
    IF sy-subrc = 2.
      MESSAGE e105(tb) WITH gd_table INTO ld_buf. "raise no_auth
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ELSEIF sy-subrc = 1.
      MESSAGE e131(da) WITH gd_table INTO ld_buf. "raise not_available
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDIF.

    IF id_block_list_tabl IS INITIAL.
      gd_block_list = 'RFC_READ_TABLE_TABL'.
    ELSE.
      gd_block_list = id_block_list_tabl.
    ENDIF.

    IF 0 <> cl_sldw=>check_white_list( EXPORTING id_wl_name  = gd_block_list
                                                 id_wl_ename = CONV #( id_table )
                                                 id_silent   = abap_true        ).
      MESSAGE e072(sais) WITH gd_table INTO ld_buf . "raise no_auth
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDIF.

    IF id_block_list_call IS INITIAL.
      gd_block_list_call = 'RFC_READ_TABLE_CALL'.
    ELSE.
      gd_block_list_call = id_block_list_call.
    ENDIF.

    IF 0 <> cl_sldw=>check_white_list( EXPORTING id_wl_name  = gd_block_list_call
                                                 id_wl_ename = CONV #( sy-cprog )
                                                 id_silent   = abap_true        ).
      MESSAGE e072(sais) WITH sy-cprog INTO ld_buf . "raise no_auth
      RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDIF.


    gd_delim   = id_delim.
    gd_no_data = id_no_data.
    gd_sorted  = id_sort.

  ENDMETHOD.


  METHOD convert_lines.
    DATA:
      ld_buf TYPE c LENGTH 255,
      ls_out TYPE ts_out_str.
    FIELD-SYMBOLS:
      <fs_rec>  TYPE any,
      <ft_rec>  TYPE ANY TABLE,
      <fs_comp> TYPE any,
      <fs_fld>  TYPE ts_fields_int.

    FIELD-SYMBOLS:
      <fs_comp_time>    TYPE tims,
      <fs_comp_date>    TYPE datum,

      <fs_comp_int1>    TYPE int1,
      <fs_comp_int2>    TYPE int2,
      <fs_comp_int8>    TYPE int8,
      <fs_comp_string>  TYPE string,
      <fs_comp_xstring> TYPE xstring,

      <fs_comp_dec16>   TYPE decfloat16,
      <fs_comp_dec34>   TYPE decfloat34,

      <fs_comp_c1>      TYPE char01,
      <fs_comp_c4>      TYPE char04,
      <fs_comp_c10>     TYPE char10,
      <fs_comp_c12>     TYPE char12,
      <fs_comp_c20>     TYPE char20,
      <fs_comp_c30>     TYPE char30,
      <fs_comp_c40>     TYPE char40,
      <fs_comp_c60>     TYPE xutext.
    "-----------------------------------------------------------------------
    ASSIGN ir_data->* TO <ft_rec>.

    LOOP AT <ft_rec> ASSIGNING <fs_rec> .
      CLEAR  ls_out-line.
      LOOP AT gt_fields_int ASSIGNING <fs_fld>.
        CASE <fs_fld>-type.
          WHEN 'P' OR 'N'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-type).
            WRITE  <fs_comp> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN 'D'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_date> .
            ls_out-line = ls_out-line && <fs_comp_date> && gd_delim.
          WHEN 'T'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_time>.
            ls_out-line = ls_out-line && <fs_comp_time> && gd_delim.
          WHEN 'b'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int1> .
            WRITE <fs_comp_int1> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN 's'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int2> .
            WRITE <fs_comp_int2> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN '8'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int8> .
            WRITE <fs_comp_int8> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN 'a'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_dec16> .
            WRITE <fs_comp_dec16> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN 'e'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_dec34> .
            WRITE <fs_comp_dec34> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN 'C'.
            CASE <fs_fld>-length_dst.
              WHEN 1.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c1> .
                ls_out-line = ls_out-line && <fs_comp_c1> && gd_delim.
              WHEN 4.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c4> .
                ls_out-line = ls_out-line && <fs_comp_c4> && gd_delim.
              WHEN 10.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c10> .
                ls_out-line = ls_out-line && <fs_comp_c10> && gd_delim.
              WHEN 12.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c12> .
                ls_out-line = ls_out-line && <fs_comp_c12> && gd_delim.
              WHEN 20.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c20> .
                ls_out-line = ls_out-line && <fs_comp_c20> && gd_delim.
              WHEN 30.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c30> .
                ls_out-line = ls_out-line && <fs_comp_c30> && gd_delim.
              WHEN 40.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c40> .
                ls_out-line = ls_out-line && <fs_comp_c40> && gd_delim.
              WHEN 60.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c60> .
                ls_out-line = ls_out-line && <fs_comp_c60> && gd_delim.
              WHEN OTHERS.
                TRY.
                    ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-rollname).
                  CATCH cx_sy_assign_cast_unknown_type.
                    ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-type).
                ENDTRY.
                ls_out-line = ls_out-line && <fs_comp> && gd_delim.
            ENDCASE.

          WHEN 'y'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp>.
            ls_out-line = ls_out-line && <fs_comp> && gd_delim.
          WHEN 'g'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp>.
            ls_out-line = ls_out-line && <fs_comp> && gd_delim.
          WHEN 'X'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp>.
            ld_buf = CONV #( <fs_comp> )." =  left-justified.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
          WHEN OTHERS.
            TRY.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-rollname).
              CATCH cx_sy_assign_cast_unknown_type.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-type).
            ENDTRY.
            WRITE  <fs_comp> TO ld_buf LEFT-JUSTIFIED.
            ls_out-line = ls_out-line && ld_buf && gd_delim.
        ENDCASE.
      ENDLOOP.

      IF gd_delim <> space.
        DATA(ld_length) = strlen( ls_out-line ) - 1.
        IF ld_length > 0.
          ls_out-line = ls_out-line(ld_length). " no delimiter at the end of line
        ENDIF.
      ENDIF.
      APPEND ls_out TO et_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_lines_512.
    DATA:
      ld_buf TYPE string,
      ls_512 TYPE tab512.
    FIELD-SYMBOLS:
      <ft_rec>          TYPE ANY TABLE,
      <fs_rec>          TYPE any,
      <fs_comp>         TYPE any,
      <fs_fld>          TYPE ts_fields_int,
      <fs_out>          TYPE any,
      <fs_comp_time>    TYPE tims,
      <fs_comp_date>    TYPE datum,

      <fs_comp_int1>    TYPE int1,
      <fs_comp_int2>    TYPE int2,
      <fs_comp_int8>    TYPE int8,
      <fs_comp_string>  TYPE string,
      <fs_comp_xstring> TYPE xstring,
      <fs_comp_tstmpl>  TYPE utclong,

      <fs_comp_dec16>   TYPE decfloat16,
      <fs_comp_dec34>   TYPE decfloat34,

      <fs_comp_c1>      TYPE char01,
      <fs_comp_c4>      TYPE char04,
      <fs_comp_c10>     TYPE char10,
      <fs_comp_c12>     TYPE char12,
      <fs_comp_c20>     TYPE char20,
      <fs_comp_c30>     TYPE char30,
      <fs_comp_c40>     TYPE char40,
      <fs_comp_c60>     TYPE xutext.

    "-----------------------------------------------------------------------
    ASSIGN ls_512 TO <fs_out>.
    ASSIGN ir_data->* TO <ft_rec>.
    "to be compatible to old use cases ... delete closing delimiter
    DATA(ld_length) = strlen( gd_out_pattern ) - 1.
    IF ld_length > 0.
      gd_out_pattern = gd_out_pattern(ld_length).
    ENDIF.

    LOOP AT <ft_rec> ASSIGNING <fs_rec> .

      <fs_out> = gd_out_pattern.

      LOOP AT gt_fields_int ASSIGNING <fs_fld>.
        IF ( <fs_fld>-offset_dst + <fs_fld>-length_dst ) > 512.
          MESSAGE e559(ad) WITH gd_table '512' INTO ld_buf. "DATA_BUFFER_EXCEEDED
          RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
        ENDIF.

        CASE <fs_fld>-type.
          WHEN 'P' OR 'N'.
            TRY.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-rollname).
              CATCH cx_sy_assign_cast_unknown_type.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-type).
            ENDTRY.
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = CONV string( <fs_comp> ) .

          WHEN 'D'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_date> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) =  <fs_comp_date> .
          WHEN 'T'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_time>.
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_time>.
          WHEN 'b'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int1> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_int1> .
          WHEN 's'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int2> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_int2>.
          WHEN '8'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_int8> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_int8>.
          WHEN 'a'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_dec16> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_dec16>.
          WHEN 'e'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_dec34> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_dec34>.
          WHEN 'p'.
            ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_tstmpl> .
            <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_tstmpl>.

          WHEN 'C'.
            CASE <fs_fld>-length_dst.
              WHEN 1.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c1> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c1>.
              WHEN 4.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c4> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c4>.
              WHEN 10.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c10> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c10>.
              WHEN 12.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c12> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c12> .
              WHEN 20.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c20> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c20> .
              WHEN 30.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c30> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c30>.
              WHEN 40.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c40> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c40>.
              WHEN 60.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp_c60> .
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp_c60>.
              WHEN OTHERS.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp>. " casting type (<fs_fld>-rollname).
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) = <fs_comp>.
            ENDCASE.
          WHEN 'y' OR 'g'.
            CONTINUE.
          WHEN OTHERS.
            TRY.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-rollname).
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) =  <fs_comp>.
              CATCH cx_sy_assign_cast_unknown_type.
                ASSIGN COMPONENT <fs_fld>-fieldname OF STRUCTURE <fs_rec> TO <fs_comp> CASTING TYPE (<fs_fld>-type).
                <fs_out>+<fs_fld>-offset_dst(<fs_fld>-length_dst) =  <fs_comp>.
            ENDTRY.
        ENDCASE.
      ENDLOOP.

      APPEND <fs_out> TO et_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_to_exception.

    es_evt-msgid = sy-msgid.
    es_evt-msgno = sy-msgno.
    es_evt-attr1 = sy-msgv1.
    es_evt-attr2 = sy-msgv2.
    es_evt-attr3 = sy-msgv3.
    es_evt-attr4 = sy-msgv4.

  ENDMETHOD.


  METHOD get_from_db.
    DATA:
      ld_max_row TYPE sy-dbcnt.
    FIELD-SYMBOLS:
      <ft_rec>   TYPE STANDARD TABLE.
    "-----------------------------------------------------------------------

    IF id_max_row > 0.
      ld_max_row = id_skip_rows + id_max_row.
    ELSE.
      ld_max_row = 999999999.
    ENDIF.

    CREATE DATA er_data TYPE STANDARD TABLE OF (gd_table).
    ASSIGN er_data->* TO <ft_rec>.

    TRY.
        IF gd_sorted = abap_true.
          SELECT (gd_sel_fields)
            FROM  (gd_table)
            WHERE (gd_sel_crit)
            ORDER BY PRIMARY KEY
            INTO CORRESPONDING FIELDS OF TABLE @<ft_rec>
            UP TO @ld_max_row ROWS.
        ELSE.
          SELECT (gd_sel_fields)
            FROM  (gd_table)
            WHERE (gd_sel_crit)
            INTO CORRESPONDING FIELDS OF TABLE @<ft_rec>
            UP TO @ld_max_row ROWS.
        ENDIF.

        IF id_skip_rows > 0.
          DELETE <ft_rec> TO id_skip_rows.
        ENDIF.

      CATCH cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_syntax
            cx_sy_open_sql_db.
        MESSAGE e000(sais) WITH 'DB_Error' gd_table INTO DATA(ld_buf).
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_table_content.
    DATA:
      lr_table   TYPE REF TO  data,
      ld_max_row TYPE sy-dbcnt.
    FIELD-SYMBOLS:
      <ft_rec>   TYPE STANDARD TABLE.
    "-----------------------------------------------------------------------
    CLEAR et_data.
    gd_mode = 'S'.

    TRY.
        gt_fields   = compute_field_list( EXPORTING it_fields = it_fields ).
        gd_sel_crit = compute_sel_criteria( EXPORTING it_sel_crit = it_selopt ).
      CATCH cx_sais_raise_events .
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

    et_fields = gt_fields.

    IF gd_no_data = abap_true.
      RETURN.
    ELSEIF et_fields IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lr_table = get_from_db( EXPORTING id_skip_rows = id_skip_rows
                                          id_max_row   = id_max_row ).
        IF lr_table IS NOT INITIAL.
          et_data = convert_lines( lr_table ).
        ENDIF.

      CATCH cx_sais_raise_events.
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_table_content_512.
    DATA:
      lr_table   TYPE REF TO  data,
      ld_max_row TYPE sy-dbcnt.
    FIELD-SYMBOLS:
      <ft_rec>   TYPE STANDARD TABLE.
    "-----------------------------------------------------------------------
    CLEAR:
      et_data, gt_fields.

    gd_mode = 'C'. "Classic Mode ... collect data in 512 character table

    TRY.
        gt_fields   = compute_field_list( EXPORTING it_fields = it_fields ).
        gd_sel_crit = compute_sel_criteria( EXPORTING it_sel_crit = it_selopt ).
      CATCH cx_sais_raise_events .
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

    et_fields = gt_fields.
    IF gd_no_data = abap_true.
      RETURN.
    ELSEIF et_fields IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lr_table = get_from_db( EXPORTING id_skip_rows = id_skip_rows
                                          id_max_row   = id_max_row ).
        IF lr_table IS NOT INITIAL.
          et_data = convert_lines_512( lr_table ).
        ENDIF.

      CATCH cx_sais_raise_events.
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_table_dref.
    DATA:ld_max_row TYPE sy-dbcnt.
    FIELD-SYMBOLS:
      <ft_rec>   TYPE STANDARD TABLE.
    "-----------------------------------------------------------------------
    CLEAR:
      et_data, gt_fields.

    gd_mode = 'C'. "Classic Mode ... collect data in 512 character table

    TRY.
        gt_fields   = compute_field_list( EXPORTING it_fields = it_fields ).
        gd_sel_crit = compute_sel_criteria( EXPORTING it_sel_crit = it_selopt ).
      CATCH cx_sais_raise_events .
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

    et_fields = gt_fields.
    IF gd_no_data = abap_true.
      RETURN.
    ELSEIF et_fields IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        et_data = get_from_db( EXPORTING id_skip_rows = id_skip_rows
                                          id_max_row   = id_max_row ).

      CATCH cx_sais_raise_events.
        RAISE EXCEPTION TYPE cx_sais_raise_events EXPORTING textid = cl_rfc_read_table=>convert_to_exception( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

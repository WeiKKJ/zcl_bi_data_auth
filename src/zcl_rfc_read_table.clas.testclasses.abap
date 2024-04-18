*"* use this source file for your ABAP unit test classes
"--------------------------------------------------------------------------------------------------
" definitions
"--------------------------------------------------------------------------------------------------
class ltc_test definition final for testing
                             duration long
                             risk level harmless
                             create public.
  public section.
    methods:
      test_512 for testing,
      test_long for testing.
endclass.

"--------------------------------------------------------------------------------------------------
" implementations
"--------------------------------------------------------------------------------------------------
class ltc_test implementation.
  method test_512.
    data:
      lt_content    type table of tab512,
      lt_selopt     type standard table of rfc_db_opt,
      lt_fields     type standard table of rfc_db_fld,
      lt_fields_out type standard table of rfc_db_fld,
      lo_read       type ref to cl_rfc_read_table.

    append 'OBJ_NAME' to lt_fields.
    append 'DEVCLASS' to lt_fields.
    append 'DEVCLASS = ''SDTI'' ' to  lt_selopt.

    try.
        create object lo_read
          exporting
            id_table   = 'TADIR'
            id_delim   = '|'
            id_no_data = abap_false
            id_sort    = abap_false.

        lo_read->get_table_content_512( exporting it_fields    = lt_fields[]
                                                  it_selopt    = lt_selopt[]
                                        importing et_data      = lt_content[]
                                                  et_fields    = lt_fields_out[] ).
      catch cx_sais_raise_events.
        clear lt_content.
    endtry.
    cl_abap_unit_assert=>assert_equals( exporting act = lines( lt_fields[] )
                                                  exp = lines( lt_fields_out[] )
                                                  msg = |Field List for Output wrong|
                                                  quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_number_between( exporting lower  = 10
                                                          upper  = 25
                                                          number = lines( lt_content )
                                                          msg    = |Count of Hits for TADIR Selection wrong|
                                                          quit   = if_aunit_constants=>quit-no ).

    free lo_read.
    clear: lt_fields, lt_selopt.
    try.
        create object lo_read
          exporting
            id_table   = 'RSAUPROF'
            id_delim   = '|'
            id_no_data = abap_false
            id_sort    = abap_false.

        lo_read->get_table_content_512( exporting it_fields    = lt_fields[]
                                                  it_selopt    = lt_selopt[]
                                        importing et_data      = lt_content[]
                                                  et_fields    = lt_fields_out[] ).
      catch cx_sais_raise_events.
        clear lt_content.
    endtry.
    cl_abap_unit_assert=>assert_equals( exporting act = lines( lt_fields_out[] )
                                                  exp = 12
                                                  msg = |RSAUPROF Field List for Output wrong|
                                                  quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_number_between( exporting lower  = 2
                                                          upper  = 1000
                                                          number = lines( lt_content )
                                                          msg    = |Count of Hits for RSAUPROF Selection wrong|
                                                          quit   = if_aunit_constants=>quit-no ).



  endmethod.

  method test_long.
    data:
      lt_content    type sdti_result_tab,
      lt_selopt     type standard table of rfc_db_opt,
      lt_fields     type standard table of rfc_db_fld,
      lt_fields_out type standard table of rfc_db_fld,
      lo_read       type ref to cl_rfc_read_table.

    append 'OBJ_NAME' to lt_fields.
    append 'DEVCLASS' to lt_fields.
    append 'DEVCLASS = ''SDTI'' ' to  lt_selopt.

    try.
        create object lo_read
          exporting
            id_table   = 'TADIR'
            id_delim   = '|'
            id_no_data = abap_false
            id_sort    = abap_true.

        lo_read->get_table_content( exporting id_max_row   = 100
                                              it_fields    = lt_fields[]
                                              it_selopt    = lt_selopt[]
                                    importing et_data      = lt_content[]
                                              et_fields    = lt_fields_out[] ).
      catch cx_sais_raise_events.
        clear lt_content.
    endtry.

    cl_abap_unit_assert=>assert_equals( exporting act  = lines( lt_fields[] )
                                                  exp  = lines( lt_fields_out[] )
                                                  msg  = |Field List for Output wrong|
                                                  quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_number_between( exporting lower  = 10
                                                          upper  = 25
                                                          number = lines( lt_content )
                                                          msg    = |Count of Hits for TADIR Selection wrong|
                                                          quit   = if_aunit_constants=>quit-no ).

    free lo_read.
    clear: lt_fields, lt_selopt.
    try.
        create object lo_read
          exporting
            id_table   = 'RSAUPROF'
            id_delim   = '|'
            id_no_data = abap_false
            id_sort    = abap_false.

        lo_read->get_table_content( exporting id_max_row   = 100
                                              it_fields    = lt_fields[]
                                              it_selopt    = lt_selopt[]
                                    importing et_data      = lt_content[]
                                              et_fields    = lt_fields_out[] ).
      catch cx_sais_raise_events.
        clear lt_content.
    endtry.

    cl_abap_unit_assert=>assert_equals( exporting act = lines( lt_fields_out[] )
                                                  exp = 12
                                                  msg = |RSAUPROF Field List for Output wrong|
                                                  quit = if_aunit_constants=>quit-no ).

    cl_abap_unit_assert=>assert_number_between( exporting lower  = 2
                                                          upper  = 100
                                                          number = lines( lt_content )
                                                          msg    = |Count of Hits for RSAUPROF Selection wrong|
                                                          quit   = if_aunit_constants=>quit-no ).
  endmethod.
endclass.

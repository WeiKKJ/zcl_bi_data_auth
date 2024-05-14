*----------------------------------------------------------------------*
***INCLUDE LZFG_BI_AUTHF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form compute_sel_criteria
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- OPTIONS
*&---------------------------------------------------------------------*
FORM compute_sel_criteria  tables p_options STRUCTURE rfc_db_opt.
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

  LOOP AT p_options REFERENCE INTO DATA(lr_crit).
    CONCATENATE ld_sel_crit lr_crit->text INTO ld_sel_crit. " to be downward compatible avoid the SPACE between lines
  ENDLOOP.

  "check for critical key words or char
*  FIND ALL OCCURRENCES OF REGEX ' SELECT| UPDATE| MODIFY| INSERT| DELETE| JOIN| LEFT| RIGHT| EXISTS'
  FIND ALL OCCURRENCES OF REGEX 'SELECT|UPDATE|MODIFY|INSERT|DELETE|JOIN|LEFT|RIGHT|EXISTS'
    IN TABLE p_options
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
    CLEAR:p_options[].
  ENDIF.
ENDFORM.

REPORT ztest_extended_scenarios.

DATA: gv_user_rfc    TYPE sy-uname,
      gv_user_perform TYPE sy-uname,
      gv_user_unused  TYPE sy-uname.

START-OF-SELECTION.

  " --- Scenario 1: RFC Call ---
  gv_user_rfc = sy-uname.
  CALL FUNCTION 'Z_RFC_UPDATE_USER'
    EXPORTING
      i_username = gv_user_rfc.


  " --- Scenario 2: Scope Boundary (PERFORM) ---
  gv_user_perform = sy-uname.
  PERFORM process_user_data USING gv_user_perform.


  " --- Scenario 3: Not Found (Unused) ---
  gv_user_unused = sy-uname.
  " This variable is never used in a sink.


FORM process_user_data USING p_user TYPE sy-uname.
  DATA: ls_user_log TYPE zuser_log.
  ls_user_log-uname = p_user.
  INSERT zuser_log FROM ls_user_log.
ENDFORM.

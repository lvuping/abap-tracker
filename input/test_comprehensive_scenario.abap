REPORT ztest_comprehensive_scenario.

DATA: gv_user      TYPE sy-uname,
      gv_log_user  TYPE sy-uname,
      gv_temp_user TYPE sy-uname.

TYPES: BEGIN OF ty_data,
         created_by TYPE sy-uname,
         some_data  TYPE string,
       END OF ty_data.

DATA: ls_data TYPE ty_data,
      lt_data TYPE TABLE OF ty_data.

START-OF-SELECTION.

  " 1. SY-UNAME is assigned to a variable.
  gv_user = sy-uname.

  " 2. Tainted variable is assigned to another variable for logging.
  gv_log_user = gv_user.

  " 3. Tainted variable is assigned to a temporary variable.
  gv_temp_user = gv_user.

  " 4. Assign to structure field.
  ls_data-created_by = gv_temp_user.

  " 5. The temporary variable is cleared. The taint should be removed.
  CLEAR gv_temp_user.

  " 6. Append the tainted structure to an internal table.
  " The internal table lt_data should now be tainted.
  APPEND ls_data TO lt_data.

  " 7. Use the tainted internal table to modify a Z table.
  " This is the SINK that should be detected.
  MODIFY zuser_data FROM TABLE lt_data.

  " This part of the code should not be reached if the sink is found.
  PERFORM log_user_activity USING gv_log_user.


FORM log_user_activity USING p_user TYPE sy-uname.
  " Some logging logic here...
ENDFORM.

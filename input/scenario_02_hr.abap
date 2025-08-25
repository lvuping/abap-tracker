REPORT zhr_personnel_action_processing.

*&---------------------------------------------------------------------*
*& Report ZHR_PERSONNEL_ACTION_PROCESSING
*&---------------------------------------------------------------------*
*&
*& Description: This program simulates a complex HR personnel action,
*&              such as hiring an employee. It tests SY-UNAME tracking
*&              through infotype structures (PAnnnn), complex data
*&              manipulation, and database modifications.
*&
*&---------------------------------------------------------------------*

TABLES: pa0000, pa0001, pa0002.

INFOTYPES: 0000, " Actions
           0001, " Org. Assignment
           0002. " Personal Data

DATA: gv_current_user TYPE sy-uname.

DATA: ls_p0000 TYPE pa0000,
      ls_p0001 TYPE pa0001,
      ls_p0002 TYPE pa0002.

DATA: BEGIN OF ls_new_hire_data,
        pernr    TYPE pernr_d,
        ename    TYPE ename,
        orgeh    TYPE orgeh,
        plans    TYPE plans_d,
        hire_date TYPE datum,
        action_by TYPE sy-uname,
      END OF ls_new_hire_data.

DATA: ls_log_data TYPE zhr_action_log.

START-OF-SELECTION.
  PERFORM prepare_hire_data.
  PERFORM execute_hiring_action.
  PERFORM update_organizational_assignment.
  PERFORM log_transaction.

*&---------------------------------------------------------------------*
*&      Form  prepare_hire_data
*&---------------------------------------------------------------------*
FORM prepare_hire_data.
  " Simulate fetching new hire data from an external source
  ls_new_hire_data-pernr = '99999999'.
  ls_new_hire_data-ename = 'TEST USER'.
  ls_new_hire_data-orgeh = '50001234'.
  ls_new_hire_data-plans = '60005678'.
  ls_new_hire_data-hire_date = sy-datum.
  
  " Pattern 14: Direct assignment to a structure field
  ls_new_hire_data-action_by = sy-uname.
  
  " Store the current user in a global variable for later use
  gv_current_user = sy-uname.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  execute_hiring_action
*&---------------------------------------------------------------------*
FORM execute_hiring_action.
  DATA: lv_processor TYPE uname.
  
  " Pattern 15: MOVE from a tainted structure field
  MOVE ls_new_hire_data-action_by TO lv_processor.

  " Fill PA0000 (Actions) infotype structure
  ls_p0000-pernr = ls_new_hire_data-pernr.
  ls_p0000-subty = space.
  ls_p0000-begda = ls_new_hire_data-hire_date.
  ls_p0000-endda = '99991231'.
  ls_p0000-massn = '01'. " Hiring action
  ls_p0000-massg = '01'. " Reason for action
  
  " Pattern 16: Assign tainted variable to an audit field in an infotype
  ls_p0000-uname = lv_processor.
  
  " SINK: Modify the PA0000 table
  " The UNAME field is tainted by sy-uname
  MODIFY pa0000 FROM ls_p0000.
  
  IF sy-subrc <> 0.
    MESSAGE 'Error updating PA0000' TYPE 'E'.
  ENDIF.
  
  PERFORM create_audit_log USING: 'HIRE_ACTION', lv_processor.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  update_organizational_assignment
*&---------------------------------------------------------------------*
FORM update_organizational_assignment.
  " Fill PA0001 (Org. Assignment)
  ls_p0001-pernr = ls_new_hire_data-pernr.
  ls_p0001-begda = ls_new_hire_data-hire_date.
  ls_p0001-endda = '99991231'.
  ls_p0001-orgeh = ls_new_hire_data-orgeh.
  ls_p0001-plans = ls_new_hire_data-plans.
  
  " Pattern 17: Assign from a global variable that was tainted
  ls_p0001-aenam = gv_current_user. " Changed by
  
  " SINK: Modify the PA0001 table
  " The AENAM field is tainted by sy-uname
  MODIFY pa0001 FROM ls_p0001.

  " Fill PA0002 (Personal Data)
  ls_p0002-pernr = ls_new_hire_data-pernr.
  ls_p0002-begda = ls_new_hire_data-hire_date.
  ls_p0002-endda = '99991231'.
  ls_p0002-nachn = 'USER'.
  ls_p0002-vorna = 'TEST'.
  
  " Pattern 18: Another assignment for a different audit field
  " This tests if the tracker can find multiple sinks
  DATA(lv_data_entry_clerk) = sy-uname.
  ls_p0002-aenam = lv_data_entry_clerk.
  
  " SINK: Modify the PA0002 table
  " The AENAM field is tainted by sy-uname
  MODIFY pa0002 FROM ls_p0002.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  log_transaction
*&---------------------------------------------------------------------*
FORM log_transaction.
  DATA: lt_log_table TYPE TABLE OF zhr_action_log,
        ls_log_line  TYPE zhr_action_log.

  " Pattern 19: APPEND to an internal table
  ls_log_line-pernr = ls_new_hire_data-pernr.
  ls_log_line-action_type = 'HIRE'.
  ls_log_line-executed_by = sy-uname.
  ls_log_line-executed_on = sy-datum.
  APPEND ls_log_line TO lt_log_table.
  
  " SINK: INSERT into a log table from an internal table
  " The executed_by field is tainted
  INSERT zhr_action_log FROM TABLE lt_log_table.
  
  IF sy-subrc = 0.
    COMMIT WORK.
    WRITE: / 'Hiring action for PERNR', ls_new_hire_data-pernr, 'completed.'.
  ELSE.
    ROLLBACK WORK.
    WRITE: / 'Error logging hiring action.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_audit_log
*&---------------------------------------------------------------------*
FORM create_audit_log USING iv_action_type TYPE c
                             iv_user        TYPE sy-uname.
  DATA: ls_audit TYPE zhr_audit.
  
  ls_audit-action = iv_action_type.
  ls_audit-pernr = ls_new_hire_data-pernr.
  
  " Pattern 20: Tainted variable passed via parameter
  ls_audit-changed_by = iv_user.
  ls_audit-changed_on = sy-datum.
  
  " SINK: INSERT into a separate audit log table
  INSERT zhr_audit FROM ls_audit.
ENDFORM.

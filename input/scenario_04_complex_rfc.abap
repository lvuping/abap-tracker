REPORT zbc_rfc_bdc_integration_test.

*&---------------------------------------------------------------------*
*& Report ZBC_RFC_BDC_INTEGRATION_TEST
*&---------------------------------------------------------------------*
*&
*& Description: This program tests SY-UNAME tracking through complex
*&              integration scenarios, including Remote Function Calls
*&              (RFC) and Batch Data Communication (BDC) for transaction
*&              posting.
*&
*&---------------------------------------------------------------------*

TABLES: zremote_log, zuser_mapping.

DATA: gv_rfc_destination TYPE rfcdest VALUE 'NONE'.

TYPES: BEGIN OF ty_user_data,
         sap_user  TYPE sy-uname,
         legacy_id TYPE string,
       END OF ty_user_data.

DATA: ls_user_data TYPE ty_user_data.

DATA: lt_bdcdata TYPE TABLE OF bdcdata,
      ls_bdcdata TYPE bdcdata.

START-OF-SELECTION.
  PERFORM prepare_user_data.
  PERFORM execute_remote_logon_simulation.
  PERFORM create_bdc_for_user_update.
  PERFORM process_bdc_session.

*&---------------------------------------------------------------------*
*&      Form  prepare_user_data
*&---------------------------------------------------------------------*
FORM prepare_user_data.
  " Pattern 26: Direct assignment for RFC/BDC processing
  ls_user_data-sap_user = sy-uname.
  ls_user_data-legacy_id = 'LEGACY_USER_123'.
  
  " SINK: Update a user mapping table
  " This is a direct sink before the complex scenarios start
  UPDATE zuser_mapping
     SET last_login_sap = sy-datum,
         sap_user_id    = ls_user_data-sap_user
   WHERE legacy_user_id = ls_user_data-legacy_id.
   
  IF sy-subrc <> 0.
    INSERT zuser_mapping FROM ls_user_data.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  execute_remote_logon_simulation
*&---------------------------------------------------------------------*
FORM execute_remote_logon_simulation.
  DATA: lv_remote_user TYPE sy-uname,
        lv_message     TYPE string.
        
  MOVE ls_user_data-sap_user TO lv_remote_user.

  " Pattern 27: SINK - RFC Call
  " The EXPORTING parameter 'USER' is tainted by sy-uname.
  " This is a high-priority sink as it sends data externally.
  CALL FUNCTION 'Z_LOG_REMOTE_USER_ACTIVITY'
    DESTINATION gv_rfc_destination
    EXPORTING
      user      = lv_remote_user
      activity  = 'Remote Logon Simulation'
    IMPORTING
      log_message = lv_message
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.

  IF sy-subrc = 0.
    WRITE: / 'RFC Call Successful:', lv_message.
  ELSE.
    WRITE: / 'RFC Call Failed.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_bdc_for_user_update
*&---------------------------------------------------------------------*
FORM create_bdc_for_user_update.
  DATA: lv_user_field_value TYPE bdc_fval.
  
  " Pattern 28: Tainted variable is moved to a BDC field value
  lv_user_field_value = ls_user_data-sap_user.

  " Simulate creating a BDC session for transaction SU01 (User Maintenance)
  " This is a simplified example.
  
  CLEAR lt_bdcdata.

  " Initial screen of SU01
  PERFORM bdc_dynpro      USING 'SAPLSUU5' '0050'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'SUID_ST_BNAME-BNAME'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  " Pattern 29: SINK - BDC Field Population
  " The field for the user name is populated with a tainted variable.
  " This is another high-priority sink.
  PERFORM bdc_field       USING 'SUID_ST_BNAME-BNAME'
                                lv_user_field_value.

  " Go to Address tab
  PERFORM bdc_dynpro      USING 'SAPLSUU5' '0050'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'SUID_ST_NODE_ADR6-NAME_LAST'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=CHAN'.
  
  " Update a field in the address tab (e.g., department)
  PERFORM bdc_dynpro      USING 'SAPLSZA1' '0300'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'ADDR1_DATA-DEPARTMENT'.
  PERFORM bdc_field       USING 'ADDR1_DATA-DEPARTMENT'
                                'BDC_UPDATED'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SAVE'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  process_bdc_session
*&---------------------------------------------------------------------*
FORM process_bdc_session.
  " Pattern 30: SINK - CALL TRANSACTION
  " The BDC table lt_bdcdata contains the tainted user name.
  " This will ultimately cause a database change in the user tables (USR*).
  CALL TRANSACTION 'SU01' USING lt_bdcdata
                         MODE  'N' " No screen
                         UPDATE 'S'. " Synchronous update

  IF sy-subrc = 0.
    WRITE: / 'BDC session for user', ls_user_data-sap_user, 'processed.'.
    
    " Final log after BDC processing
    DATA(ls_final_log) = VALUE zremote_log(
      log_id = 'BDC_SUCCESS',
      log_user = sy-uname,
      log_date = sy-datum
    ).
    
    " SINK: Final log entry
    INSERT zremote_log FROM ls_final_log.
    COMMIT WORK.
  ELSE.
    WRITE: / 'Error processing BDC session.'.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Helper Forms for BDC
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.

FORM bdc_field USING fnam fval.
  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fnam.
  ls_bdcdata-fval = fval.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.

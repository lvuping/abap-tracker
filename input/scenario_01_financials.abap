REPORT zfi_complex_document_posting.

*&---------------------------------------------------------------------*
*& Report ZFI_COMPLEX_DOCUMENT_POSTING
*&---------------------------------------------------------------------*
*&
*& Description: This program simulates a complex financial document
*&              posting process with intricate SY-UNAME tracking.
*&              It includes multiple FORMs, dynamic assignments, and
*&              database updates to test the tracker's capabilities.
*&
*&---------------------------------------------------------------------*

TABLES: bkpf, bseg.

DATA: gv_user         TYPE sy-uname,
      gv_timestamp    TYPE timestamp,
      gv_log_message  TYPE string.

TYPES: BEGIN OF ty_doc_header,
         bukrs TYPE bukrs, " Company Code
         belnr TYPE belnr_d, " Document Number
         gjahr TYPE gjahr,   " Fiscal Year
         blart TYPE blart,   " Document Type
         bldat TYPE bldat,   " Document Date
         budat TYPE budat,   " Posting Date
         monat TYPE monat,   " Period
         usnam TYPE usnam,   " User Name
         tcode TYPE tcode,   " Transaction Code
       END OF ty_doc_header.

TYPES: BEGIN OF ty_doc_item,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,   " Item Number
         bschl TYPE bschl,   " Posting Key
         koart TYPE koart,   " Account Type
         hkont TYPE hkont,   " G/L Account
         wrbtr TYPE wrbtr,   " Amount in DC
         sgtxt TYPE sgtxt,   " Item Text
       END OF ty_doc_item.

DATA: ls_header      TYPE ty_doc_header,
      ls_item        TYPE ty_doc_item,
      lt_items       TYPE TABLE OF ty_doc_item,
      ls_bkpf        TYPE bkpf,
      ls_bseg        TYPE bseg,
      lt_bseg        TYPE TABLE OF bseg.

DATA: BEGIN OF ls_audit_info,
        created_by TYPE sy-uname,
        created_on TYPE sy-datum,
        changed_by TYPE sy-uname,
        changed_on TYPE sy-datum,
      END OF ls_audit_info.

DATA: ls_change_log TYPE zfi_change_log.

START-OF-SELECTION.
  PERFORM initialize_variables.
  PERFORM create_document_data.
  PERFORM process_document.
  PERFORM finalize_processing.

*&---------------------------------------------------------------------*
*&      Form  initialize_variables
*&---------------------------------------------------------------------*
FORM initialize_variables.
  CLEAR: ls_header, ls_item, lt_items[], ls_bkpf, ls_bseg, lt_bseg[].
  
  " Pattern 1: Direct assignment from SY-UNAME
  ls_audit_info-created_by = sy-uname.
  ls_audit_info-created_on = sy-datum.

  " Move SY-UNAME to a local variable for further processing
  DATA(lv_current_user) = sy-uname.
  PERFORM log_user_activity USING lv_current_user 'Initialization'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_document_data
*&---------------------------------------------------------------------*
FORM create_document_data.
  " Prepare Header Data
  ls_header-bukrs = '1000'.
  ls_header-blart = 'SA'.
  ls_header-bldat = sy-datum.
  ls_header-budat = sy-datum.
  ls_header-monat = sy-datum+4(2).
  
  " Pattern 2: Assign from a structure field that was tainted
  ls_header-usnam = ls_audit_info-created_by.
  ls_header-tcode = sy-tcode.

  " Prepare Item Data (Credit)
  ls_item-buzei = '001'.
  ls_item-bschl = '50'. " Credit
  ls_item-koart = 'S'. " G/L
  ls_item-hkont = '0000113100'. " Bank Account
  ls_item-wrbtr = '1000.00'.
  ls_item-sgtxt = 'Test credit posting'.
  APPEND ls_item TO lt_items.

  " Prepare Item Data (Debit)
  ls_item-buzei = '002'.
  ls_item-bschl = '40'. " Debit
  ls_item-koart = 'S'. " G/L
  ls_item-hkont = '0000400000'. " Expense Account
  ls_item-wrbtr = '1000.00'.
  ls_item-sgtxt = 'Test debit posting'.
  APPEND ls_item TO lt_items.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  process_document
*&---------------------------------------------------------------------*
FORM process_document.
  DATA: lv_doc_creator TYPE usnam.

  " Pattern 3: MOVE statement
  MOVE ls_header-usnam TO lv_doc_creator.

  PERFORM build_bkpf_structure USING lv_doc_creator
                               CHANGING ls_bkpf.
  
  PERFORM build_bseg_table.

  IF ls_bkpf-belnr IS INITIAL.
    PERFORM post_new_document.
  ELSE.
    PERFORM update_existing_document.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_bkpf_structure
*&---------------------------------------------------------------------*
FORM build_bkpf_structure USING iv_creator TYPE usnam
                          CHANGING cs_bkpf TYPE bkpf.
  MOVE-CORRESPONDING ls_header TO cs_bkpf.
  
  " Pattern 4: Parameter-based assignment
  cs_bkpf-usnam = iv_creator.
  
  " Simulate getting a new document number
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'RF_BELEG'
    IMPORTING
      number      = cs_bkpf-belnr.
  
  ls_header-belnr = cs_bkpf-belnr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_bseg_table
*&---------------------------------------------------------------------*
FORM build_bseg_table.
  LOOP AT lt_items INTO ls_item.
    MOVE-CORRESPONDING ls_item TO ls_bseg.
    MOVE-CORRESPONDING ls_header TO ls_bseg.
    APPEND ls_bseg TO lt_bseg.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_new_document
*&---------------------------------------------------------------------*
FORM post_new_document.
  " Pattern 5: SINK - INSERT from a structure
  " The ls_bkpf-usnam field is tainted by sy-uname
  INSERT bkpf FROM ls_bkpf.
  
  IF sy-subrc = 0.
    " Pattern 6: SINK - INSERT from an internal table
    INSERT bseg FROM TABLE lt_bseg.
    
    " Log the change with the user who created it
    DATA(lv_user_for_log) = ls_bkpf-usnam.
    PERFORM create_change_log USING 'CREATE' lv_user_for_log.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  update_existing_document
*&---------------------------------------------------------------------*
FORM update_existing_document.
  DATA: ls_audit_trail TYPE zfi_audit_trail.
  
  " Before updating, create an audit trail
  ls_audit_trail-belnr = ls_bkpf-belnr.
  ls_audit_trail-gjahr = ls_bkpf-gjahr.
  ls_audit_trail-changed_on = sy-datum.
  ls_audit_trail-changed_at = sy-uzeit.
  
  " Pattern 7: Complex CONCATENATE
  CONCATENATE 'Document updated by' sy-uname INTO ls_audit_trail-change_reason SEPARATED BY space.
  
  " Pattern 8: SINK - The field ls_audit_trail-changed_by is tainted via CONCATENATE
  " This is an indirect taint.
  SPLIT ls_audit_trail-change_reason AT 'by' INTO DATA(lv_text) ls_audit_trail-changed_by.
  
  INSERT zfi_audit_trail FROM ls_audit_trail.

  " Now, update the document header
  " Pattern 9: SINK - UPDATE statement with tainted field
  " We need a new variable to simulate a change by a different user for logging
  DATA(lv_modifier) = sy-uname.
  
  UPDATE bkpf SET xblnr = 'MANUAL_UPDATE'
                 bktxt = 'Updated by background job'
                 aenam = lv_modifier " AENAM is an audit field
           WHERE belnr = ls_bkpf-belnr
             AND gjahr = ls_bkpf-gjahr.
             
  PERFORM log_user_activity USING lv_modifier 'Document Update'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  finalize_processing
*&---------------------------------------------------------------------*
FORM finalize_processing.
  " Pattern 10: MOVE from SY-UNAME to a structure for final logging
  ls_change_log-uname = sy-uname.
  ls_change_log-timestamp = sy-datum.
  ls_change_log-program = sy-repid.
  ls_change_log-message = 'Processing completed successfully'.
  
  " Pattern 11: SINK - MODIFY a custom log table
  MODIFY zfi_run_log FROM ls_change_log.
  
  COMMIT WORK.
  WRITE: / 'Document', ls_header-belnr, 'processed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  log_user_activity
*&---------------------------------------------------------------------*
FORM log_user_activity USING iv_user TYPE sy-uname iv_activity TYPE string.
  DATA: ls_log TYPE zapp_log.
  
  ls_log-user_id = iv_user.
  ls_log-activity = iv_activity.
  GET TIME STAMP FIELD gv_timestamp.
  ls_log-log_time = gv_timestamp.
  
  " Pattern 12: SINK - INSERT into a generic application log
  INSERT INTO zapp_log VALUES ls_log.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_change_log
*&---------------------------------------------------------------------*
FORM create_change_log USING iv_action TYPE c iv_user TYPE usnam.
  DATA: ls_log_entry TYPE zfi_change_log.
  
  ls_log_entry-action = iv_action.
  ls_log_entry-uname = iv_user.
  ls_log_entry-timestamp = sy-datum.
  
  " Pattern 13: SINK - Another MODIFY
  MODIFY zfi_change_log FROM ls_log_entry.
ENDFORM.

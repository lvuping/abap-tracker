REPORT zbc_mega_business_process_sim.

*&---------------------------------------------------------------------*
*& Report ZBC_MEGA_BUSINESS_PROCESS_SIM
*&---------------------------------------------------------------------*
*&
*& Description: A comprehensive and extremely complex simulation of an
*&              end-to-end business process involving SD, MM, FI, and HR
*&              modules. This report is designed to rigorously test the
*&              SY-UNAME tracker with long variable chains, multiple
*&              independent taint paths, unused variable declarations,
*&              and various complex ABAP patterns.
*&              Approximate length: 600 lines.
*&
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                        TABLES AND GLOBAL DATA                        *
*----------------------------------------------------------------------*
TABLES: vbak, vbap, likp, lips, vbrk, vbrp, bkpf, bseg, mard, pa0105.

*--- Global Variables
DATA: gv_current_processor   TYPE sy-uname,
      gv_process_start_time  TYPE timestamp,
      gv_log_handle          TYPE balloghndl.

*--- Unused variable as per user request to test negative path
DATA: gv_unused_user         TYPE sy-uname.

*--- Complex Structures for Data Handling
TYPES: BEGIN OF ty_sales_order_data,
         customer_id TYPE kunnr,
         material_id TYPE matnr,
         quantity    TYPE kwmeng,
         created_by  TYPE ernam,
       END OF ty_sales_order_data.

TYPES: BEGIN OF ty_process_control,
         run_id      TYPE guid_32,
         is_test_run TYPE abap_bool,
         user_id     TYPE sy-uname,
       END OF ty_process_control.

*--- Internal Tables
DATA: gt_sales_data TYPE TABLE OF ty_sales_order_data,
      gt_bdc_data   TYPE TABLE OF bdcdata.

*--- Global Structures
DATA: gs_process_ctrl TYPE ty_process_control,
      gs_sales_input  TYPE ty_sales_order_data.

*----------------------------------------------------------------------*
*                           SELECTION SCREEN                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_cust  TYPE kunnr DEFAULT '0000001234',
            p_mat   TYPE matnr DEFAULT 'RM-1001',
            p_qty   TYPE kwmeng DEFAULT 10.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*                         MAIN PROCESSING BLOCK                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM initialize_process.
  PERFORM execute_sales_cycle.
  PERFORM execute_hr_subprocess.
  PERFORM finalize_process.

*&---------------------------------------------------------------------*
*&      Form  initialize_process
*&---------------------------------------------------------------------*
FORM initialize_process.
  " Pattern 31: Assign SY-UNAME to a global control structure
  gs_process_ctrl-user_id = sy-uname.
  gs_process_ctrl-is_test_run = abap_true.
  GET TIME STAMP FIELD gv_process_start_time.

  " Move from control structure to a global variable for processing
  gv_current_processor = gs_process_ctrl-user_id.

  " Prepare input data from selection screen
  gs_sales_input-customer_id = p_cust.
  gs_sales_input-material_id = p_mat.
  gs_sales_input-quantity    = p_qty.
  
  " Pattern 32: Tainted global variable assigned to structure
  gs_sales_input-created_by  = gv_current_processor.

  APPEND gs_sales_input TO gt_sales_data.
  
  " Initialize application log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_slog = VALUE bal_s_log( object = 'Z_MEGA_PROC' )
    IMPORTING
      e_log_handle = gv_log_handle.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  execute_sales_cycle
*&---------------------------------------------------------------------*
FORM execute_sales_cycle.
  DATA: lv_sales_doc  TYPE vbeln_va,
        lv_delivery_doc TYPE vbeln_vl,
        lv_billing_doc TYPE vbeln_vf.

  PERFORM create_sales_order
    USING    gt_sales_data
    CHANGING lv_sales_doc.

  IF lv_sales_doc IS NOT INITIAL.
    PERFORM create_delivery
      USING    lv_sales_doc
      CHANGING lv_delivery_doc.
  ENDIF.

  IF lv_delivery_doc IS NOT INITIAL.
    PERFORM post_goods_issue
      USING lv_delivery_doc
            gs_sales_input-material_id
            gs_sales_input-quantity.
            
    PERFORM create_billing_document
      USING    lv_delivery_doc
      CHANGING lv_billing_doc.
  ENDIF.
  
  IF lv_billing_doc IS NOT INITIAL.
    PERFORM create_accounting_document
      USING lv_billing_doc
            gs_process_ctrl-user_id. " Pass tainted user
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_sales_order
*&---------------------------------------------------------------------*
FORM create_sales_order USING it_sales_data TYPE TABLE OF ty_sales_order_data
                        CHANGING cv_sales_doc TYPE vbeln_va.
  DATA: ls_vbak TYPE vbak,
        lt_vbap TYPE TABLE OF vbap,
        ls_vbap TYPE vbap,
        ls_data TYPE ty_sales_order_data.

  LOOP AT it_sales_data INTO ls_data.
    " Simulate number assignment
    cv_sales_doc = '0000001001'.

    " Header Data
    ls_vbak-vbeln = cv_sales_doc.
    ls_vbak-auart = 'OR'.
    ls_vbak-vkorg = '1000'.
    ls_vbak-vtweg = '10'.
    ls_vbak-spart = '00'.
    ls_vbak-kunnr = ls_data-customer_id.
    
    " Pattern 33: Long chain - tainted value from loop variable
    ls_vbak-ernam = ls_data-created_by.
    ls_vbak-erdat = sy-datum.

    " Item Data
    ls_vbap-vbeln = cv_sales_doc.
    ls_vbap-posnr = '000010'.
    ls_vbap-matnr = ls_data-material_id.
    ls_vbap-kwmeng = ls_data-quantity.
    APPEND ls_vbap TO lt_vbap.

    " SINK: INSERT into sales order header table
    INSERT vbak FROM ls_vbak.
    " SINK: INSERT into sales order item table
    INSERT vbap FROM TABLE lt_vbap.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_delivery
*&---------------------------------------------------------------------*
FORM create_delivery USING iv_sales_doc TYPE vbeln_va
                     CHANGING cv_delivery_doc TYPE vbeln_vl.
  DATA: ls_likp TYPE likp,
        ls_lips TYPE lips.
        
  cv_delivery_doc = '0080001002'.
  
  ls_likp-vbeln = cv_delivery_doc.
  ls_likp-lfart = 'LF'.
  ls_likp-kunnr = p_cust.
  
  " Pattern 34: Indirect taint. The creator of the delivery is the
  " current system user, not the one from the sales order.
  " This creates a new, independent taint path.
  DATA(lv_delivery_creator) = sy-uname.
  ls_likp-ernam = lv_delivery_creator.
  
  " SINK: INSERT into delivery header
  INSERT likp FROM ls_likp.
  
  ls_lips-vbeln = cv_delivery_doc.
  ls_lips-posnr = '000010'.
  ls_lips-vgbel = iv_sales_doc.
  ls_lips-vgpos = '000010'.
  ls_lips-matnr = p_mat.
  
  " SINK: INSERT into delivery item
  INSERT lips FROM ls_lips.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_goods_issue
*&---------------------------------------------------------------------*
FORM post_goods_issue USING iv_delivery_doc TYPE vbeln_vl
                             iv_material     TYPE matnr
                             iv_quantity     TYPE kwmeng.
  DATA: ls_mard TYPE mard,
        lv_user_for_log TYPE sy-uname.

  " Pattern 35: Another direct assignment for logging purposes
  lv_user_for_log = sy-uname.

  SELECT SINGLE * FROM mard INTO ls_mard
    WHERE matnr = iv_material AND werks = '1000' AND lgort = '0001'.
    
  IF sy-subrc = 0.
    ls_mard-labst = ls_mard-labst - iv_quantity.
    
    " SINK: UPDATE stock table. The data itself is not tainted,
    " but this action is often logged with a user ID.
    UPDATE mard FROM ls_mard.
    
    PERFORM log_material_movement
      USING iv_delivery_doc
            iv_material
            lv_user_for_log. " Pass tainted user to logging routine
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  log_material_movement
*&---------------------------------------------------------------------*
FORM log_material_movement USING iv_doc TYPE vbeln_vl
                                  iv_mat TYPE matnr
                                  iv_user TYPE sy-uname.
  DATA: ls_log TYPE zmm_movement_log.
  
  ls_log-doc_nr = iv_doc.
  ls_log-matnr = iv_mat.
  ls_log-log_date = sy-datum.
  
  " Pattern 36: Tainted variable received via parameter
  ls_log-processed_by = iv_user.
  
  " SINK: MODIFY a custom log table
  MODIFY zmm_movement_log FROM ls_log.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_billing_document
*&---------------------------------------------------------------------*
FORM create_billing_document USING iv_delivery_doc TYPE vbeln_vl
                             CHANGING cv_billing_doc TYPE vbeln_vf.
  DATA: ls_vbrk TYPE vbrk.
  
  cv_billing_doc = '0090001003'.
  
  ls_vbrk-vbeln = cv_billing_doc.
  ls_vbrk-fkart = 'F2'.
  ls_vbrk-kunag = p_cust.
  
  " Pattern 37: Re-using the global tainted variable from the start
  ls_vbrk-ernam = gv_current_processor.
  
  " SINK: INSERT into billing header
  INSERT vbrk FROM ls_vbrk.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_accounting_document
*&---------------------------------------------------------------------*
FORM create_accounting_document USING iv_billing_doc TYPE vbeln_vf
                                      iv_user        TYPE sy-uname.
  DATA: ls_bkpf TYPE bkpf,
        ls_bseg TYPE bseg,
        lt_bseg TYPE TABLE OF bseg,
        lv_acc_doc TYPE belnr_d.
        
  lv_acc_doc = '0100001004'.
  
  " Header
  ls_bkpf-belnr = lv_acc_doc.
  ls_bkpf-bukrs = '1000'.
  ls_bkpf-gjahr = sy-datum(4).
  ls_bkpf-bldat = sy-datum.
  ls_bkpf-budat = sy-datum.
  
  " Pattern 38: Tainted user passed via parameter
  ls_bkpf-usnam = iv_user.
  
  " SINK: INSERT into accounting document header
  INSERT bkpf FROM ls_bkpf.
  
  " Items (simplified)
  ls_bseg-belnr = lv_acc_doc.
  ls_bseg-gjahr = sy-datum(4).
  ls_bseg-buzei = 1.
  APPEND ls_bseg TO lt_bseg.
  
  " SINK: INSERT into accounting document items
  INSERT bseg FROM TABLE lt_bseg.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  execute_hr_subprocess
*&---------------------------------------------------------------------*
FORM execute_hr_subprocess.
  " This FORM simulates a completely separate business process
  " to test multiple independent taint paths.
  DATA: lt_employees TYPE TABLE OF pa0001,
        ls_employee  TYPE pa0001.
        
  SELECT * FROM pa0001 INTO TABLE lt_employees UP TO 10 ROWS.
  
  LOOP AT lt_employees INTO ls_employee.
    PERFORM update_employee_comm_data
      USING ls_employee-pernr.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  update_employee_comm_data
*&---------------------------------------------------------------------*
FORM update_employee_comm_data USING iv_pernr TYPE pernr_d.
  DATA: ls_p0105 TYPE pa0105,
        lv_hr_admin TYPE sy-uname.
        
  " Pattern 39: A new, local taint source for this subprocess
  lv_hr_admin = sy-uname.
  
  SELECT SINGLE * FROM pa0105 INTO ls_p0105
    WHERE pernr = iv_pernr AND subty = '0010'. " Email
    
  IF sy-subrc = 0.
    " Update existing record
    ls_p0105-usrid = 'new.email@company.com'.
    
    " Pattern 40: Tainted variable assigned to audit field
    ls_p0105-aenam = lv_hr_admin.
    
    " SINK: MODIFY infotype table
    MODIFY pa0105 FROM ls_p0105.
  ELSE.
    " Create new record
    CLEAR ls_p0105.
    ls_p0105-pernr = iv_pernr;
    ls_p0105-subty = '0010';
    ls_p0105-begda = sy-datum.
    ls_p0105-endda = '99991231'.
    ls_p0105-usrid = 'test.email@company.com'.
    ls_p0105-aenam = lv_hr_admin.
    
    " SINK: INSERT into infotype table
    INSERT pa0105 FROM ls_p0105.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  finalize_process
*&---------------------------------------------------------------------*
FORM finalize_process.
  DATA: ls_log_msg TYPE bal_s_msg.
  
  ls_log_msg-msgty = 'S'.
  ls_log_msg-msgid = 'Z_MSG'.
  ls_log_msg-msgno = '001'.
  CONCATENATE 'Process completed by user' gv_current_processor
    INTO ls_log_msg-msgv1 SEPARATED BY space.
    
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = gv_log_handle
      i_s_msg      = ls_log_msg.
      
  COMMIT WORK.
  WRITE: / 'Mega process simulation completed.'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  unused_routine
*&---------------------------------------------------------------------*
FORM unused_routine.
  " This FORM is intentionally not called to test if the analyzer
  " correctly ignores dead code paths.
  DATA: lv_temp_user TYPE sy-uname.
  lv_temp_user = sy-uname.
  
  UPDATE z_some_other_table SET user = lv_temp_user.
ENDFORM.

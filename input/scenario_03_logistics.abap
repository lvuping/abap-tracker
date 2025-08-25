REPORT zmm_goods_movement_processor.

*&---------------------------------------------------------------------*
*& Report ZMM_GOODS_MOVEMENT_PROCESSOR
*&---------------------------------------------------------------------*
*&
*& Description: This program simulates a goods receipt process, similar
*&              to transaction MIGO. It focuses on SY-UNAME tracking
*&              in UPDATE and MODIFY statements for material documents
*&              (MKPF, MSEG) and inventory tables.
*&
*&---------------------------------------------------------------------*

TABLES: mkpf, mseg, mard.

DATA: gv_processor_id TYPE sy-uname.

TYPES: BEGIN OF ty_goods_receipt,
         mblnr    TYPE mblnr, " Material Document
         mjahr    TYPE mjahr, " Material Doc. Year
         matnr    TYPE matnr, " Material Number
         werks    TYPE werks_d, " Plant
         lgort    TYPE lgort_d, " Storage Location
         menge    TYPE menge_d, " Quantity
         erfmg    TYPE erfmg, " Qty in Unit of Entry
         erfme    TYPE erfme, " Unit of Entry
         posted_by TYPE sy-uname,
       END OF ty_goods_receipt.

DATA: ls_receipt_data TYPE ty_goods_receipt,
      ls_mkpf         TYPE mkpf,
      ls_mseg         TYPE mseg,
      lt_mseg         TYPE TABLE OF mseg.

DATA: ls_stock_data TYPE mard.

START-OF-SELECTION.
  PERFORM initialize_user.
  PERFORM prepare_receipt_data.
  PERFORM process_goods_receipt.
  PERFORM update_stock_levels.
  PERFORM finalize_and_log.

*&---------------------------------------------------------------------*
*&      Form  initialize_user
*&---------------------------------------------------------------------*
FORM initialize_user.
  " Pattern 21: Assign SY-UNAME to a global variable
  gv_processor_id = sy-uname.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  prepare_receipt_data
*&---------------------------------------------------------------------*
FORM prepare_receipt_data.
  " Simulate input data for a goods receipt
  ls_receipt_data-matnr = 'RM-1001'.
  ls_receipt_data-werks = '1000'.
  ls_receipt_data-lgort = '0001'.
  ls_receipt_data-menge = 100.
  ls_receipt_data-erfmg = 100.
  ls_receipt_data-erfme = 'EA'.
  
  " Pattern 22: Assign from the global tainted variable
  ls_receipt_data-posted_by = gv_processor_id.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  process_goods_receipt
*&---------------------------------------------------------------------*
FORM process_goods_receipt.
  DATA: lv_user TYPE usnam.
  
  MOVE ls_receipt_data-posted_by TO lv_user.

  " Create Material Document Header (MKPF)
  ls_mkpf-mblnr = '4900000001'. " Simulated number
  ls_mkpf-mjahr = sy-datum(4).
  ls_mkpf-bldat = sy-datum.
  ls_mkpf-budat = sy-datum.
  ls_mkpf-cputm = sy-uzeit.
  
  " Pattern 23: Tainted variable assigned to USNAM field
  ls_mkpf-usnam = lv_user.
  
  " SINK: INSERT into MKPF
  INSERT mkpf FROM ls_mkpf.

  " Create Material Document Item (MSEG)
  ls_mseg-mblnr = ls_mkpf-mblnr.
  ls_mseg-mjahr = ls_mkpf-mjahr.
  ls_mseg-zeile = 1.
  MOVE-CORRESPONDING ls_receipt_data TO ls_mseg.
  APPEND ls_mseg TO lt_mseg.
  
  " SINK: INSERT into MSEG
  INSERT mseg FROM TABLE lt_mseg.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  update_stock_levels
*&---------------------------------------------------------------------*
FORM update_stock_levels.
  DATA: lv_stock_manager TYPE sy-uname.
  
  " Pattern 24: Another direct assignment for a different purpose
  lv_stock_manager = sy-uname.

  SELECT SINGLE * FROM mard
    INTO ls_stock_data
    WHERE matnr = ls_receipt_data-matnr
      AND werks = ls_receipt_data-werks
      AND lgort = ls_receipt_data-lgort.
      
  IF sy-subrc = 0.
    " Stock exists, update it
    ls_stock_data-labst = ls_stock_data-labst + ls_receipt_data-menge.
    
    " SINK: UPDATE MARD table
    " The WHERE condition is not tainted, but the SET statement could be.
    " Here, we are just updating stock, but we could add a log field.
    UPDATE mard SET labst = ls_stock_data-labst
      WHERE matnr = ls_stock_data-matnr
        AND werks = ls_stock_data-werks
        AND lgort = ls_stock_data-lgort.
        
    " Now, let's create a more complex update scenario for a log table
    PERFORM update_material_log USING lv_stock_manager.
  ELSE.
    " Stock doesn't exist, create it
    ls_stock_data-matnr = ls_receipt_data-matnr.
    ls_stock_data-werks = ls_receipt_data-werks.
    ls_stock_data-lgort = ls_receipt_data-lgort.
    ls_stock_data-labst = ls_receipt_data-menge.
    
    " SINK: INSERT into MARD
    INSERT mard FROM ls_stock_data.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  update_material_log
*&---------------------------------------------------------------------*
FORM update_material_log USING iv_user TYPE sy-uname.
  DATA: ls_log TYPE zmm_material_log.
  
  " SINK: UPDATE a log table where the SET value is tainted
  " This is a very important pattern to detect.
  UPDATE zmm_material_log
     SET last_changed_by = iv_user,
         last_change_on  = sy-datum
   WHERE matnr = ls_receipt_data-matnr.
   
  IF sy-subrc <> 0.
    " If no log exists, create one
    ls_log-matnr = ls_receipt_data-matnr.
    ls_log-last_changed_by = iv_user.
    ls_log-last_change_on = sy-datum.
    
    " SINK: INSERT into the log table
    INSERT zmm_material_log FROM ls_log.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  finalize_and_log
*&---------------------------------------------------------------------*
FORM finalize_and_log.
  DATA: ls_process_log TYPE zmm_process_log.
  
  " Pattern 25: WHERE condition with a tainted variable
  " This is a read operation, but it's important for tracking data flow.
  SELECT SINGLE * FROM zmm_process_log
    INTO ls_process_log
    WHERE processor = gv_processor_id
      AND process_date = sy-datum.
      
  IF sy-subrc = 0.
    " Log entry for this user and date already exists, update it
    ls_process_log-last_transaction = sy-tcode.
    ls_process_log-last_doc = ls_mkpf-mblnr.
    
    " SINK: MODIFY statement where the record was found via a tainted key
    MODIFY zmm_process_log FROM ls_process_log.
  ELSE.
    " Create a new log entry
    ls_process_log-processor = gv_processor_id.
    ls_process_log-process_date = sy-datum.
    ls_process_log-last_transaction = sy-tcode.
    ls_process_log-last_doc = ls_mkpf-mblnr.
    
    " SINK: INSERT statement with a tainted field
    INSERT zmm_process_log FROM ls_process_log.
  ENDIF.
  
  COMMIT WORK.
  WRITE: / 'Goods receipt', ls_mkpf-mblnr, 'processed successfully.'.
ENDFORM.

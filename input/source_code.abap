*&---------------------------------------------------------------------*
*& Report Z_VENDOR_PURCHASE_ORDER_LIST
*&---------------------------------------------------------------------*
*&
*& Description: This report displays a list of purchase orders for a
*&              vendor associated with the current user's ID. It calls
*&              an RFC to get additional vendor details from a remote
*&              system.
*&
*&---------------------------------------------------------------------*
REPORT z_vendor_purchase_order_list.

*----------------------------------------------------------------------*
* TYPE-POOLS Declaration
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* TABLES Declaration for Selection Screen
*----------------------------------------------------------------------*
TABLES: ekko, lfa1.

*----------------------------------------------------------------------*
* DATA Structures and Internal Tables
*----------------------------------------------------------------------*
*-- Structure for the final output list
TYPES: BEGIN OF ty_po_list,
         ebeln TYPE ekko-ebeln, " Purchase Order Number
         bukrs TYPE ekko-bukrs, " Company Code
         bsart TYPE ekko-bsart, " PO Document Type
         aedat TYPE ekko-aedat, " PO Creation Date
         lifnr TYPE ekko-lifnr, " Vendor Number
         name1 TYPE lfa1-name1, " Vendor Name
         remote_info TYPE c LENGTH 100, " Additional info from RFC
       END OF ty_po_list.

*-- Standard table for the final output
DATA: gt_po_list TYPE STANDARD TABLE OF ty_po_list,
      gs_po_list TYPE ty_po_list.

*-- Field catalog for ALV display
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE slis_fieldcat_alv.

*-- Global variables
DATA: gv_rfc_message TYPE bapiret2-message.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bukrs TYPE ekko-bukrs OBLIGATORY, " Company Code
              p_ekorg TYPE ekko-ekorg OBLIGATORY. " Purchasing Organization
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* INITIALIZATION Event
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.

*----------------------------------------------------------------------*
* START-OF-SELECTION Event (Main Logic)
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM get_data.
  IF gt_po_list IS NOT INITIAL.
    PERFORM display_data.
  ELSE.
    MESSAGE 'No data found for the given criteria.' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
* This subroutine fetches the main data.
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lt_ekko TYPE STANDARD TABLE OF ekko.
  DATA: lv_lifnr TYPE lifnr. " Declare the variable for vendor number

  "--------------------------------------------------------------------"
  " 1. Assign current user ID (sy-uname) to the vendor variable.
  "    In a real scenario, there might be a custom table or logic
  "    to map user IDs to vendor numbers. Here, we assume a direct link
  "    for demonstration purposes.
  "--------------------------------------------------------------------"
  lv_lifnr = sy-uname.

  "--------------------------------------------------------------------"
  " 2. Select Purchase Order Headers from EKKO table.
  "--------------------------------------------------------------------"
  SELECT ebeln
         bukrs
         bsart
         aedat
         lifnr
    FROM ekko
    INTO TABLE lt_ekko
    WHERE bukrs = @p_bukrs
      AND ekorg = @p_ekorg
      AND lifnr = @lv_lifnr.

  IF sy-subrc = 0.
    SORT lt_ekko BY ebeln.

    "------------------------------------------------------------------"
    " 3. Loop through the PO data and get additional details.
    "------------------------------------------------------------------"
    LOOP AT lt_ekko INTO DATA(ls_ekko).
      CLEAR: gs_po_list.

      gs_po_list-ebeln = ls_ekko-ebeln.
      gs_po_list-bukrs = ls_ekko-bukrs.
      gs_po_list-bsart = ls_ekko-bsart.
      gs_po_list-aedat = ls_ekko-aedat.
      gs_po_list-lifnr = ls_ekko-lifnr.

      " Get Vendor Name from LFA1
      SELECT SINGLE name1
        FROM lfa1
        INTO gs_po_list-name1
        WHERE lifnr = ls_ekko-lifnr.

      "----------------------------------------------------------------"
      " 4. Call RFC to get additional information from a remote system.
      "    The variable 'lv_lifnr' is passed as the 3rd parameter.
      "----------------------------------------------------------------"
      PERFORM call_remote_system
        USING p_bukrs
              p_ekorg
              lv_lifnr " Passing lv_lifnr as the 3rd parameter
        CHANGING gs_po_list-remote_info
                 gv_rfc_message.

      " If RFC call failed, display a message but continue processing.
      IF gv_rfc_message IS NOT INITIAL.
        gs_po_list-remote_info = gv_rfc_message.
      ENDIF.

      APPEND gs_po_list TO gt_po_list.
    ENDLOOP.
  ENDIF.

ENDFORM. " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_REMOTE_SYSTEM
*&---------------------------------------------------------------------*
* This subroutine calls the RFC function module.
*----------------------------------------------------------------------*
FORM call_remote_system USING    value(iv_bukrs) TYPE bukrs
                                 value(iv_ekorg) TYPE ekorg
                                 value(iv_lifnr) TYPE lifnr
                        CHANGING cv_remote_info  TYPE c
                                 cv_rfc_message  TYPE bapiret2-message.

  DATA: lv_info TYPE c LENGTH 100.

  " Clear the changing parameters before the call
  CLEAR: cv_remote_info, cv_rfc_message.

  " ** RFC Call **
  " 'RFC_DESTINATION' should be replaced with the actual RFC destination
  " configured in transaction SM59.
  CALL FUNCTION 'Z_RFC_GET_VENDOR_REMOTE_INFO'
    DESTINATION 'YOUR_RFC_DESTINATION'
    EXPORTING
      i_company_code    = iv_bukrs " 1st Parameter
      i_purch_org       = iv_ekorg " 2nd Parameter
      i_vendor_number   = iv_lifnr " 3rd Parameter (Value from sy-uname)
    IMPORTING
      e_vendor_add_info = lv_info
    EXCEPTIONS
      system_failure        = 1 MESSAGE gv_rfc_message
      communication_failure = 2 MESSAGE gv_rfc_message
      resource_failure      = 3
      vendor_not_found      = 4
      OTHERS                = 5.

  CASE sy-subrc.
    WHEN 0.
      " RFC call was successful
      cv_remote_info = lv_info.
    WHEN 1.
      " System Failure: message is already in gv_rfc_message
      cv_rfc_message = |System Failure: { gv_rfc_message }|.
    WHEN 2.
      " Communication Failure: message is already in gv_rfc_message
      cv_rfc_message = |Communication Failure: { gv_rfc_message }|.
    WHEN 3.
      cv_rfc_message = 'Resource failure in remote system.'.
    WHEN 4.
      cv_rfc_message = 'Vendor not found in the remote system.'.
    WHEN OTHERS.
      cv_rfc_message = 'An unexpected error occurred during RFC call.'.
  ENDCASE.

ENDFORM. " CALL_REMOTE_SYSTEM

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
* This subroutine displays the final data in an ALV Grid.
*----------------------------------------------------------------------*
FORM display_data.

  DATA: ls_layout TYPE slis_layout_alv.

  " Build field catalog dynamically
  PERFORM build_field_catalog.

  " Set ALV Layout properties
  ls_layout-zebra = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-window_titlebar = 'Vendor Purchase Order List'.

  " Call ALV Grid display function module
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_po_list
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM. " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
* This subroutine prepares the field catalog for ALV.
*----------------------------------------------------------------------*
FORM build_field_catalog.
  DATA: lv_colpos TYPE i.
  CLEAR gt_fieldcat.

  lv_colpos = 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'EBELN' 'Purchase Order' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'BUKRS' 'Company Code' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'BSART' 'PO Type' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'AEDAT' 'Created On' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'LIFNR' 'Vendor' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'NAME1' 'Vendor Name' 'GT_PO_LIST'.

  lv_colpos = lv_colpos + 1.
  PERFORM add_field_to_catalog
    USING lv_colpos 'REMOTE_INFO' 'Info from Remote System' 'GT_PO_LIST'.

ENDFORM. " BUILD_FIELD_CATALOG

*&---------------------------------------------------------------------*
*&      Form  ADD_FIELD_TO_CATALOG
*&---------------------------------------------------------------------*
* Helper routine to populate field catalog internal table.
*----------------------------------------------------------------------*
FORM add_field_to_catalog USING p_colpos    TYPE i
                                p_fieldname TYPE slis_fieldname
                                p_text      TYPE slis_reptext
                                p_tabname   TYPE slis_tabname.

  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = p_colpos.
  gs_fieldcat-fieldname = p_fieldname.
  gs_fieldcat-seltext_m = p_text.
  gs_fieldcat-tabname   = p_tabname.
  APPEND gs_fieldcat TO gt_fieldcat.

ENDFORM. " ADD_FIELD_TO_CATALOG
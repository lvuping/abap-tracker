*&---------------------------------------------------------------------*
*& Report Z_EXTENDED_VARIABLE_FLOW_TEST
*&---------------------------------------------------------------------*
*& 다양한 ABAP 할당 패턴을 테스트하기 위한 확장된 샘플 코드
*&---------------------------------------------------------------------*
REPORT z_extended_variable_flow_test.

*----------------------------------------------------------------------*
* DATA Declarations
*----------------------------------------------------------------------*
DATA: lv_original_user TYPE sy-uname,
      lv_temp_user     TYPE string,
      lv_final_user    TYPE string,
      lv_user_info     TYPE string,
      lv_formatted     TYPE string,
      lv_prefix        TYPE string,
      lv_suffix        TYPE string.

*-- Structure for user data
TYPES: BEGIN OF ty_user_data,
         user_id      TYPE sy-uname,
         display_name TYPE string,
         email        TYPE string,
         department   TYPE string,
       END OF ty_user_data.

DATA: gs_user_data   TYPE ty_user_data,
      gs_backup_data TYPE ty_user_data,
      gt_user_list   TYPE TABLE OF ty_user_data.

*----------------------------------------------------------------------*
* INITIALIZATION Event
*----------------------------------------------------------------------*
INITIALIZATION.

  "--------------------------------------------------------------------"
  " Test Case 1: Basic Assignment Patterns
  "--------------------------------------------------------------------"
  
  " 1.1 Direct assignment from sy-uname
  lv_original_user = sy-uname.
  
  " 1.2 MOVE statement
  MOVE lv_original_user TO lv_temp_user.
  
  " 1.3 Structure field assignment
  gs_user_data-user_id = lv_temp_user.

  "--------------------------------------------------------------------"
  " Test Case 2: CONCATENATE Patterns
  "--------------------------------------------------------------------"
  
  " 2.1 Simple concatenation
  CONCATENATE 'USER_' lv_original_user INTO lv_user_info.
  
  " 2.2 Multiple source concatenation
  CONCATENATE 'Dept:' 'IT' 'User:' lv_temp_user INTO lv_formatted SEPARATED BY '_'.
  
  " 2.3 Concatenation with structure field
  CONCATENATE gs_user_data-user_id '@company.com' INTO gs_user_data-email.

  "--------------------------------------------------------------------"
  " Test Case 3: MOVE-CORRESPONDING Pattern
  "--------------------------------------------------------------------"
  
  " 3.1 Move corresponding between structures
  MOVE-CORRESPONDING gs_user_data TO gs_backup_data.

  "--------------------------------------------------------------------"
  " Test Case 4: SPLIT Patterns
  "--------------------------------------------------------------------"
  
  " 4.1 Split user info
  SPLIT lv_user_info AT '_' INTO lv_prefix lv_suffix.
  
  " 4.2 Split email address
  SPLIT gs_user_data-email AT '@' INTO gs_user_data-display_name lv_temp_user.

  "--------------------------------------------------------------------"
  " Test Case 5: REPLACE Patterns  
  "--------------------------------------------------------------------"
  
  " 5.1 Replace in string variable
  REPLACE 'USER_' IN lv_user_info WITH lv_original_user.
  
  " 5.2 Replace all occurrences
  REPLACE ALL OCCURRENCES OF 'temp' IN lv_formatted WITH gs_backup_data-user_id.

  "--------------------------------------------------------------------"
  " Test Case 6: Complex Expression Assignments
  "--------------------------------------------------------------------"
  
  " 6.1 Compute statement
  COMPUTE lv_temp_user = lv_original_user && '_COMPUTED'.
  
  " 6.2 Direct calculation assignment
  lv_final_user = gs_user_data-user_id && '_' && sy-datum.

  "--------------------------------------------------------------------"
  " Test Case 7: PERFORM Calls with Various Parameters
  "--------------------------------------------------------------------"
  
  " 7.1 PERFORM with USING
  PERFORM validate_user_access USING lv_original_user.
  
  " 7.2 PERFORM with multiple parameters
  PERFORM process_user_data USING gs_user_data-user_id 
                                  lv_formatted
                            CHANGING gs_user_data-department.
  
  " 7.3 PERFORM with structure
  PERFORM log_user_activity USING gs_backup_data.

  "--------------------------------------------------------------------"
  " Test Case 8: SELECT Statements with Variables
  "--------------------------------------------------------------------"
  
  " 8.1 SELECT with WHERE clause using tainted variable
  SELECT SINGLE bname 
    FROM usr02 
    INTO lv_temp_user 
    WHERE bname = @lv_original_user.
  
  " 8.2 SELECT into table with user filter
  SELECT * 
    FROM usr02 
    INTO TABLE @DATA(lt_users) 
    WHERE bname = @gs_user_data-user_id.

  "--------------------------------------------------------------------"
  " Test Case 9: RFC Function Calls
  "--------------------------------------------------------------------"
  
  " 9.1 RFC call with direct variable
  CALL FUNCTION 'Z_USER_AUTHORIZATION_CHECK'
    DESTINATION 'RFC_SYSTEM'
    EXPORTING
      i_username    = lv_original_user
      i_system_info = lv_formatted
    IMPORTING
      e_auth_status = DATA(lv_auth_result).
  
  " 9.2 RFC call with structure field
  CALL FUNCTION 'Z_LOG_USER_ACTIVITY' 
    EXPORTING
      i_user_id     = gs_backup_data-user_id
      i_activity    = 'LOGIN'
      i_timestamp   = sy-uzeit
    EXCEPTIONS
      user_not_found = 1
      system_error   = 2
      OTHERS         = 3.
  
  " 9.3 RFC call with concatenated variable
  CALL FUNCTION 'Z_SEND_USER_NOTIFICATION'
    EXPORTING
      i_recipient   = gs_user_data-email
      i_message     = lv_user_info
      i_priority    = 'HIGH'.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_USER_ACCESS
*&---------------------------------------------------------------------*
FORM validate_user_access USING iv_username TYPE sy-uname.
  
  DATA: lv_check_user TYPE sy-uname.
  
  " Move parameter to local variable
  lv_check_user = iv_username.
  
  " Call validation RFC
  CALL FUNCTION 'Z_VALIDATE_USER_PERMISSIONS'
    EXPORTING
      i_user_to_check = lv_check_user
    IMPORTING
      e_is_valid      = DATA(lv_valid).
      
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_USER_DATA
*&---------------------------------------------------------------------*
FORM process_user_data USING    iv_user_id    TYPE sy-uname
                                iv_info       TYPE string
                       CHANGING cv_department TYPE string.
  
  DATA: lv_processing_user TYPE sy-uname,
        lv_dept_info       TYPE string.
  
  " Process the user data
  lv_processing_user = iv_user_id.
  
  " Build department info
  CONCATENATE 'DEPT_FOR_' lv_processing_user INTO lv_dept_info.
  cv_department = lv_dept_info.
  
  " Final RFC call in subroutine
  CALL FUNCTION 'Z_UPDATE_USER_DEPARTMENT'
    EXPORTING
      i_username   = lv_processing_user
      i_department = cv_department.
      
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LOG_USER_ACTIVITY
*&---------------------------------------------------------------------*
FORM log_user_activity USING is_user_data TYPE ty_user_data.
  
  DATA: lv_log_user TYPE sy-uname.
  
  " Extract user from structure
  lv_log_user = is_user_data-user_id.
  
  " Log via RFC
  CALL FUNCTION 'Z_ACTIVITY_LOGGER'
    EXPORTING
      i_user_id    = lv_log_user
      i_timestamp  = sy-uzeit
      i_action     = 'DATA_PROCESSING'.
      
ENDFORM.

#SingleInstance, Force
SetTitleMatchMode, 2 ; Match partial window titles

; ###########################################
; ## SAP RFC Call Script - Z_USER_INFO      ##
; ## Connects to SAP GUI and calls RFC      ##
; ## with USER_ID = TEST parameter          ##
; ###########################################

; Hotkey: Ctrl + U to execute the RFC call
^u::
    ; -------------------- 1. Connect to SAP Session --------------------
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.`nPlease login to SAP and run the script again.
        Return
    }
    
    MsgBox, 64, Connected, Successfully connected to SAP GUI session.`nPreparing to call RFC Z_USER_INFO...
    
    ; -------------------- 2. Create RFC Function Control --------------------
    Try {
        ; Create COM object for RFC calls (Unicode system)
        FunctionCtrl := ComObjCreate("SAP.Functions.Unicode")
        FunctionCtrl.Connection := SAP_Session.Connection
        
        ; Add the Z_USER_INFO function module
        UserInfoFunc := FunctionCtrl.Add("Z_USER_INFO")
        
        ; Set the import parameter USER_ID to "TEST"
        UserInfoFunc.Exports("USER_ID").Value := "TEST"
        
        MsgBox, 64, RFC Setup, RFC Function Z_USER_INFO configured.`nCalling with USER_ID = TEST...
        
    } Catch e {
        MsgBox, 16, Error, Failed to setup RFC function:`n%e.Message%
        Return
    }
    
    ; -------------------- 3. Call the RFC Function --------------------
    Try {
        ; Call the function
        If (UserInfoFunc.Call() = True) {
            MsgBox, 64, Success, RFC Z_USER_INFO called successfully!`n`nChecking for return values...
            
            ; Try to retrieve any export parameters if they exist
            ; Note: You may need to adjust these based on actual Z_USER_INFO export parameters
            Try {
                ; Example of reading potential export parameters
                ; Uncomment and adjust parameter names as needed:
                ; userName := UserInfoFunc.Imports("USER_NAME").Value
                ; userDept := UserInfoFunc.Imports("DEPARTMENT").Value
                ; MsgBox, 64, Results, User Info Retrieved:`nName: %userName%`nDepartment: %userDept%
                
                MsgBox, 64, Complete, RFC call completed successfully.`nCheck SAP system for any results.
            } Catch {
                MsgBox, 64, Info, RFC executed but no export parameters to display.
            }
        } else {
            Throw, UserInfoFunc.Exception
        }
    } Catch e {
        MsgBox, 16, Error, RFC call failed:`n%e.Message%`n`nPlease verify that Z_USER_INFO exists in the SAP system.
        Return
    }
    
Return

; ############### SAP GUI Session Connection Function ###############
Connect_To_SAP_Session() {
    Try {
        ; Get SAP GUI scripting object
        SapGuiAuto := ComObjGet("SAPGUI")
        application := SapGuiAuto.GetScriptingEngine
        
        ; Connect to first available connection and session
        connection := application.Children(0)
        session := connection.Children(0)
        
        Return session
    } Catch e {
        ; Return empty if connection fails
        Return ""
    }
}

; ############### Alternative: Direct RFC Call Without GUI ###############
; This function can be called directly without GUI interaction
; Usage: CallRFC_Z_USER_INFO("TEST")
CallRFC_Z_USER_INFO(userId) {
    Try {
        ; Get active SAP session
        SAP_Session := Connect_To_SAP_Session()
        If (!IsObject(SAP_Session)) {
            Return "Error: No SAP session"
        }
        
        ; Setup and call RFC
        FunctionCtrl := ComObjCreate("SAP.Functions.Unicode")
        FunctionCtrl.Connection := SAP_Session.Connection
        UserInfoFunc := FunctionCtrl.Add("Z_USER_INFO")
        UserInfoFunc.Exports("USER_ID").Value := userId
        
        If (UserInfoFunc.Call() = True) {
            ; Collect any results here if needed
            Return "Success"
        } else {
            Return "Call failed"
        }
    } Catch e {
        Return "Error: " . e.Message
    }
}

; ############### Information Display ###############
; Hotkey: Ctrl + I to show script information
^i::
    MsgBox, 64, SAP RFC Script Info, 
    (
    SAP RFC Call Script - Z_USER_INFO
    ================================
    
    Hotkeys:
    - Ctrl+U : Call Z_USER_INFO RFC with USER_ID=TEST
    - Ctrl+I : Show this information
    - Ctrl+Q : Exit script
    
    Requirements:
    1. SAP GUI must be running and logged in
    2. SAP GUI Scripting must be enabled
    3. Z_USER_INFO RFC must exist in SAP system
    
    The script will:
    1. Connect to active SAP session
    2. Call Z_USER_INFO with USER_ID="TEST"
    3. Display results if available
    )
Return

; ############### Exit Script ###############
; Hotkey: Ctrl + Q to exit
^q::
    MsgBox, 36, Exit, Do you want to exit the SAP RFC script?
    IfMsgBox Yes
        ExitApp
Return
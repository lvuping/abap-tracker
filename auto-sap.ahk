#SingleInstance, Force
SetTitleMatchMode, 2 ; Match partial window titles

; ###########################################
; ## SAP RFC Call Script - STFC_CONNECTION  ##
; ## Connects to SAP GUI and calls standard ##
; ## test RFC that exists in all SAP systems##
; ###########################################

; Hotkey: Ctrl + U to execute the RFC call
^u::
    ; -------------------- 1. Connect to SAP Session --------------------
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.`nPlease login to SAP and run the script again.
        Return
    }
    
    MsgBox, 64, Connected, Successfully connected to SAP GUI session.`nPreparing to call standard test RFC...
    
    ; -------------------- 2. Setup and Call STFC_CONNECTION (Standard Test RFC) --------------------
    ; STFC_CONNECTION is a standard SAP test RFC that exists in all systems
    ; It simply echoes back the text you send to it
    
    Try {
        ; Create the Functions object
        sapFunc := ComObjCreate("SAP.Functions")
        sapFunc.Connection := SAP_Session.Connection
        
        ; Add the standard test function
        testFunc := sapFunc.Add("STFC_CONNECTION")
        
        ; Set the import parameter - send a test message
        testFunc.exports.Item("REQUTEXT").Value := "Hello from AutoHotkey"
        
        ; Call the function
        result := testFunc.Call()
        
        If (result = True) {
            ; Get the response
            echoText := testFunc.imports.Item("ECHOTEXT").Value
            respText := testFunc.imports.Item("RESPTEXT").Value
            
            MsgBox, 64, Success!, RFC Test Successful!`n`nSent: Hello from AutoHotkey`nEcho: %echoText%`nResponse: %respText%
        } else {
            MsgBox, 16, Error, RFC call returned false
        }
        
    } Catch e {
        ; If the above doesn't work, try an even simpler approach
        Try {
            MsgBox, 64, Info, First method failed. Trying alternative approach...
            
            ; Alternative simpler syntax
            sapFunc2 := SAP_Session.Connection.Functions
            testFunc2 := sapFunc2.Add("STFC_CONNECTION")
            testFunc2.exports.Item("REQUTEXT").Value := "Test Message"
            
            If (testFunc2.Call() = True) {
                echoText2 := testFunc2.imports.Item("ECHOTEXT").Value
                MsgBox, 64, Success!, Alternative method worked!`nEcho: %echoText2%
            } else {
                MsgBox, 16, Error, Both RFC methods failed. Check SAP GUI Scripting is enabled.
            }
            
        } Catch e2 {
            MsgBox, 16, Error, Failed to call RFC.`n`nTroubleshooting:`n1. Ensure SAP GUI Scripting is enabled`n2. Check SAP Logon Options > Accessibility & Scripting > Enable scripting`n3. Verify you're logged into SAP
        }
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

; ############### Alternative: Direct RFC Test Function ###############
; This function can be called directly without GUI interaction
; Usage: TestRFC_Connection("Your test message")
TestRFC_Connection(testMessage) {
    Try {
        ; Get active SAP session
        SAP_Session := Connect_To_SAP_Session()
        If (!IsObject(SAP_Session)) {
            Return "Error: No SAP session"
        }
        
        ; Setup and call RFC
        sapFunc := ComObjCreate("SAP.Functions")
        sapFunc.Connection := SAP_Session.Connection
        testFunc := sapFunc.Add("STFC_CONNECTION")
        testFunc.exports("REQUTEXT").Value := testMessage
        
        If (testFunc.Call() = True) {
            echoResult := testFunc.imports("ECHOTEXT").Value
            Return "Success: " . echoResult
        } else {
            Return "Call failed"
        }
    } Catch e {
        Return "Error: RFC call failed"
    }
}

; ############### Information Display ###############
; Hotkey: Ctrl + I to show script information
^i::
    MsgBox, 64, SAP RFC Script Info, 
    (
    SAP RFC Test Script - STFC_CONNECTION
    =====================================
    
    Hotkeys:
    - Ctrl+U : Test RFC connection using STFC_CONNECTION
    - Ctrl+I : Show this information
    - Ctrl+Q : Exit script
    
    Requirements:
    1. SAP GUI must be running and logged in
    2. SAP GUI Scripting must be enabled
       (SAP Logon > Options > Accessibility & Scripting)
    
    The script will:
    1. Connect to active SAP session
    2. Call STFC_CONNECTION (standard test RFC)
    3. Display echo response from SAP
    
    STFC_CONNECTION is a standard SAP RFC that exists
    in all SAP systems for testing connectivity.
    )
Return

; ############### Exit Script ###############
; Hotkey: Ctrl + Q to exit
^q::
    MsgBox, 36, Exit, Do you want to exit the SAP RFC script?
    IfMsgBox Yes
        ExitApp
Return
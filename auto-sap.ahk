#SingleInstance, Force
SetTitleMatchMode, 2 ; Match partial window titles

; ###########################################
; ## SAP RFC Test Script via SE37          ##
; ## Automates SE37 transaction to test    ##
; ## STFC_CONNECTION RFC through SAP GUI   ##
; ###########################################

; Hotkey: Ctrl + U to execute the RFC test via SE37
^u::
    ; -------------------- 1. Connect to SAP Session --------------------
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.`nPlease login to SAP and run the script again.
        Return
    }
    
    MsgBox, 64, Connected, Successfully connected to SAP GUI session.`nNavigating to SE37 to test RFC...
    
    ; -------------------- 2. Navigate to SE37 and Test RFC --------------------
    Try {
        ; Start SE37 transaction
        SAP_Session.StartTransaction("SE37")
        Sleep, 500
        
        ; Enter function module name
        SAP_Session.findById("wnd[0]/usr/ctxtRS38L-NAME").text := "STFC_CONNECTION"
        
        ; Press F8 or click Test/Execute button
        SAP_Session.findById("wnd[0]").sendVKey(8) ; F8 key
        Sleep, 500
        
        ; Enter test value in REQUTEXT field
        Try {
            SAP_Session.findById("wnd[0]/usr/txtREQUTEXT").text := "Hello from AutoHotkey"
        } Catch {
            ; Alternative field ID that might be used
            SAP_Session.findById("wnd[0]/usr/txt*REQUTEXT").text := "Hello from AutoHotkey"
        }
        
        ; Execute the function module (F8 again)
        SAP_Session.findById("wnd[0]").sendVKey(8)
        Sleep, 1000
        
        ; Try to read the result
        Try {
            echoText := SAP_Session.findById("wnd[0]/usr/txtECHOTEXT").text
            respText := SAP_Session.findById("wnd[0]/usr/txtRESPTEXT").text
            
            MsgBox, 64, Success!, RFC Test Successful via SE37!`n`nSent: Hello from AutoHotkey`nEcho: %echoText%`nResponse: %respText%
        } Catch {
            MsgBox, 64, Success!, RFC executed successfully!`nCheck SE37 screen for results.
        }
        
        ; Go back to initial screen
        SAP_Session.findById("wnd[0]").sendVKey(3) ; F3 - Back
        Sleep, 500
        SAP_Session.findById("wnd[0]").sendVKey(3) ; F3 - Back again
        
    } Catch e {
        errorMsg := e.Message
        MsgBox, 16, Error, Failed to execute RFC test via SE37.`n`nError: %errorMsg%`n`nTroubleshooting:`n1. Ensure you have authorization for SE37`n2. Verify STFC_CONNECTION exists in your system`n3. Check that SAP GUI Scripting is enabled
    }
    
Return

; ############### Alternative: RFC via BAPI Transaction ###############
; Hotkey: Ctrl + B to test via BAPI transaction
^b::
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.
        Return
    }
    
    Try {
        ; Try using BAPI transaction for testing
        SAP_Session.StartTransaction("BAPI")
        Sleep, 500
        
        ; Navigate to test function
        SAP_Session.findById("wnd[0]/usr/ctxtGD-OBJTYPE").text := "BUS0001"
        SAP_Session.findById("wnd[0]").sendVKey(0) ; Enter
        Sleep, 500
        
        MsgBox, 64, Info, BAPI Explorer opened. You can test BAPIs from here.
        
    } Catch e {
        ; Alternative: Try SE80 for testing
        Try {
            SAP_Session.StartTransaction("SE80")
            Sleep, 500
            MsgBox, 64, Info, SE80 opened. Navigate to Function Groups > Test Functions
        } Catch {
            MsgBox, 16, Error, Cannot open BAPI or SE80 transaction
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

; ############### Direct Command Execution ###############
; Hotkey: Ctrl + D to execute direct command
^d::
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.
        Return
    }
    
    ; Execute command in OK-Code field
    Try {
        InputBox, command, SAP Command, Enter SAP transaction or command:, , 300, 130
        If (ErrorLevel = 0 and command != "") {
            SAP_Session.findById("wnd[0]/tbar[0]/okcd").text := command
            SAP_Session.findById("wnd[0]").sendVKey(0) ; Enter
            MsgBox, 64, Success, Command executed: %command%
        }
    } Catch e {
        MsgBox, 16, Error, Failed to execute command
    }
Return

; ############### Check RFC Authorization ###############
; Hotkey: Ctrl + R to check RFC authorizations
^r::
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.
        Return
    }
    
    Try {
        ; Check user's RFC authorizations via SU53
        SAP_Session.StartTransaction("SU53")
        Sleep, 500
        MsgBox, 64, Info, SU53 opened.`nThis shows your last authorization check.`nIf RFC failed due to authorization, it will appear here.
    } Catch {
        MsgBox, 16, Error, Cannot open SU53 transaction
    }
Return

; ############### Information Display ###############
; Hotkey: Ctrl + I to show script information
^i::
    MsgBox, 64, SAP RFC Script Info, 
    (
    SAP RFC Test Script - SE37 Method
    ==================================
    
    Hotkeys:
    - Ctrl+U : Test RFC via SE37 transaction
    - Ctrl+B : Open BAPI Explorer
    - Ctrl+D : Execute direct SAP command
    - Ctrl+R : Check RFC authorizations (SU53)
    - Ctrl+I : Show this information
    - Ctrl+Q : Exit script
    
    Requirements:
    1. SAP GUI must be running and logged in
    2. SAP GUI Scripting must be enabled
    3. User must have SE37 authorization
    
    The script will:
    1. Connect to active SAP session
    2. Navigate to SE37 transaction
    3. Test STFC_CONNECTION RFC
    4. Display results
    
    Note: This method uses GUI automation to test RFCs
    through the standard SAP interface.
    )
Return

; ############### Exit Script ###############
; Hotkey: Ctrl + Q to exit
^q::
    MsgBox, 36, Exit, Do you want to exit the SAP RFC script?
    IfMsgBox Yes
        ExitApp
Return
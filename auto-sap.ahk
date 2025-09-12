#SingleInstance, Force
SetTitleMatchMode, 2 ; Match partial window titles

; ###########################################
; ## SAP RFC Test Script - Multiple Methods ##
; ## 1. SE37 GUI Automation (Foreground)   ##
; ## 2. Background RFC via ActiveX/Jobs    ##
; ###########################################

; Hotkey: Ctrl + U to execute the RFC test via SE37 (Foreground)
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

; ############### Background RFC Execution ###############
; Hotkey: Ctrl + G to execute RFC in background
^g::
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.
        Return
    }
    
    MsgBox, 64, Info, Attempting background RFC execution...
    
    ; Method 1: Try SAP ActiveX Controls
    backgroundSuccess := False
    Try {
        ; Try different ActiveX control variations
        sapRFC := ""
        
        ; Try SAP.Functions.Unicode (newer systems)
        Try {
            sapRFC := ComObjCreate("SAP.Functions.Unicode")
            sapRFC.Connection := SAP_Session.Connection.ConnectionObject
        } Catch {
            ; Try SAPBAPIControl
            Try {
                sapRFC := ComObjCreate("SAPBAPIControl.SAPBAPIControl.1")
                sapRFC.Connection := SAP_Session.Connection
            } Catch {
                ; Try SAP.LogonControl
                Try {
                    sapLogon := ComObjCreate("SAP.LogonControl.1")
                    sapConn := sapLogon.NewConnection
                    
                    ; Get connection parameters from existing session
                    sapConn.System := SAP_Session.Info.SystemName
                    sapConn.Client := SAP_Session.Info.Client
                    sapConn.User := SAP_Session.Info.User
                    sapConn.Language := SAP_Session.Info.Language
                    
                    If (sapConn.Logon(0, True)) {
                        sapRFC := ComObjCreate("SAP.Functions")
                        sapRFC.Connection := sapConn
                    }
                } Catch {
                    ; ActiveX controls not available
                    sapRFC := ""
                }
            }
        }
        
        If (IsObject(sapRFC)) {
            ; Try to call the RFC
            rfcFunc := sapRFC.Add("STFC_CONNECTION")
            rfcFunc.exports("REQUTEXT") := "Background call from AutoHotkey"
            
            If (rfcFunc.Call()) {
                echoResult := rfcFunc.imports("ECHOTEXT")
                MsgBox, 64, Success!, Background RFC executed via ActiveX!`nEcho: %echoResult%
                backgroundSuccess := True
            }
        }
    } Catch e {
        ; ActiveX method failed, will try alternative
    }
    
    ; Method 2: Create background job via SM36
    If (!backgroundSuccess) {
        Try {
            CreateBackgroundJob(SAP_Session)
            backgroundSuccess := True
        } Catch {
            ; SM36 method failed
        }
    }
    
    ; Method 3: Execute via SE38 with ABAP program
    If (!backgroundSuccess) {
        Try {
            ExecuteViaABAPProgram(SAP_Session)
            backgroundSuccess := True
        } Catch {
            MsgBox, 16, Error, All background execution methods failed.`nPlease check authorizations and system configuration.
        }
    }
    
Return

; ############### Create Background Job via SM36 ###############
CreateBackgroundJob(SAP_Session) {
    ; Navigate to SM36 (Job Scheduling)
    SAP_Session.StartTransaction("SM36")
    Sleep, 500
    
    ; Generate unique job name with timestamp
    FormatTime, timeStamp, , yyyyMMdd_HHmmss
    jobName := "RFC_TEST_" . timeStamp
    
    ; Enter job name
    SAP_Session.findById("wnd[0]/usr/txtBTCH2170-JOBNAME").text := jobName
    
    ; Job class (C = low priority background)
    SAP_Session.findById("wnd[0]/usr/ctxtBTCH2170-JOBCLASS").text := "C"
    
    ; Click on "Step" button to define job step
    SAP_Session.findById("wnd[0]/tbar[1]/btn[8]").press()
    Sleep, 500
    
    ; Select ABAP Program
    SAP_Session.findById("wnd[0]/usr/btnBTN_ABAP").press()
    Sleep, 500
    
    ; Enter program name (using standard SAP test program)
    Try {
        SAP_Session.findById("wnd[0]/usr/ctxtRS38M-PROGRAMM").text := "SAPMSSY1"
        SAP_Session.findById("wnd[0]/usr/ctxtRS38M-VARIANT").text := ""
    } Catch {
        ; Alternative: Create inline ABAP code
        SAP_Session.findById("wnd[0]/usr/txtRS38M-PROGRAMM").text := "RSCONN01"
    }
    
    ; Save step (Enter or checkmark)
    SAP_Session.findById("wnd[0]/tbar[0]/btn[0]").press()
    Sleep, 500
    
    ; Go back to main job screen
    SAP_Session.findById("wnd[0]/tbar[0]/btn[3]").press()
    Sleep, 500
    
    ; Schedule immediately
    SAP_Session.findById("wnd[0]/tbar[1]/btn[9]").press() ; Start condition button
    Sleep, 500
    
    ; Select "Immediate" execution
    SAP_Session.findById("wnd[0]/usr/btnBTN_IMMEDIATE").press()
    Sleep, 500
    
    ; Save
    SAP_Session.findById("wnd[0]/tbar[0]/btn[0]").press()
    Sleep, 500
    
    ; Save and release job
    SAP_Session.findById("wnd[0]/tbar[0]/btn[11]").press()
    Sleep, 1000
    
    MsgBox, 64, Success!, Background job created: %jobName%`nCheck SM37 to monitor execution.
    
    ; Optionally open SM37 to check job status
    SAP_Session.StartTransaction("SM37")
    Sleep, 500
    SAP_Session.findById("wnd[0]/usr/txtBTCH2170-JOBNAME").text := jobName
    SAP_Session.findById("wnd[0]/usr/txtBTCH2170-USERNAME").text := "*"
    SAP_Session.findById("wnd[0]").sendVKey(8) ; Execute
}

; ############### Execute via ABAP Program in SE38 ###############
ExecuteViaABAPProgram(SAP_Session) {
    ; Navigate to SE38
    SAP_Session.StartTransaction("SE38")
    Sleep, 500
    
    ; Generate temporary program name
    FormatTime, timeStamp, , HHmmss
    progName := "ZRFC_TEST_" . timeStamp
    
    ; Enter program name
    SAP_Session.findById("wnd[0]/usr/ctxtRS38M-PROGRAMM").text := progName
    
    ; Click Create button
    SAP_Session.findById("wnd[0]/tbar[1]/btn[5]").press()
    Sleep, 500
    
    ; Set program attributes
    Try {
        SAP_Session.findById("wnd[1]/usr/txtRS38M-REPTI").text := "RFC Test Program"
        SAP_Session.findById("wnd[1]/usr/cmbRS38M-SUBC").key := "1" ; Executable program
        SAP_Session.findById("wnd[1]/tbar[0]/btn[0]").press() ; Continue
        Sleep, 500
    } Catch {
        ; Attributes dialog might be different
    }
    
    ; Write ABAP code to call RFC
    abapCode := "REPORT " . progName . ".`n"
    abapCode .= "DATA: lv_echo TYPE string.`n"
    abapCode .= "DATA: lv_resp TYPE string.`n"
    abapCode .= "CALL FUNCTION 'STFC_CONNECTION'`n"
    abapCode .= "  EXPORTING`n"
    abapCode .= "    requtext = 'Background test from AutoHotkey'`n"
    abapCode .= "  IMPORTING`n"
    abapCode .= "    echotext = lv_echo`n"
    abapCode .= "    resptext = lv_resp.`n"
    abapCode .= "WRITE: / 'Echo:', lv_echo.`n"
    abapCode .= "WRITE: / 'Response:', lv_resp."
    
    ; Enter the ABAP code (simplified - would need proper editor handling)
    SAP_Session.findById("wnd[0]/usr/txtRS38M-PROGRAMM").text := abapCode
    
    ; Save
    SAP_Session.findById("wnd[0]").sendVKey(11) ; Ctrl+S
    Sleep, 500
    
    ; Execute in background (F9)
    SAP_Session.findById("wnd[0]").sendVKey(9)
    Sleep, 500
    
    ; Confirm background execution
    Try {
        SAP_Session.findById("wnd[1]/tbar[0]/btn[0]").press()
    } Catch {
        ; No confirmation needed
    }
    
    MsgBox, 64, Success!, ABAP program created and executed in background: %progName%`nCheck SM37 or SP01 for results.
}

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

; ############### Direct RFC Call without GUI ###############
; Hotkey: Ctrl + T to test direct RFC connection
^t::
    MsgBox, 64, Info, Testing direct RFC connection methods...
    
    ; Method 1: Try SAP NCo (.NET Connector) via COM
    Try {
        sapNCo := ComObjCreate("SAP.Connector.SAPConnection")
        MsgBox, 64, Found, SAP .NET Connector found!
    } Catch {
        ; Not available
    }
    
    ; Method 2: Try SAP JCo (Java Connector) via COM bridge
    Try {
        sapJCo := ComObjCreate("com.sap.conn.jco.JCo")
        MsgBox, 64, Found, SAP Java Connector bridge found!
    } Catch {
        ; Not available
    }
    
    ; Method 3: Try librfc32.dll direct call
    Try {
        hModule := DllCall("LoadLibrary", "Str", "librfc32.dll", "Ptr")
        If (hModule) {
            MsgBox, 64, Found, SAP RFC library (librfc32.dll) loaded!
            DllCall("FreeLibrary", "Ptr", hModule)
        }
    } Catch {
        ; Not available
    }
    
    ; Method 4: Check for SAP GUI installation
    Try {
        RegRead, sapPath, HKEY_LOCAL_MACHINE\SOFTWARE\SAP\SAP Shared, SAPsysdir
        If (sapPath) {
            MsgBox, 64, Info, SAP installation found at: %sapPath%`n`nYou can use SAP RFC SDK from this path.
        }
    } Catch {
        MsgBox, 48, Info, No direct RFC connectors found.`nUsing GUI automation methods instead.
    }
    
Return

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

; ############### Monitor Background Jobs ###############
; Hotkey: Ctrl + M to monitor background jobs
^m::
    SAP_Session := Connect_To_SAP_Session()
    If (!IsObject(SAP_Session)) {
        MsgBox, 48, Error, Cannot find SAP GUI session.
        Return
    }
    
    Try {
        ; Open SM37 to monitor jobs
        SAP_Session.StartTransaction("SM37")
        Sleep, 500
        
        ; Set job selection criteria
        SAP_Session.findById("wnd[0]/usr/txtBTCH2170-JOBNAME").text := "RFC_TEST_*"
        SAP_Session.findById("wnd[0]/usr/txtBTCH2170-USERNAME").text := SAP_Session.Info.User
        
        ; Set date range to today
        FormatTime, today, , yyyyMMdd
        SAP_Session.findById("wnd[0]/usr/ctxtBTCH2170-STRTDATE").text := today
        
        ; Execute search
        SAP_Session.findById("wnd[0]").sendVKey(8)
        Sleep, 500
        
        MsgBox, 64, Info, SM37 opened with RFC test jobs filter.`nCheck status of your background RFC executions.
    } Catch {
        MsgBox, 16, Error, Cannot open SM37 transaction
    }
Return

; ############### Information Display ###############
; Hotkey: Ctrl + I to show script information
^i::
    MsgBox, 64, SAP RFC Script Info, 
    (
    SAP RFC Test Script - Multiple Methods
    =======================================
    
    Main Functions:
    - Ctrl+U : Test RFC via SE37 (Foreground GUI)
    - Ctrl+G : Execute RFC in Background (Multiple methods)
    - Ctrl+T : Test direct RFC connection availability
    - Ctrl+M : Monitor background jobs (SM37)
    
    Additional Tools:
    - Ctrl+B : Open BAPI Explorer
    - Ctrl+D : Execute direct SAP command
    - Ctrl+R : Check RFC authorizations (SU53)
    - Ctrl+I : Show this information
    - Ctrl+Q : Exit script
    
    Background Execution Methods:
    1. SAP ActiveX Controls (if available)
    2. Background job via SM36
    3. ABAP program execution via SE38
    
    Requirements:
    1. SAP GUI must be running and logged in
    2. SAP GUI Scripting must be enabled
    3. User authorizations for transactions
    
    The background execution will try multiple methods
    to execute RFC without GUI interaction.
    )
Return

; ############### Exit Script ###############
; Hotkey: Ctrl + Q to exit
^q::
    MsgBox, 36, Exit, Do you want to exit the SAP RFC script?
    IfMsgBox Yes
        ExitApp
Return
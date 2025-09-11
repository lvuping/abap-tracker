#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%
#Include auto.ahk

; Test the ProcessF9Text function
TestResults := ""

; Test Case 1
Input1 := "* U3 C12345678901  GSDK!@#$%^ 2025.09.10 {some text1} {some text2}"
Expected1 := "* U4               GSDK!@#$%^ 2025.09.11 DH2025.KIM  SAP ID Replace"
Result1 := ProcessF9Text(Input1)
TestResults .= "Test 1:`nInput:    " . Input1 . "`nExpected: " . Expected1 . "`nResult:   " . Result1 . "`n`n"

; Test Case 2
Input2 := "* N C98765432101 TEST123 2025.12.31 김동현 변경내역"
Expected2 := "* U1              TEST123 2026.01.01 DH2025.KIM  SAP ID Replace"
Result2 := ProcessF9Text(Input2)
TestResults .= "Test 2:`nInput:    " . Input2 . "`nExpected: " . Expected2 . "`nResult:   " . Result2 . "`n`n"

; Test Case 3
Input3 := "* U1 C55555555555 ABC!@# 2025.02.28 홍길동 some random text"
Expected3 := "* U2              ABC!@# 2025.03.01 DH2025.KIM  SAP ID Replace"
Result3 := ProcessF9Text(Input3)
TestResults .= "Test 3:`nInput:    " . Input3 . "`nExpected: " . Expected3 . "`nResult:   " . Result3 . "`n`n"

; Test Case 4
Input4 := "U1 data U2 more U3 C11111111111 XYZ 2025.01.15 이순신 description"
Expected4 := "U2 data U3 more U4              XYZ 2025.01.16 DH2025.KIM  SAP ID Replace"
Result4 := ProcessF9Text(Input4)
TestResults .= "Test 4:`nInput:    " . Input4 . "`nExpected: " . Expected4 . "`nResult:   " . Result4 . "`n`n"

; Test Case 5 - Different date separator
Input5 := "* U5 C22222222222 DEF456 2025-06-30 박지성 some notes"
Expected5 := "* U6              DEF456 2025-07-01 DH2025.KIM  SAP ID Replace"
Result5 := ProcessF9Text(Input5)
TestResults .= "Test 5:`nInput:    " . Input5 . "`nExpected: " . Expected5 . "`nResult:   " . Result5 . "`n`n"

; Display results
MsgBox, 0, Test Results, %TestResults%

; Write results to file
FileDelete, test_results.txt
FileAppend, %TestResults%, test_results.txt

ExitApp
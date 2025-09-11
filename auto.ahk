#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%

; Global variables
TooltipText := ""
ShowTooltip := false
SavedText := "" ; Text to store in memory
CurrentSequence := "N" ; Track current sequence (N, U1, U2, U3, etc.)

; Win+1 hotkey - Type current sequence + " SAP ID Replace" with quote at start
#1::
    global CurrentSequence
    SendInput, "%CurrentSequence% SAP ID Replace
return

F9::
    global CurrentSequence
    ; Get currently selected text
    Clipboard := ""
    Send, ^c
    ClipWait, 1

    if (ErrorLevel) {
        MsgBox, Please select text first.
        return
    }

    OriginalText := Clipboard

    ; Process the text with special F9 requirements
    ProcessedText := ProcessF9Text(OriginalText)

    ; Extract and save the current sequence from processed text
    UpdateCurrentSequence(ProcessedText)

    ; Replace the selected text with the processed text
    Clipboard := ProcessedText
    Send, ^v
return

; F10 hotkey - Input selected text to last empty row in Excel column 
F10::
    ; Get currently selected text
    Clipboard := ""
    Send, ^c
    ClipWait, 1

    if (ErrorLevel) {
        MsgBox, Please select text first.
        return
    }

    SelectedText := Clipboard

    ; Hardcoded target column letter (change to "B", "C", or "D" as needed)
    ColumnLetter := "A"
    ; Optional: set a title keyword like "기준" or "변경" to target a workbook
    TitleKeyword := ""
    InsertToExcelColumnByLetter(ColumnLetter, SelectedText, TitleKeyword)
return

; Tooltip update timer
UpdateTooltip:
    if (ShowTooltip) {
        ; Display tooltip at top right corner of screen (doubled size area)
        xPos := A_ScreenWidth - 400
        ToolTip, %TooltipText%, %xPos%, 60
    } else {
        ToolTip
    }
return

; Function to process sequence and date
ProcessSequenceAndDate(text) {
    ; 1. Process sequence (N, U1, U2, U3...)
    text := ProcessSequence(text)

    ; 2. Process date
    text := ProcessDate(text)

    ; 3. Process Korean names (replace 3-character Korean names with "DH2025.KIM")
    text := ProcessKoreanNames(text)

    ; 4. Remove strings starting with C (like C2001023213) until space
    text := RemoveCStrings(text)

    ; 5. Replace the last part (변경내역) with "SAP ID replacement"
    text := ProcessChangeHistory(text)

return text
}

; Sequence processing function
ProcessSequence(text) {
    ; Find N or U + number patterns
    HasN := false
    HasU := false
    MaxUNumber := 0

    ; Check for standalone N
    if (RegExMatch(text, "\bN\b")) {
        HasN := true
    }

    ; Find U + number patterns and find maximum value
    StartPos := 1
    While (StartPos := RegExMatch(text, "\bU(\d+)\b", Match, StartPos)) {
        HasU := true
        CurrentNumber := Match1
        if (CurrentNumber > MaxUNumber) {
            MaxUNumber := CurrentNumber
        }
        StartPos += StrLen(Match)
    }

    ; Processing logic
    if (HasN) {
        ; Replace N with U1 and increment existing U numbers
        text := ReplaceUPattern(text, MaxUNumber, true)
    }
    else if (HasU) {
        ; Only increment U numbers
        text := IncrementUNumbers(text, MaxUNumber)
    }
    else {
        ; If nothing exists, add N at the beginning
        text := "N " . text
    }

return text
}

; Function to extract and update the current sequence from processed text
UpdateCurrentSequence(text) {
    global CurrentSequence
    
    ; First check for N followed by a number (like N3)
    if (RegExMatch(text, "\b(N\d+)\b", Match)) {
        ; Save the exact pattern (like N3)
        CurrentSequence := Match
    }
    ; Check for U followed by a number (like U1, U2, etc.)
    else if (RegExMatch(text, "\b(U\d+)\b", Match)) {
        ; Save the exact U pattern found (don't increment here)
        CurrentSequence := Match
    }
    ; Check for standalone N
    else if (RegExMatch(text, "\b(N)\b", Match)) {
        ; Save N as is
        CurrentSequence := "N"
    }
    ; If no sequence found in processed text, don't change CurrentSequence
}

; U pattern replacement function
ReplaceUPattern(text, maxNum, hasN) {
    result := text

    if (hasN) {
        ; Increment from largest number to prevent duplication
        Loop, %maxNum% {
            currentNum := maxNum - A_Index + 1
            oldPattern := "\bU" . currentNum . "\b"
            newNum := currentNum + 1
            newPattern := "U" . newNum
            result := RegExReplace(result, oldPattern, newPattern)
        }

        ; Replace N with U1
        result := RegExReplace(result, "\bN\b", "U1")
    }

return result
}

; U number increment function
IncrementUNumbers(text, maxNum) {
    result := text

    ; Increment from largest number to prevent duplication
    Loop, %maxNum% {
        currentNum := maxNum - A_Index + 1
        oldPattern := "\bU" . currentNum . "\b"
        newNum := currentNum + 1
        newPattern := "U" . newNum
        result := RegExReplace(result, oldPattern, newPattern)
    }

return result
}

; Date processing function
ProcessDate(text) {
    ; Get today's date
    FormatTime, Today, , yyyy.MM.dd
    FormatTime, TodayDash, , yyyy-MM-dd
    FormatTime, TodaySlash, , yyyy/MM/dd
    FormatTime, TodayNoSep, , yyyyMMdd

    ; Process various date format patterns
    ; yyyyMMdd format (8 digits without separators - like 20211003)
    text := RegExReplace(text, "\b\d{8}\b", TodayNoSep)

    ; yyyy.MM.dd format
    text := RegExReplace(text, "\d{4}\.\d{1,2}\.\d{1,2}", Today)

    ; yyyy-MM-dd format
    text := RegExReplace(text, "\d{4}-\d{1,2}-\d{1,2}", TodayDash)

    ; yyyy/MM/dd format
    text := RegExReplace(text, "\d{4}/\d{1,2}/\d{1,2}", TodaySlash)

    ; yyyy-MM.dd or yyyy.MM-dd format
    text := RegExReplace(text, "\d{4}[-\.]\d{1,2}[-\.]\d{1,2}", Today)

return text
}

; Korean name processing function
ProcessKoreanNames(text) {
    ; AutoHotkey v1 has limited Unicode support in regex
    ; We'll use a pattern that matches exactly 3 non-ASCII characters
    ; Korean names are typically 3 characters (sometimes 2 or 4, but 3 is most common)

    ; Pattern for 3 consecutive non-ASCII characters (Korean names)
    ; This will match any 3-character sequence where all characters are non-ASCII

    ; First, replace Korean names that appear after dates
    ; Match: date pattern + space + 3 non-ASCII characters
    text := RegExReplace(text, "(\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2}\s+)[^\x00-\x7F]{3}\b", "$1DH2025.KIM")

    ; Also replace standalone 3-character Korean names anywhere in the text
    ; Match: word boundary + 3 non-ASCII characters + word boundary
    text := RegExReplace(text, "\b[^\x00-\x7F]{3}\b", "DH2025.KIM")

    ; Handle 2-character Korean names (less common but possible)
    text := RegExReplace(text, "\b[^\x00-\x7F]{2}\b", "DH2025.KIM")

    ; Handle 4-character Korean names (rare but possible)
    text := RegExReplace(text, "\b[^\x00-\x7F]{4}\b", "DH2025.KIM")

return text
}

; Function to remove strings starting with C or CR
RemoveCStrings(text) {
    ; Remove strings that start with C or CR followed by numbers
    ; After removal, pad with spaces to maintain 12 character length

    ; Create 12 spaces for replacement
    TwelveSpaces := "            " ; Exactly 12 spaces

    ; First, handle CR followed by 10 digits (total 12 chars like CR2102090002)
    pos := 1
    While (pos := RegExMatch(text, "CR\d{10}", Match, pos)) {
        ; Replace the found match with 12 spaces
        StringReplace, text, text, %Match%, %TwelveSpaces%
        pos := pos + 12
    }

    ; Then handle C followed by 11 digits (total 12 chars)
    pos := 1
    While (pos := RegExMatch(text, "C\d{11}", Match, pos)) {
        ; Replace the found match with 12 spaces
        StringReplace, text, text, %Match%, %TwelveSpaces%
        pos := pos + 12
    }

    ; Also handle cases where C or CR might be followed by any number of digits
    ; Replace CR followed by digits
    text := RegExReplace(text, "\bCR\d+\b", TwelveSpaces)
    ; Replace C followed by digits (but not CR)
    text := RegExReplace(text, "\bC(?!R)\d+\b", TwelveSpaces)

return text
}

; Function to process change history (변경내역)
ProcessChangeHistory(text) {
    ; Replace the last part of the text with "SAP ID replacement"
    ; We'll look for the last significant text after all the processed elements

    ; Split text by spaces and tabs
    StringSplit, parts, text, %A_Space%%A_Tab%

    ; Find the position after the Korean name (DH2025.KIM) or date
    ; and replace everything after that with "SAP ID replacement"

    ; Look for DH2025.KIM in the text
    if (InStr(text, "DH2025.KIM")) {
        ; Find position after DH2025.KIM
        pos := InStr(text, "DH2025.KIM") + StrLen("DH2025.KIM")

        ; Get the part before and after
        beforePart := SubStr(text, 1, pos)
        afterPart := SubStr(text, pos + 1)

        ; Trim spaces from afterPart
        afterPart := Trim(afterPart)

        ; If there's text after DH2025.KIM, replace it
        if (StrLen(afterPart) > 0) {
            text := beforePart . " SAP ID replacement"
        }
    }
    else {
        ; If no Korean name found, look for the last substantial text
        ; and replace it with "SAP ID replacement"

        ; Find the last non-empty part
        lastPartIndex := 0
        Loop, %parts0% {
            if (StrLen(parts%A_Index%) > 0) {
                lastPartIndex := A_Index
            }
        }

        ; If we have parts, replace the last one
        if (lastPartIndex > 0) {
            ; Rebuild the text without the last part
            newText := ""
            Loop, % lastPartIndex - 1 {
                if (StrLen(parts%A_Index%) > 0) {
                    if (StrLen(newText) > 0) {
                        newText := newText . " " . parts%A_Index%
                    }
                    else {
                        newText := parts%A_Index%
                    }
                }
            }

            ; Add "SAP ID replacement" as the last part
            if (StrLen(newText) > 0) {
                text := newText . " SAP ID replacement"
            }
            else {
                text := "SAP ID replacement"
            }
        }
    }

return text
}

; Function to process text for F9 hotkey
ProcessF9Text(text) {
    ; 1. Process sequence (N, U1, U2, U3...) first
    text := ProcessSequence(text)

    ; 2. Replace C followed by 11 digits (total 12 chars) with 12 spaces
    text := RemoveCStrings(text)

    ; 3. Process date - replace with today's date
    text := ProcessDate(text)

    ; 4. Find the date position to handle everything after it
    ; Match various date patterns including YYYYMMDD format
    datePos := 0
    dateEndPos := 0
    
    ; Check for YYYYMMDD format (8 digits)
    if (RegExMatch(text, "\b\d{8}\b", Match, 1)) {
        datePos := RegExMatch(text, "\b\d{8}\b", Match, 1)
        dateEndPos := datePos + StrLen(Match)
    }
    ; Check for date with separators
    else if (RegExMatch(text, "\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2}", Match, 1)) {
        datePos := RegExMatch(text, "\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2}", Match, 1)
        dateEndPos := datePos + StrLen(Match)
    }
    
    ; If date found, replace everything after it
    if (datePos > 0) {
        ; Get the part before and including the date
        beforeDate := SubStr(text, 1, dateEndPos)
        
        ; Replace everything after the date with DH2025.KIM SAP ID Replace
        text := beforeDate . " DH2025.KIM SAP ID Replace"
    }
    else {
        ; If no date found, just append at the end
        text := text . " DH2025.KIM SAP ID Replace"
    }

return text
}

; Optimized function to insert text into Excel column
InsertToExcelColumnByLetter(columnLetter, textToInsert, titleKeyword:="") {
    ; Normalize and validate column letter
    columnLetter := Trim(columnLetter)
    if (StrLen(columnLetter) != 1) {
        MsgBox, Please enter a single column letter like A, B, C, or D.
        return
    }
    StringUpper, upper, %columnLetter%
    colNum := Asc(upper) - Asc("A") + 1
    if (colNum < 1 || colNum > 26) {
        MsgBox, Invalid column letter: %columnLetter%
        return
    }
    InsertToExcelColumn(colNum, upper, textToInsert, titleKeyword)
}
InsertToExcelColumn(columnNum, columnLetter, textToInsert, titleKeyword:="") {
    ; Get Excel COM object
    try {
        xl := ComObjActive("Excel.Application")
    } catch {
        MsgBox, Excel is not running. Please open Excel first.
        return
    }

    try {
        ; Optionally target a specific workbook by title keyword
        if (StrLen(Trim(titleKeyword)) > 0) {
            wb := FindWorkbookByKeyword(xl, titleKeyword)
            if (wb) {
                wb.Activate
            } else {
                MsgBox, Could not find an Excel workbook whose title includes: %titleKeyword%
                return
            }
        }

        ; Get active worksheet (after targeting workbook if provided)
        ws := xl.ActiveSheet

        ; Find last used row in the specified column
        lastRow := ws.Cells(ws.Rows.Count, columnNum).End(-4162).Row ; -4162 = xlUp

        ; If column is completely empty, use 1, otherwise lastRow + 1
        cellAddress := columnLetter . "1"
        if (ws.Range(cellAddress).Value = "") {
            targetRow := 1
        } else {
            targetRow := lastRow + 1
        }

        ; Input value to target cell
        ws.Cells(targetRow, columnNum).Value := textToInsert

        ; Success message
        TrayTip, Excel Input Complete, Entered in cell %columnLetter%%targetRow%, 2

    } catch e {
        MsgBox, Error occurred during Excel operation.`n%e%
    }
}
FindWorkbookByKeyword(xl, keyword) {
    keyword := Trim(keyword)
    if (StrLen(keyword) = 0) {
        return ""
    }

    ; Search by window caption, workbook name, or full name
    try {
        count := xl.Workbooks.Count
        Loop, %count% {
            wb := xl.Workbooks.Item(A_Index)
            caption := ""
            try {
                if (wb.Windows.Count >= 1) {
                    caption := wb.Windows(1).Caption
                }
            }
            if (InStr(wb.Name, keyword) || InStr(wb.FullName, keyword) || InStr(caption, keyword)) {
                return wb
            }
        }
    }
return ""
}

; ESC key to exit script
Esc::ExitApp
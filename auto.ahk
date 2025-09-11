#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%

; Global variables
TooltipText := ""
ShowTooltip := false
SavedText := ""  ; Text to store in memory

; F1 hotkey - Process selected text and replace it
F1::
    ; Get currently selected text
    Clipboard := ""
    Send, ^c
    ClipWait, 1
    
    if (ErrorLevel) {
        MsgBox, Please select text first.
        return
    }
    
    OriginalText := Clipboard
    ProcessedText := ProcessSequenceAndDate(OriginalText)
    
    ; Show InputBox
    InputBox, UserInput, Text Processing, Processed text:, , 500, 200, , , , , %ProcessedText%
    
    if (ErrorLevel = 0) {
        ; OK button clicked
        ; Replace the selected text with the processed text
        Clipboard := UserInput
        Send, ^v
        
        ; Save to memory
        SavedText := UserInput
        
        ; Start tooltip display
        TooltipText := UserInput
        ShowTooltip := true
        SetTimer, UpdateTooltip, 100
    }
return

; F2 hotkey - Stop tooltip
F2::
    ShowTooltip := false
    SetTimer, UpdateTooltip, Off
    ToolTip
return

; F3 hotkey - Input as "* saved text" format
F3::
    if (SavedText != "") {
        OutputText := "* " . SavedText
        Clipboard := OutputText
        Send, %OutputText%
    } else {
        MsgBox, No saved text. Please use F1 first.
    }
return

; F4 hotkey - Input as '"saved text" format
F4::
    if (SavedText != "") {
        OutputText := "'" . chr(34) . SavedText . chr(34)
        Clipboard := OutputText
        Send, %OutputText%
    } else {
        MsgBox, No saved text. Please use F1 first.
    }
return

; F5 hotkey - Input selected text to last empty row in Excel column A
F5::
    ; Get currently selected text
    Clipboard := ""
    Send, ^c
    ClipWait, 1
    
    if (ErrorLevel) {
        MsgBox, Please select text first.
        return
    }
    
    SelectedText := Clipboard
    
    ; Get Excel COM object
    try {
        xl := ComObjActive("Excel.Application")
    } catch {
        MsgBox, Excel is not running. Please open Excel first.
        return
    }
    
    try {
        ; Get active worksheet
        ws := xl.ActiveSheet
        
        ; Find last used row in column A
        lastRow := ws.Cells(ws.Rows.Count, 1).End(-4162).Row  ; -4162 = xlUp
        
        ; If column A is completely empty, use 1, otherwise lastRow + 1
        if (ws.Range("A1").Value = "") {
            targetRow := 1
        } else {
            targetRow := lastRow + 1
        }
        
        ; Input value to target cell
        ws.Cells(targetRow, 1).Value := SelectedText
        
        ; Success message
        TrayTip, Excel Input Complete, Entered in cell A%targetRow%, 2
        
    } catch e {
        MsgBox, Error occurred during Excel operation.`n%e%
    }
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
    
    ; Process various date format patterns
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

; Function to remove strings starting with C
RemoveCStrings(text) {
    ; Remove strings that start with C followed by numbers
    ; After removal, pad with spaces to maintain 12 character length
    
    ; Create 12 spaces for replacement
    TwelveSpaces := "            "  ; Exactly 12 spaces
    
    ; Replace all occurrences of C followed by digits with 12 spaces
    ; Using a callback function to ensure proper replacement
    pos := 1
    While (pos := RegExMatch(text, "C\d{11}", Match, pos)) {
        ; Replace the found match with 12 spaces
        StringReplace, text, text, %Match%, %TwelveSpaces%
        pos := pos + 12
    }
    
    ; Also handle cases where C might be followed by fewer than 11 digits
    ; Replace any C followed by digits (up to word boundary) with 12 spaces
    text := RegExReplace(text, "\bC\d+\b", TwelveSpaces)
    
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

; Function to increment date by one day
IncrementDate(dateStr) {
    ; Parse the date string (supports yyyy.MM.dd, yyyy-MM-dd, yyyy/MM/dd)
    if (RegExMatch(dateStr, "(\d{4})([\.\-/])(\d{1,2})([\.\-/])(\d{1,2})", Match)) {
        year := Match1
        separator := Match2
        month := Match3
        day := Match5
        
        ; Remove leading zeros for calculation
        month := month + 0
        day := day + 0
        
        ; Days in each month (non-leap year)
        daysInMonth := [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        
        ; Check for leap year
        if ((Mod(year, 4) = 0 && Mod(year, 100) != 0) || Mod(year, 400) = 0) {
            daysInMonth[2] := 29
        }
        
        ; Increment day
        day := day + 1
        
        ; Check if we need to roll over to next month
        if (day > daysInMonth[month]) {
            day := 1
            month := month + 1
            
            ; Check if we need to roll over to next year
            if (month > 12) {
                month := 1
                year := year + 1
            }
        }
        
        ; Format the new date with leading zeros
        monthStr := month < 10 ? "0" . month : month
        dayStr := day < 10 ? "0" . day : day
        
        ; Return the incremented date with the same separator
        return year . separator . monthStr . separator . dayStr
    }
    
    ; If date format not recognized, return original
    return dateStr
}

; Function to process text for F9 hotkey
ProcessF9Text(text) {
    ; 1. Process sequence (N, U1, U2, U3...) first
    text := ProcessSequence(text)
    
    ; 2. Replace C followed by digits with spaces (maintaining alignment)
    text := RemoveCStringsForF9(text)
    
    ; 3. Increment date by 1 day
    text := IncrementDatesInText(text)
    
    ; 4. Find the date position and replace everything after it
    ; Match various date patterns
    if (RegExMatch(text, "\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2}", Match, 1)) {
        datePos := RegExMatch(text, "\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2}", Match, 1)
        dateEndPos := datePos + StrLen(Match)
        
        ; Get the part before and including the date
        beforeDate := SubStr(text, 1, dateEndPos)
        
        ; Replace everything after the date with DH2025.KIM  SAP ID Replace (two spaces)
        text := beforeDate . " DH2025.KIM  SAP ID Replace"
    }
    else {
        ; If no date found, just append at the end
        text := text . " DH2025.KIM  SAP ID Replace"
    }
    
    return text
}

; Function to increment all dates in text
IncrementDatesInText(text) {
    result := text
    
    ; Process yyyy.MM.dd format
    pos := 1
    While (pos := RegExMatch(result, "O)(\d{4}\.\d{1,2}\.\d{1,2})", Match, pos)) {
        oldDate := Match.Value
        newDate := IncrementDate(oldDate)
        StringReplace, result, result, %oldDate%, %newDate%
        pos := Match.Pos + StrLen(newDate)
    }
    
    ; Process yyyy-MM-dd format
    pos := 1
    While (pos := RegExMatch(result, "O)(\d{4}-\d{1,2}-\d{1,2})", Match, pos)) {
        oldDate := Match.Value
        newDate := IncrementDate(oldDate)
        StringReplace, result, result, %oldDate%, %newDate%
        pos := Match.Pos + StrLen(newDate)
    }
    
    ; Process yyyy/MM/dd format
    pos := 1
    While (pos := RegExMatch(result, "O)(\d{4}/\d{1,2}/\d{1,2})", Match, pos)) {
        oldDate := Match.Value
        newDate := IncrementDate(oldDate)
        StringReplace, result, result, %oldDate%, %newDate%
        pos := Match.Pos + StrLen(newDate)
    }
    
    return result
}

; Function to remove C-strings for F9 (maintains proper spacing)
RemoveCStringsForF9(text) {
    ; Remove strings that start with C followed by numbers
    ; Replace with appropriate number of spaces to maintain alignment
    
    result := text
    
    ; Match C followed by digits and replace with equivalent spaces
    pos := 1
    While (pos := RegExMatch(result, "O)(C\d+)", Match, pos)) {
        matchLen := StrLen(Match.Value)
        spaces := ""
        Loop, %matchLen% {
            spaces := spaces . " "
        }
        StringReplace, result, result, % Match.Value, %spaces%
        pos := Match.Pos + matchLen
    }
    
    return result
}

; F6 hotkey - Input selected text to last empty row in Excel column B
F6::
    ; Get currently selected text
    Clipboard := ""
    Send, ^c
    ClipWait, 1
    
    if (ErrorLevel) {
        MsgBox, Please select text first.
        return
    }
    
    SelectedText := Clipboard
    
    ; Get Excel COM object
    try {
        xl := ComObjActive("Excel.Application")
    } catch {
        MsgBox, Excel is not running. Please open Excel first.
        return
    }
    
    try {
        ; Get active worksheet
        ws := xl.ActiveSheet
        
        ; Find last used row in column B
        lastRow := ws.Cells(ws.Rows.Count, 2).End(-4162).Row  ; -4162 = xlUp, 2 = column B
        
        ; If column B is completely empty, use 1, otherwise lastRow + 1
        if (ws.Range("B1").Value = "") {
            targetRow := 1
        } else {
            targetRow := lastRow + 1
        }
        
        ; Input value to target cell
        ws.Cells(targetRow, 2).Value := SelectedText
        
        ; Success message
        TrayTip, Excel Input Complete, Entered in cell B%targetRow%, 2
        
    } catch e {
        MsgBox, Error occurred during Excel operation.`n%e%
    }
return

; F9 hotkey - Process text and replace with formatted version
F9::
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
    
    ; Replace the selected text with the processed text
    Clipboard := ProcessedText
    Send, ^v
return

; Optimized function to insert text into Excel column
InsertToExcelColumn(columnNum, columnLetter, textToInsert) {
    ; Get Excel COM object
    try {
        xl := ComObjActive("Excel.Application")
    } catch {
        MsgBox, Excel is not running. Please open Excel first.
        return
    }
    
    try {
        ; Get active worksheet
        ws := xl.ActiveSheet
        
        ; Find last used row in the specified column
        lastRow := ws.Cells(ws.Rows.Count, columnNum).End(-4162).Row  ; -4162 = xlUp
        
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

; ESC key to exit script
Esc::ExitApp
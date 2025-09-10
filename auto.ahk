#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%

; Global variables
TooltipText := ""
ShowTooltip := false
SavedText := ""  ; Text to store in memory

; F1 hotkey - Process selected text
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
        Clipboard := UserInput
        ; Replace selected text with new text
        Send, %UserInput%
        
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

; F5 hotkey - Input saved text to last empty row in Excel column A
F5::
    if (SavedText = "") {
        MsgBox, No saved text. Please use F1 first.
        return
    }
    
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
        ws.Cells(targetRow, 1).Value := SavedText
        
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
    
    ; 3. Process Korean names (replace 3-character Korean names after date with "Kim Dong-hyun")
    text := ProcessKoreanNames(text)
    
    ; 4. Remove strings starting with C (like C2001023213) until space
    text := RemoveCStrings(text)
    
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
    ; Regular expression to match 3-character Korean names after date patterns
    ; Korean character range: [\x{AC00}-\x{D7AF}] but AutoHotkey v1 doesn't support Unicode regex well
    ; So we use a different approach for Korean text detection
    
    ; Pattern to find date followed by Korean name (3 characters)
    ; This regex looks for date patterns followed by exactly 3 Korean characters
    ; Note: AutoHotkey v1 has limited Unicode regex support, so this is a simplified version
    
    ; Replace patterns like "2024.01.10 홍길동" with "2024.01.10 Kim Dong-hyun"
    ; Due to AutoHotkey v1 limitations with Unicode regex, we'll use a simpler approach
    
    ; Match date followed by space and 3 characters that might be Korean name
    text := RegExReplace(text, "(\d{4}[\.\-/]\d{1,2}[\.\-/]\d{1,2})\s+.{3}\b", "$1 Kim Dong-hyun")
    
    return text
}

; Function to remove strings starting with C
RemoveCStrings(text) {
    ; Remove strings that start with C followed by numbers until space
    ; Example: C2001023213 will be removed
    text := RegExReplace(text, "\bC\d+\s*", "")
    
    return text
}

; ESC key to exit script
Esc::ExitApp
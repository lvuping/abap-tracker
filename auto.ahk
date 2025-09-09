#NoEnv
SendMode Input
SetWorkingDir %A_ScriptDir%

; 전역 변수
TooltipText := ""
ShowTooltip := false
SavedText := ""  ; 메모리에 저장할 텍스트

; F1 핫키 - 선택된 텍스트 처리
F1::
    ; 현재 선택된 텍스트 가져오기
    Clipboard := ""
    Send, ^c
    ClipWait, 1
    
    if (ErrorLevel) {
        MsgBox, 텍스트를 선택해주세요.
        return
    }
    
    OriginalText := Clipboard
    ProcessedText := ProcessSequenceAndDate(OriginalText)
    
    ; InputBox 표시
    InputBox, UserInput, 텍스트 처리, 처리된 텍스트:, , 500, 200, , , , , %ProcessedText%
    
    if (ErrorLevel = 0) {
        ; OK 버튼 클릭시
        Clipboard := UserInput
        ; 선택된 텍스트를 새 텍스트로 교체
        Send, %UserInput%
        
        ; 메모리에 저장
        SavedText := UserInput
        
        ; 툴팁 표시 시작
        TooltipText := UserInput
        ShowTooltip := true
        SetTimer, UpdateTooltip, 100
    }
return

; F2 핫키 - 툴팁 종료
F2::
    ShowTooltip := false
    SetTimer, UpdateTooltip, Off
    ToolTip
return

; F3 핫키 - "* 저장된 문장" 형식으로 입력
F3::
    if (SavedText != "") {
        OutputText := "* " . SavedText
        Clipboard := OutputText
        Send, %OutputText%
    } else {
        MsgBox, 저장된 텍스트가 없습니다. F1을 먼저 사용해주세요.
    }
return

; F4 핫키 - '"저장된 문장" 형식으로 입력
F4::
    if (SavedText != "") {
        OutputText := "'" . chr(34) . SavedText . chr(34)
        Clipboard := OutputText
        Send, %OutputText%
    } else {
        MsgBox, 저장된 텍스트가 없습니다. F1을 먼저 사용해주세요.
    }
return

; F5 핫키 - Excel A열의 마지막 빈칸에 저장된 텍스트 입력
F5::
    if (SavedText = "") {
        MsgBox, 저장된 텍스트가 없습니다. F1을 먼저 사용해주세요.
        return
    }
    
    ; Excel COM 객체 가져오기
    try {
        xl := ComObjActive("Excel.Application")
    } catch {
        MsgBox, Excel이 실행되지 않았습니다. Excel을 먼저 열어주세요.
        return
    }
    
    try {
        ; 활성 워크시트 가져오기
        ws := xl.ActiveSheet
        
        ; A열의 마지막 사용된 행 찾기
        lastRow := ws.Cells(ws.Rows.Count, 1).End(-4162).Row  ; -4162 = xlUp
        
        ; A열이 완전히 비어있으면 1, 아니면 lastRow + 1
        if (ws.Range("A1").Value = "") {
            targetRow := 1
        } else {
            targetRow := lastRow + 1
        }
        
        ; 해당 셀에 값 입력
        ws.Cells(targetRow, 1).Value := SavedText
        
        ; 성공 메시지
        TrayTip, Excel 입력 완료, A%targetRow% 셀에 입력되었습니다., 2
        
    } catch e {
        MsgBox, Excel 작업 중 오류가 발생했습니다.`n%e%
    }
return

; 툴팁 업데이트 타이머
UpdateTooltip:
    if (ShowTooltip) {
        ; 화면 우측 상단에 툴팁 표시
        xPos := A_ScreenWidth - 200
        ToolTip, %TooltipText%, %xPos%, 30
    } else {
        ToolTip
    }
return

; 시퀀스와 날짜를 처리하는 함수
ProcessSequenceAndDate(text) {
    ; 1. 시퀀스 처리 (N, U1, U2, U3...)
    text := ProcessSequence(text)
    
    ; 2. 날짜 처리
    text := ProcessDate(text)
    
    return text
}

; 시퀀스 처리 함수
ProcessSequence(text) {
    ; N 또는 U + 숫자 패턴 찾기
    HasN := false
    HasU := false
    MaxUNumber := 0
    
    ; N 체크 - 단독 N만 찾기
    if (RegExMatch(text, "\bN\b")) {
        HasN := true
    }
    
    ; U + 숫자 패턴 찾기 및 최대값 찾기
    StartPos := 1
    While (StartPos := RegExMatch(text, "\bU(\d+)\b", Match, StartPos)) {
        HasU := true
        CurrentNumber := Match1
        if (CurrentNumber > MaxUNumber) {
            MaxUNumber := CurrentNumber
        }
        StartPos += StrLen(Match)
    }
    
    ; 처리 로직
    if (HasN) {
        ; N을 U1으로 교체하고 기존 U 숫자들 증가
        text := ReplaceUPattern(text, MaxUNumber, true)
    }
    else if (HasU) {
        ; U 숫자들만 증가
        text := IncrementUNumbers(text, MaxUNumber)
    }
    else {
        ; 아무것도 없으면 맨 앞에 N 추가
        text := "N " . text
    }
    
    return text
}

; U 패턴 교체 함수
ReplaceUPattern(text, maxNum, hasN) {
    result := text
    
    if (hasN) {
        ; 큰 숫자부터 증가시켜야 중복 방지
        Loop, %maxNum% {
            currentNum := maxNum - A_Index + 1
            oldPattern := "\bU" . currentNum . "\b"
            newNum := currentNum + 1
            newPattern := "U" . newNum
            result := RegExReplace(result, oldPattern, newPattern)
        }
        
        ; N을 U1으로 교체
        result := RegExReplace(result, "\bN\b", "U1")
    }
    
    return result
}

; U 숫자 증가 함수
IncrementUNumbers(text, maxNum) {
    result := text
    
    ; 큰 숫자부터 증가시켜야 중복 방지
    Loop, %maxNum% {
        currentNum := maxNum - A_Index + 1
        oldPattern := "\bU" . currentNum . "\b"
        newNum := currentNum + 1
        newPattern := "U" . newNum
        result := RegExReplace(result, oldPattern, newPattern)
    }
    
    return result
}

; 날짜 처리 함수
ProcessDate(text) {
    ; 오늘 날짜 가져오기
    FormatTime, Today, , yyyy.MM.dd
    FormatTime, TodayDash, , yyyy-MM-dd
    FormatTime, TodaySlash, , yyyy/MM/dd
    
    ; 다양한 날짜 형식 패턴 처리
    ; yyyy.MM.dd 형식
    text := RegExReplace(text, "\d{4}\.\d{1,2}\.\d{1,2}", Today)
    
    ; yyyy-MM-dd 형식
    text := RegExReplace(text, "\d{4}-\d{1,2}-\d{1,2}", TodayDash)
    
    ; yyyy/MM/dd 형식
    text := RegExReplace(text, "\d{4}/\d{1,2}/\d{1,2}", TodaySlash)
    
    ; yyyy-MM.dd 또는 yyyy.MM-dd 형식
    text := RegExReplace(text, "\d{4}[-\.]\d{1,2}[-\.]\d{1,2}", Today)
    
    return text
}

; ESC 키로 스크립트 종료
Esc::ExitApp
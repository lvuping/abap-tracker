import re
from patterns import *

temp_audit_result = None


def group_statements(snippet):
    """
    ABAP 코드 라인들을 완전한 문장 단위로 그룹화합니다.
    각 문장은 마침표(.)로 끝납니다. 주석은 제거됩니다.
    반환값: (문장, 시작 라인 번호) 튜플의 리스트
    """
    statements = []
    current_statement = ""
    statement_start_line = -1

    for i, line in enumerate(snippet):
        # 주석 처리 (라인 시작이 * 이거나, "가 중간에 있는 경우)
        if line.strip().startswith("*"):
            continue
        line = line.split('"')[0]

        clean_line = line.strip()
        if not clean_line:
            continue

        if statement_start_line == -1:
            statement_start_line = i

        current_statement += " " + clean_line

        if clean_line.endswith("."):
            statements.append((current_statement.strip(), statement_start_line))
            current_statement = ""
            statement_start_line = -1

    return statements


def has_important_commands_in_range(statements, target_line, range_lines=10):
    """
    지정된 라인 위 range_lines 줄 내에 중요한 명령어가 있는지 확인
    중요 명령어: CALL FUNCTION, UPDATE, INSERT, MODIFY, DELETE, CALL TRANSACTION
    """
    important_patterns = [
        r"CALL\s+FUNCTION",
        r"UPDATE\s+",
        r"INSERT\s+",
        r"MODIFY\s+",
        r"DELETE\s+",
        r"CALL\s+TRANSACTION",
    ]

    # target_line 위 range_lines 줄까지 검사
    start_check_line = max(0, target_line - range_lines)

    for i, (stmt, line_num) in enumerate(statements):
        if start_check_line <= line_num <= target_line:
            stmt_upper = stmt.upper()
            for pattern in important_patterns:
                if re.search(pattern, stmt_upper, re.IGNORECASE):
                    return True, line_num, pattern

    return False, -1, None


def trace_sy_uname_in_snippet(snippet, start_line_in_snippet):
    global temp_audit_result
    temp_audit_result = None

    # 빈 snippet 처리
    if not snippet:
        return {"status": "Not Found", "reason": "Empty snippet"}

    statements = group_statements(snippet)
    # 빈 statements 처리
    if not statements:
        return {"status": "Not Found", "reason": "No valid statements found"}

    # 1단계: 원본 라인에서 직접 확인 (주석, 빈 라인 등을 고려)
    if start_line_in_snippet >= len(snippet):
        return {
            "status": "Not Found",
            "reason": f"Specified line {start_line_in_snippet + 1} is out of range",
            "specified_line": start_line_in_snippet + 1,
            "error_type": "LINE_OUT_OF_RANGE",
        }

    # 원본 라인 내용 확인
    original_line = snippet[start_line_in_snippet].strip()

    # 주석 라인 확인
    if original_line.startswith("*") or not original_line:
        return {
            "status": "Not Found",
            "reason": f"Specified line {start_line_in_snippet + 1} is a comment or empty line",
            "specified_line": start_line_in_snippet + 1,
            "actual_content": original_line,
            "error_type": "SYUNAME_NOT_AT_SPECIFIED_LINE",
        }

    # 원본 라인에 sy-uname이 있는지 확인
    if "SY-UNAME" not in original_line.upper():
        return {
            "status": "Not Found",
            "reason": f"SY-UNAME not found at specified line {start_line_in_snippet + 1}",
            "specified_line": start_line_in_snippet + 1,
            "actual_content": original_line,
            "error_type": "SYUNAME_NOT_AT_SPECIFIED_LINE",
        }

    # 2단계: statements에서 해당 라인의 statement 찾기
    sy_uname_found_at_start_line = False
    sy_uname_statement_idx = -1
    actual_sy_uname_line = -1

    for i, (stmt, line_num) in enumerate(statements):
        # 정확히 start_line_in_snippet에 해당하는 statement 찾기
        if (
            line_num
            <= start_line_in_snippet
            < (statements[i + 1][1] if i + 1 < len(statements) else float("inf"))
        ):
            # 해당 statement에 sy-uname이 있는지 확인
            if "SY-UNAME" in stmt.upper():
                sy_uname_found_at_start_line = True
                sy_uname_statement_idx = i
                actual_sy_uname_line = line_num
                break
            else:
                # 이론적으로는 여기 도달하면 안됨 (이미 원본에서 확인했기 때문)
                return {
                    "status": "Not Found",
                    "reason": f"SY-UNAME found in original line but not in processed statement",
                    "specified_line": start_line_in_snippet + 1,
                    "actual_content": stmt.strip(),
                    "error_type": "STATEMENT_PROCESSING_ERROR",
                }

    # 2단계: sy-uname이 발견되지 않은 경우 처리
    if not sy_uname_found_at_start_line:
        return {
            "status": "Not Found",
            "reason": f"SY-UNAME not found at specified line {start_line_in_snippet + 1}",
            "specified_line": start_line_in_snippet + 1,
            "error_type": "SYUNAME_NOT_AT_SPECIFIED_LINE",
        }

    # 3단계: sy-uname이 있는 경우에만 분석 시작
    tainted_vars = {"sy-uname"}
    trace_path = [
        f"✅ SY-UNAME found at line {actual_sy_uname_line + 1} (specified line {start_line_in_snippet + 1})"
    ]

    # LOOP 구문 추적을 위한 변수들
    loop_workarea = None  # 현재 LOOP의 workarea 변수
    in_loop = False  # LOOP 내부인지 여부
    loop_tables = []  # LOOP에서 처리하는 테이블들

    # 4단계: 정확한 범위에서만 분석 - sy-uname 위치부터 시작
    start_statement_idx = sy_uname_statement_idx
    trace_path.append(
        f"Starting analysis from verified SY-UNAME location (line {actual_sy_uname_line + 1})"
    )

    for i in range(start_statement_idx, len(statements)):
        statement, line_num = statements[i]
        statement_upper = statement.upper()

        # --- 스코프 경계 체크: 이런 구문을 만나면 추적 종료 ---

        # PERFORM 호출 감지 (다른 서브루틴으로 전환)
        perform_call_match = PERFORM_CALL_PATTERN.match(statement_upper)
        if perform_call_match:
            subroutine_name = perform_call_match.group("subroutine")
            trace_path.append(
                f"Line {line_num+1}: ⛔ PERFORM {subroutine_name} - 추적 종료 (스코프 경계)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "PERFORM_CALL",
                "subroutine": subroutine_name,
                "reason": f"PERFORM 호출로 인한 스코프 경계 도달: {subroutine_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # INCLUDE 구문 감지 (다른 파일 포함)
        include_match = INCLUDE_PATTERN.match(statement_upper)
        if include_match:
            include_name = include_match.group("include_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ INCLUDE {include_name} - 추적 종료 (스코프 경계)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "INCLUDE",
                "include_name": include_name,
                "reason": f"INCLUDE 구문으로 인한 스코프 경계 도달: {include_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # SUBMIT 프로그램 실행 감지 (다른 프로그램으로 전환)
        submit_match = SUBMIT_PATTERN.match(statement_upper)
        if submit_match:
            program_name = submit_match.group("program_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ SUBMIT {program_name} - 추적 종료 (스코프 경계)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "SUBMIT",
                "program_name": program_name,
                "reason": f"SUBMIT 구문으로 인한 스코프 경계 도달: {program_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # FORM 정의 시작 감지 (새로운 서브루틴 정의 시작)
        form_def_match = FORM_DEFINITION_PATTERN.match(statement_upper)
        if form_def_match:
            form_name = form_def_match.group("form_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ FORM {form_name} - 추적 종료 (새 서브루틴 정의)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "FORM_DEFINITION",
                "form_name": form_name,
                "reason": f"새로운 FORM 정의로 인한 스코프 경계 도달: {form_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # FUNCTION 정의 시작 감지 (새로운 함수 모듈 정의 시작)
        function_def_match = FUNCTION_DEFINITION_PATTERN.match(statement_upper)
        if function_def_match:
            function_name = function_def_match.group("function_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ FUNCTION {function_name} - 추적 종료 (새 함수 정의)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "FUNCTION_DEFINITION",
                "function_name": function_name,
                "reason": f"새로운 FUNCTION 정의로 인한 스코프 경계 도달: {function_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # METHOD 정의 시작 감지 (새로운 메소드 정의 시작)
        method_def_match = METHOD_DEFINITION_PATTERN.match(statement_upper)
        if method_def_match:
            method_name = method_def_match.group("method_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ METHOD {method_name} - 추적 종료 (새 메소드 정의)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "METHOD_DEFINITION",
                "method_name": method_name,
                "reason": f"새로운 METHOD 정의로 인한 스코프 경계 도달: {method_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # CLASS 정의 시작 감지 (새로운 클래스 정의 시작)
        class_def_match = CLASS_DEFINITION_PATTERN.match(statement_upper)
        if class_def_match:
            class_name = class_def_match.group("class_name")
            trace_path.append(
                f"Line {line_num+1}: ⛔ CLASS {class_name} - 추적 종료 (새 클래스 정의)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "CLASS_DEFINITION",
                "class_name": class_name,
                "reason": f"새로운 CLASS 정의로 인한 스코프 경계 도달: {class_name}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # 동적 PERFORM 호출 감지 (변수명으로 서브루틴 호출)
        dynamic_perform_match = DYNAMIC_PERFORM_PATTERN.match(statement_upper)
        if dynamic_perform_match:
            subroutine_var = dynamic_perform_match.group("subroutine_var")
            trace_path.append(
                f"Line {line_num+1}: ⛔ PERFORM ({subroutine_var}) - 추적 종료 (동적 호출)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "DYNAMIC_PERFORM",
                "subroutine_variable": subroutine_var,
                "reason": f"동적 PERFORM 호출로 인한 스코프 경계 도달: ({subroutine_var})",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # 동적 CALL FUNCTION 감지 (변수명으로 함수 호출)
        dynamic_function_match = DYNAMIC_CALL_FUNCTION_PATTERN.match(statement_upper)
        if dynamic_function_match:
            function_var = dynamic_function_match.group("function_var")
            trace_path.append(
                f"Line {line_num+1}: ⛔ CALL FUNCTION {function_var} - 추적 종료 (동적 호출)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "DYNAMIC_CALL_FUNCTION",
                "function_variable": function_var,
                "reason": f"동적 CALL FUNCTION 호출로 인한 스코프 경계 도달: {function_var}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # 동적 METHOD 호출 감지
        dynamic_method_match = DYNAMIC_CALL_METHOD_PATTERN.match(statement_upper)
        if dynamic_method_match:
            object_ref = dynamic_method_match.group("object_ref")
            trace_path.append(
                f"Line {line_num+1}: ⛔ CALL METHOD {object_ref} - 추적 종료 (동적 메소드 호출)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "DYNAMIC_CALL_METHOD",
                "object_reference": object_ref,
                "reason": f"동적 CALL METHOD 호출로 인한 스코프 경계 도달: {object_ref}",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # 객체 메소드 호출 감지 (lo_object->method())
        object_method_match = OBJECT_METHOD_CALL_PATTERN.match(statement_upper)
        if object_method_match:
            object_name = object_method_match.group("object")
            method_name = object_method_match.group("method")
            trace_path.append(
                f"Line {line_num+1}: ⛔ {object_name}->{method_name}() - 추적 종료 (객체 메소드 호출)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "OBJECT_METHOD_CALL",
                "object_name": object_name,
                "method_name": method_name,
                "reason": f"객체 메소드 호출로 인한 스코프 경계 도달: {object_name}->{method_name}()",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # TRY 블록 시작 감지 (새로운 예외 처리 스코프)
        try_block_match = TRY_BLOCK_PATTERN.match(statement_upper)
        if try_block_match:
            trace_path.append(
                f"Line {line_num+1}: ⛔ TRY - 추적 종료 (예외 처리 스코프)"
            )
            return {
                "status": "Scope Boundary Reached",
                "type": "TRY_BLOCK",
                "reason": "TRY 블록 시작으로 인한 스코프 경계 도달",
                "boundary_line": line_num + 1,
                "verified_syuname_line": actual_sy_uname_line + 1,
                "tainted_variables": list(tainted_vars),
                "trace_path": trace_path,
            }

        # --- 모든 패턴 매칭은 이제 statement_upper를 대상으로 수행 ---

        # LOOP 구문 처리
        # LOOP AT 시작
        loop_at_match = LOOP_AT_PATTERN.match(statement_upper)
        if loop_at_match:
            loop_workarea = loop_at_match.group("workarea").strip().lower()
            in_loop = True
            trace_path.append(
                f"Line {line_num+1}: LOOP AT {loop_at_match.group('table')} INTO {loop_workarea}"
            )

        # ENDLOOP 처리
        endloop_match = ENDLOOP_PATTERN.match(statement_upper)
        if endloop_match:
            if in_loop:
                trace_path.append(f"Line {line_num+1}: ENDLOOP")
                in_loop = False
                loop_workarea = None

        # LOOP 내부에서 workarea 필드 할당 추적
        if in_loop and loop_workarea:
            # workarea-field = variable 패턴 확인
            loop_field_match = re.match(
                rf"^\s*{re.escape(loop_workarea)}\-(?P<field>[\w\d_]+)\s*=\s*(?P<source>[\w\d\-\>\[\]]+)\s*\.",
                statement_upper,
                re.IGNORECASE,
            )
            if loop_field_match:
                source_var = loop_field_match.group("source").strip().lower()
                field_name = loop_field_match.group("field").strip().lower()
                target_field = f"{loop_workarea}-{field_name}"
                if source_var in tainted_vars:
                    tainted_vars.add(target_field)
                    trace_path.append(
                        f"Line {line_num+1}: LOOP workarea assignment '{source_var}' -> '{target_field}'"
                    )

        # 연속 할당 구문 처리: a = b = c.
        chain_assign_match = CHAIN_ASSIGN_PATTERN.match(statement_upper)
        if chain_assign_match:
            source_var = chain_assign_match.group("source").strip().lower()
            targets_str = chain_assign_match.group("targets")
            target_vars = [
                v.strip().lower() for v in targets_str.split("=") if v.strip()
            ]
            chain = target_vars + [source_var]
            for j in range(len(chain) - 2, -1, -1):
                right_var, left_var = chain[j + 1], chain[j]
                if right_var in tainted_vars and left_var not in tainted_vars:
                    tainted_vars.add(left_var)
                    trace_path.append(
                        f"Line {line_num+1}: Chained Assignment '{right_var}' -> '{left_var}'"
                    )

        # 연속적인 MOVE 구문(multi-line) 처리
        chain_move_match = CHAIN_MOVE_PATTERN.match(statement_upper)
        if chain_move_match:
            moves_str = chain_move_match.group("moves")
            move_pairs = MOVE_PAIR_PATTERN.finditer(moves_str)
            for move in move_pairs:
                source_var = move.group("source").strip().lower()
                target_var = move.group("target").strip().lower()
                if source_var in tainted_vars and target_var not in tainted_vars:
                    tainted_vars.add(target_var)
                    trace_path.append(
                        f"Line {line_num+1}: Chained MOVE '{source_var}' -> '{target_var}'"
                    )

        # 기본 변수 전파
        match = MOVE_PATTERN.match(statement_upper) or ASSIGN_PATTERN.match(
            statement_upper
        )
        if match:
            source_var = match.group("source").strip().lower()
            target_var = match.group("target").strip().lower()
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: Assignment '{source_var}' -> '{target_var}'"
                )

        # 구조체 필드 할당
        struct_match = STRUCTURE_ASSIGN_PATTERN.match(statement_upper)
        if struct_match:
            source_var = struct_match.group("source").strip().lower()
            target_field = struct_match.group("target").strip().lower()
            if source_var in tainted_vars:
                tainted_vars.add(target_field)
                trace_path.append(
                    f"Line {line_num+1}: Structure assignment '{source_var}' -> '{target_field}'"
                )

        # CONCATENATE 패턴 처리
        concat_match = CONCATENATE_PATTERN.match(statement_upper)
        if concat_match:
            sources_str = concat_match.group("sources")
            target_var = concat_match.group("target").strip().lower()
            # sources에서 오염된 변수 찾기
            for tainted_var in tainted_vars:
                if re.search(
                    rf"\b{re.escape(tainted_var)}\b", sources_str, re.IGNORECASE
                ):
                    if target_var not in tainted_vars:
                        tainted_vars.add(target_var)
                        trace_path.append(
                            f"Line {line_num+1}: CONCATENATE '{tainted_var}' -> '{target_var}'"
                        )
                    break

        # SPLIT 패턴 처리
        split_match = SPLIT_PATTERN.match(statement_upper)
        if split_match:
            source_var = split_match.group("source").strip().lower()
            targets_str = split_match.group("targets")
            if source_var in tainted_vars:
                # INTO 절의 모든 변수를 오염시킴
                target_vars = [
                    v.strip().lower() for v in targets_str.split() if v.strip()
                ]
                for target_var in target_vars:
                    if target_var not in tainted_vars:
                        tainted_vars.add(target_var)
                        trace_path.append(
                            f"Line {line_num+1}: SPLIT '{source_var}' -> '{target_var}'"
                        )

        # --- Sink 분석 (DB, RFC, 조건문 등) ---

        # 1. DB 작업 분석
        # UPDATE SET - 강화된 분석
        update_set_match = UPDATE_SET_SYUNAME_PATTERN.match(statement_upper)
        if update_set_match:
            table = update_set_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                assignments = update_set_match.group("assignments")
                affected_fields = []

                # 1) sy-uname 직접 사용 체크
                syuname_fields = UPDATE_FIELD_SYUNAME_PATTERN.finditer(assignments)
                for field_match in syuname_fields:
                    field = field_match.group("field").strip().upper()
                    affected_fields.append(field)
                    trace_path.append(
                        f"Line {line_num+1}: UPDATE SET direct sy-uname -> {table}.{field}"
                    )

                # 2) 오염된 변수들 체크
                for var in tainted_vars:
                    # 필드 할당에서 오염된 변수 찾기
                    field_match = re.search(
                        f"([\\w\\d_]+)\\s*=\\s*{re.escape(var)}",
                        assignments,
                        re.IGNORECASE,
                    )
                    if field_match:
                        field = field_match.group(1).upper()
                        if field not in affected_fields:
                            affected_fields.append(field)
                            trace_path.append(
                                f"Line {line_num+1}: UPDATE SET tainted variable '{var}' -> {table}.{field}"
                            )

                # 3) 구조체 필드 할당 체크
                structure_fields = UPDATE_FIELD_STRUCTURE_PATTERN.finditer(assignments)
                for struct_match in structure_fields:
                    field = struct_match.group("field").strip().upper()
                    source = struct_match.group("source").strip().lower()

                    # 소스가 오염된 구조체 필드인지 확인
                    if source in tainted_vars:
                        if field not in affected_fields:
                            affected_fields.append(field)
                            trace_path.append(
                                f"Line {line_num+1}: UPDATE SET structure field '{source}' -> {table}.{field}"
                            )

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_UPDATE_FIELD",
                        "table": table,
                        "fields": affected_fields,
                        "operation": "UPDATE_SET",
                        "trace_path": trace_path,
                    }

        # UPDATE FROM
        update_from_match = UPDATE_FROM_PATTERN.match(statement_upper)
        if update_from_match:
            table = update_from_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                source_var = update_from_match.group("source").strip().lower()
                base_structure_name = source_var.replace("[]", "")
                affected_fields = []
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)
                    # workarea 패턴도 확인
                    elif "-" in tainted_var and any(
                        pattern in tainted_var
                        for pattern in ["wa_", "s_", "ls_", "gs_"]
                    ):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_UPDATE_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                    }

        # INSERT VALUES 패턴들 처리 (우선 처리)
        # 1. INSERT INTO table VALUES 패턴 (직접 값 삽입)
        insert_values_match = INSERT_VALUES_PATTERN.match(statement_upper)
        if insert_values_match:
            table = insert_values_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                values_str = insert_values_match.group("values")
                # VALUES 절에서 sy-uname 또는 오염된 변수 찾기
                affected_fields = []
                if re.search(r"sy-uname", values_str, re.IGNORECASE):
                    trace_path.append(
                        f"Line {line_num+1}: INSERT VALUES direct sy-uname -> {table}"
                    )
                    return {
                        "status": "Found",
                        "type": "DATABASE_INSERT_VALUES",
                        "table": table,
                        "description": "SY-UNAME used in INSERT VALUES statement",
                        "operation": "INSERT_VALUES",
                    }

                # 오염된 변수들 체크
                for tainted_var in tainted_vars:
                    if re.search(
                        rf"\b{re.escape(tainted_var)}\b", values_str, re.IGNORECASE
                    ):
                        trace_path.append(
                            f"Line {line_num+1}: INSERT VALUES tainted variable '{tainted_var}' -> {table}"
                        )
                        return {
                            "status": "Found",
                            "type": "DATABASE_INSERT_VALUES",
                            "table": table,
                            "tainted_variable": tainted_var,
                            "operation": "INSERT_VALUES",
                        }

        # 2. INSERT table VALUES structure 패턴
        insert_values_struct_match = INSERT_VALUES_STRUCTURE_PATTERN.match(
            statement_upper
        )
        if insert_values_struct_match:
            table = insert_values_struct_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                structure = (
                    insert_values_struct_match.group("structure").strip().lower()
                )
                base_structure_name = structure.replace("[]", "")
                affected_fields = []

                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_INSERT_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                        "operation": "INSERT_VALUES_STRUCTURE",
                    }

        # 3. INSERT table FROM TABLE 패턴 (기존)
        insert_table_match = INSERT_TABLE_FROM_TABLE_PATTERN.match(statement_upper)
        if insert_table_match:
            table = insert_table_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                source_table = insert_table_match.group("source_table").strip().lower()
                base_table_name = source_table.replace("[]", "")

                # 소스 테이블의 오염된 필드들 찾기
                affected_fields = []
                for tainted_var in tainted_vars:
                    # 테이블 내부 구조체 필드들 확인
                    if tainted_var.startswith(base_table_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)
                    # workarea 필드들 확인 (LOOP에서 추가된 필드들)
                    elif "-" in tainted_var:
                        var_parts = tainted_var.split("-")
                        if len(var_parts) == 2:
                            wa_name, field_name = var_parts
                            # 일반적인 workarea 패턴들 확인
                            if any(
                                pattern in wa_name
                                for pattern in ["wa_", "s_", "ls_", "gs_"]
                            ):
                                affected_fields.append(field_name.upper())

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_INSERT_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),  # 중복 제거
                        "operation": "INSERT_TABLE",
                    }

        # INSERT (FROM or VALUES) - 기존 패턴
        insert_match = INSERT_PATTERN.match(statement_upper)
        if insert_match:
            table = insert_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                source = insert_match.group("source") or insert_match.group("values")
                if source:
                    source_var = source.strip().lower()
                    base_structure_name = source_var.replace("[]", "")
                    affected_fields = []
                    for tainted_var in tainted_vars:
                        if tainted_var.startswith(base_structure_name + "-"):
                            field = tainted_var.split("-", 1)[1].upper()
                            affected_fields.append(field)
                        # workarea 패턴도 확인
                        elif "-" in tainted_var and any(
                            pattern in tainted_var
                            for pattern in ["wa_", "s_", "ls_", "gs_"]
                        ):
                            field = tainted_var.split("-", 1)[1].upper()
                            affected_fields.append(field)

                    if affected_fields:
                        return {
                            "status": "Found",
                            "type": "DATABASE_INSERT_FIELD",
                            "table": table,
                            "fields": list(set(affected_fields)),
                        }

        # MODIFY 패턴들 처리 (확장된 패턴들)
        # 1. MODIFY table FROM TABLE 패턴 (내부 테이블에서 일괄 수정)
        modify_from_table_match = MODIFY_FROM_TABLE_PATTERN.match(statement_upper)
        if modify_from_table_match:
            table = modify_from_table_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                source_table = (
                    modify_from_table_match.group("source_table").strip().lower()
                )
                base_table_name = source_table.replace("[]", "")
                affected_fields = []

                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_table_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)
                    elif "-" in tainted_var:
                        var_parts = tainted_var.split("-")
                        if len(var_parts) == 2:
                            wa_name, field_name = var_parts
                            if any(
                                pattern in wa_name
                                for pattern in ["wa_", "s_", "ls_", "gs_"]
                            ):
                                affected_fields.append(field_name.upper())

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_MODIFY_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                        "operation": "MODIFY_FROM_TABLE",
                    }

        # 2. 단독 MODIFY 패턴 (workarea 사용)
        modify_workarea_match = MODIFY_WORKAREA_PATTERN.match(statement_upper)
        if modify_workarea_match:
            table = modify_workarea_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                # 현재 workarea에서 오염된 필드들 찾기
                affected_fields = []
                for tainted_var in tainted_vars:
                    # workarea 패턴 확인 (table 이름과 연관된 workarea)
                    if "-" in tainted_var:
                        var_parts = tainted_var.split("-")
                        if len(var_parts) == 2:
                            wa_name, field_name = var_parts
                            # table 이름과 관련된 workarea 또는 일반적인 workarea 패턴
                            if table.lower() in wa_name or any(
                                pattern in wa_name
                                for pattern in ["wa_", "s_", "ls_", "gs_"]
                            ):
                                affected_fields.append(field_name.upper())

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_MODIFY_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                        "operation": "MODIFY_WORKAREA",
                    }

        # 3. MODIFY (FROM or table.) - 기존 패턴들
        modify_match = MODIFY_PATTERN.match(statement_upper)
        if modify_match:
            table = modify_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                source_var = modify_match.group("source").strip().lower()
                base_structure_name = source_var.replace("[]", "")
                affected_fields = []
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)
                    # workarea 패턴도 확인
                    elif "-" in tainted_var and any(
                        pattern in tainted_var
                        for pattern in ["wa_", "s_", "ls_", "gs_"]
                    ):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_MODIFY_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                    }

        modify_table_match = MODIFY_TABLE_PATTERN.match(statement_upper)
        if modify_table_match:
            table = modify_table_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                base_structure_name = table.lower()
                affected_fields = []
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + "-"):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)
                    # workarea 패턴도 확인
                    elif "-" in tainted_var and any(
                        pattern in tainted_var
                        for pattern in ["wa_", "s_", "ls_", "gs_"]
                    ):
                        field = tainted_var.split("-", 1)[1].upper()
                        affected_fields.append(field)

                if affected_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_MODIFY_FIELD",
                        "table": table,
                        "fields": list(set(affected_fields)),
                    }

        # DELETE 문 분석
        delete_match = DELETE_PATTERN.match(statement_upper)
        if delete_match:
            table = delete_match.group("table").strip().upper()
            if table.startswith(("Z", "Y")):
                conditions = delete_match.group("conditions")
                # WHERE 조건에서 오염된 변수 찾기
                for tainted_var in tainted_vars:
                    if re.search(
                        rf"\b{re.escape(tainted_var)}\b", conditions, re.IGNORECASE
                    ):
                        return {
                            "status": "Found",
                            "type": "DATABASE_DELETE",
                            "table": table,
                            "condition_variable": tainted_var,
                        }

        # 2. RFC 호출 분석
        rfc_match = RFC_CALL_PATTERN.search(statement_upper)
        if rfc_match:
            rfc_name = rfc_match.group("rfc_name")
            params_str = rfc_match.group("params")
            param_matches = RFC_PARAM_PATTERN.finditer(params_str)
            for p_match in param_matches:
                param_value = p_match.group("param_value").strip().lower()
                base_structure_name = param_value.replace("[]", "")
                if param_value in tainted_vars:
                    return {
                        "status": "Found",
                        "type": "RFC",
                        "name": rfc_name,
                        "parameter": p_match.group("param_name").strip(),
                    }
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + "-"):
                        return {
                            "status": "Found",
                            "type": "RFC",
                            "name": rfc_name,
                            "parameter": p_match.group("param_name").strip(),
                        }

        # 3. SY-UNAME 하드코딩 분석 (조건문보다 먼저 체크)
        # IF문 내 하드코딩
        if_hardcode_match = IF_SYUNAME_HARDCODE_PATTERN.match(statement_upper)
        if if_hardcode_match:
            operator = if_hardcode_match.group("operator")
            value = if_hardcode_match.group("value")
            return {
                "status": "Found",
                "type": "SYUNAME_HARDCODE",
                "subtype": "IF_STATEMENT",
                "operator": operator,
                "hardcode_value": value,
                "description": f"SY-UNAME hardcoded comparison in IF: sy-uname {operator} {value}",
                "final_variable": "sy-uname",
            }

        # CHECK문 내 하드코딩
        check_hardcode_match = CHECK_SYUNAME_HARDCODE_PATTERN.match(statement_upper)
        if check_hardcode_match:
            operator = check_hardcode_match.group("operator")
            value = check_hardcode_match.group("value")
            return {
                "status": "Found",
                "type": "SYUNAME_HARDCODE",
                "subtype": "CHECK_STATEMENT",
                "operator": operator,
                "hardcode_value": value,
                "description": f"SY-UNAME hardcoded comparison in CHECK: sy-uname {operator} {value}",
                "final_variable": "sy-uname",
            }

        # WHERE절 내 하드코딩
        where_hardcode_match = WHERE_SYUNAME_HARDCODE_PATTERN.search(statement_upper)
        if where_hardcode_match:
            operator = where_hardcode_match.group("operator")
            value = where_hardcode_match.group("value")
            return {
                "status": "Found",
                "type": "SYUNAME_HARDCODE",
                "subtype": "WHERE_CLAUSE",
                "operator": operator,
                "hardcode_value": value,
                "description": f"SY-UNAME hardcoded comparison in WHERE: sy-uname {operator} {value}",
                "final_variable": "sy-uname",
            }

        # ASSERT문 내 하드코딩
        assert_hardcode_match = ASSERT_SYUNAME_HARDCODE_PATTERN.match(statement_upper)
        if assert_hardcode_match:
            operator = assert_hardcode_match.group("operator")
            value = assert_hardcode_match.group("value")
            return {
                "status": "Found",
                "type": "SYUNAME_HARDCODE",
                "subtype": "ASSERT_STATEMENT",
                "operator": operator,
                "hardcode_value": value,
                "description": f"SY-UNAME hardcoded comparison in ASSERT: sy-uname {operator} {value}",
                "final_variable": "sy-uname",
            }

        # 일반 하드코딩 패턴 (다른 문맥에서)
        general_hardcode_match = SYUNAME_HARDCODE_PATTERN.search(statement_upper)
        if general_hardcode_match:
            operator = general_hardcode_match.group("operator")
            value = general_hardcode_match.group("value")
            return {
                "status": "Found",
                "type": "SYUNAME_HARDCODE",
                "subtype": "GENERAL_COMPARISON",
                "operator": operator,
                "hardcode_value": value,
                "description": f"SY-UNAME hardcoded comparison: sy-uname {operator} {value}",
                "final_variable": "sy-uname",
            }

        # 4. WRITE 문 분석 (출력 sink)
        # 단순 WRITE 패턴
        write_simple_match = WRITE_SIMPLE_PATTERN.match(statement_upper)
        if write_simple_match:
            variable = write_simple_match.group("variable").strip().lower()
            if variable in tainted_vars:
                return {
                    "status": "Found",
                    "type": "WRITE_OUTPUT",
                    "description": f"SY-UNAME written to output via WRITE statement",
                    "final_variable": variable,
                }

        # 연속 WRITE 패턴 (콜론 사용)
        write_chain_match = WRITE_CHAIN_PATTERN.match(statement_upper)
        if write_chain_match:
            variables_str = write_chain_match.group("variables")
            write_vars = WRITE_VAR_PATTERN.finditer(variables_str)
            for var_match in write_vars:
                variable = var_match.group("variable").strip().lower()
                if variable in tainted_vars:
                    return {
                        "status": "Found",
                        "type": "WRITE_OUTPUT",
                        "description": f"SY-UNAME written to output via chained WRITE statement",
                        "final_variable": variable,
                    }

        # 여러 변수 WRITE 패턴
        write_multiple_match = WRITE_MULTIPLE_PATTERN.match(statement_upper)
        if write_multiple_match:
            variables_str = write_multiple_match.group("variables")
            # 콤마로 구분된 변수들 처리
            if "," in variables_str:
                write_vars = WRITE_VAR_PATTERN.finditer(variables_str)
                for var_match in write_vars:
                    variable = var_match.group("variable").strip().lower()
                    if variable in tainted_vars:
                        return {
                            "status": "Found",
                            "type": "WRITE_OUTPUT",
                            "description": f"SY-UNAME written to output via multiple WRITE statement",
                            "final_variable": variable,
                        }

                # 5. UPDATE 후 검증 패턴 처리 (일반 조건문 분석 이전에)
        # 현재 라인 이전 5줄 내에서 UPDATE 찾기
        update_found = False
        update_table = None

        for j in range(max(0, i - 5), i):
            if j < len(statements):
                prev_stmt, prev_line = statements[j]
                prev_stmt_upper = prev_stmt.upper()

                # UPDATE 패턴들 확인
                if (
                    UPDATE_SET_SYUNAME_PATTERN.match(prev_stmt_upper)
                    or UPDATE_FROM_PATTERN.match(prev_stmt_upper)
                    or UPDATE_FROM_TABLE_PATTERN.match(prev_stmt_upper)
                ):
                    update_found = True
                    # 테이블명 추출
                    for pattern in [
                        UPDATE_SET_SYUNAME_PATTERN,
                        UPDATE_FROM_PATTERN,
                        UPDATE_FROM_TABLE_PATTERN,
                    ]:
                        match = pattern.match(prev_stmt_upper)
                        if match:
                            update_table = match.group("table").strip().upper()
                            break
                    break

        if update_found and update_table:
            # 현재 라인에서 sy-uname 검증 패턴 찾기
            # IF sy-uname 검증
            if_validation_match = POST_UPDATE_IF_VALIDATION_PATTERN.match(
                statement_upper
            )
            if if_validation_match:
                return {
                    "status": "Found",
                    "type": "POST_UPDATE_VALIDATION",
                    "subtype": "IF_VALIDATION",
                    "table": update_table,
                    "description": f"SY-UNAME validation in IF statement after UPDATE on {update_table}",
                    "validation_line": line_num + 1,
                    "trace_path": trace_path
                    + [f"Line {line_num+1}: IF validation after UPDATE {update_table}"],
                }

            # CHECK sy-uname 검증
            check_validation_match = POST_UPDATE_CHECK_VALIDATION_PATTERN.match(
                statement_upper
            )
            if check_validation_match:
                return {
                    "status": "Found",
                    "type": "POST_UPDATE_VALIDATION",
                    "subtype": "CHECK_VALIDATION",
                    "table": update_table,
                    "description": f"SY-UNAME validation in CHECK statement after UPDATE on {update_table}",
                    "validation_line": line_num + 1,
                    "trace_path": trace_path
                    + [
                        f"Line {line_num+1}: CHECK validation after UPDATE {update_table}"
                    ],
                }

            # ASSERT sy-uname 검증
            assert_validation_match = POST_UPDATE_ASSERT_VALIDATION_PATTERN.match(
                statement_upper
            )
            if assert_validation_match:
                return {
                    "status": "Found",
                    "type": "POST_UPDATE_VALIDATION",
                    "subtype": "ASSERT_VALIDATION",
                    "table": update_table,
                    "description": f"SY-UNAME validation in ASSERT statement after UPDATE on {update_table}",
                    "validation_line": line_num + 1,
                    "trace_path": trace_path
                    + [
                        f"Line {line_num+1}: ASSERT validation after UPDATE {update_table}"
                    ],
                }

        # 6. 일반 조건문 분석 (하드코딩 패턴 이후에 체크)
        conditional_match = CONDITIONAL_CHECK_PATTERN.match(statement_upper)
        if conditional_match:
            variable = conditional_match.group("variable").strip().lower()
            if variable in tainted_vars:
                value = conditional_match.group("value").strip()
                return {
                    "status": "Found",
                    "type": "CONDITIONAL_CHECK",
                    "description": f"Hardcoded check against {value}",
                }

    # 5단계: 추적 완료 후에도 sink를 찾지 못한 경우 구체적인 이유 제공
    return {
        "status": "Not Found",
        "reason": "SY-UNAME variable flow traced but no valid sink found",
        "verified_syuname_line": actual_sy_uname_line + 1,
        "tainted_variables": list(tainted_vars),
        "trace_path": trace_path,
        "error_type": "NO_SINK_FOUND_AFTER_TRACING",
        "analysis_summary": {
            "total_statements_analyzed": len(statements) - start_statement_idx,
            "variables_propagated": len(tainted_vars) - 1,  # sy-uname 제외
            "trace_steps": len(trace_path),
        },
    }

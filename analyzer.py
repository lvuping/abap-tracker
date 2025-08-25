import re
from patterns import *  # 정규식 패턴들을 가져옴
from extended_patterns import * # 확장된 정규식 패턴들을 가져옴

# 전역 변수 - 감사 필드 임시 결과 저장
temp_audit_result = None


def trace_sy_uname_in_snippet(snippet, start_line_in_snippet):
    """
    주어진 코드 조각(snippet) 내에서 sy-uname의 흐름을 추적한다.
    """
    global temp_audit_result
    temp_audit_result = None  # 함수 시작 시 초기화

    tainted_vars = {"sy-uname"}  # 오염된 변수들을 저장할 집합(set), sy-uname으로 시작
    trace_path = []  # 추적 경로를 저장할 리스트
    form_params = {}  # FORM 파라미터 매핑을 저장
    in_move_chain = False # 연속적인 MOVE 구문 추적 플래그

    # 전체 스니펫에서 FORM 정의를 먼저 찾아서 파라미터 매핑을 구성
    for line_num, line in enumerate(snippet):
        line_upper = line.strip().upper()
        form_match = FORM_PARAM_PATTERN.match(line_upper)
        if form_match:
            params_str = form_match.group("params")

            # USING 파라미터 처리
            using_match = USING_PARAM_PATTERN.search(params_str)
            if using_match:
                using_params = using_match.group("using_params").split()
                for i, param in enumerate(using_params):
                    if "VALUE(" in param:
                        param = param.replace("VALUE(", "").replace(")", "")
                    param = param.split()[0]  # TYPE 부분 제거
                    form_params[f"USING_PARAM_{i}"] = param.strip().lower()

            # CHANGING 파라미터 처리
            changing_match = CHANGING_PARAM_PATTERN.search(params_str)
            if changing_match:
                changing_params = changing_match.group("changing_params").split()
                for i, param in enumerate(changing_params):
                    param = param.split()[0]  # TYPE 부분 제거
                    form_params[f"CHANGING_PARAM_{i}"] = param.strip().lower()

    # 시작점부터 아래로 내려가며 한 줄씩 분석
    for line_num, line in enumerate(
        snippet[start_line_in_snippet:], start=start_line_in_snippet
    ):
        line_upper = line.strip().upper()  # 대소문자 무시, 공백 제거
        original_line_for_chain_check = line.strip()

        # 연속적인 MOVE 구문(multi-line) 처리
        if not in_move_chain and line_upper.startswith("MOVE:"):
            in_move_chain = True

        if in_move_chain:
            line_to_parse = re.sub(r"^\s*MOVE\s*:", "", line_upper, count=1)
            pairs = MOVE_PAIR_PATTERN.finditer(line_to_parse)
            for match in pairs:
                source_var = match.group("source").strip().lower()
                target_var = match.group("target").strip().lower()
                if source_var in tainted_vars and target_var not in tainted_vars:
                    tainted_vars.add(target_var)
                    trace_path.append(
                        f"Line {line_num+1}: Chained MOVE '{source_var}' -> '{target_var}'"
                    )
            
            if original_line_for_chain_check.endswith("."):
                in_move_chain = False
            
            continue

        # 1. 기본 변수 전파 (Propagation) 분석
        # 예: MOVE lv_source TO lv_target. 또는 lv_target = lv_source.
        match = MOVE_PATTERN.match(line_upper) or ASSIGN_PATTERN.match(line_upper)
        if match:
            source_var = match.group("source").strip().lower()
            target_var = match.group("target").strip().lower()

            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: Assignment '{source_var}' -> '{target_var}'"
                )
        
        # 연속 할당 구문 처리: a = b = c.
        chain_assign_match = CHAIN_ASSIGN_PATTERN.match(line_upper)
        if chain_assign_match:
            source_var = chain_assign_match.group("source").strip().lower()
            targets_str = chain_assign_match.group("targets")
            target_vars = [v.strip().lower() for v in targets_str.split('=') if v.strip()]
            
            chain = target_vars + [source_var]

            # 오른쪽에서 왼쪽으로 오염 전파 (c -> b -> a)
            for i in range(len(chain) - 2, -1, -1):
                right_var = chain[i+1]
                left_var = chain[i]

                if right_var in tainted_vars and left_var not in tainted_vars:
                    tainted_vars.add(left_var)
                    trace_path.append(
                        f"Line {line_num+1}: Chained Assignment '{right_var}' -> '{left_var}'"
                    )

        # 새로운 패턴: 연속적인 MOVE
        chain_move_match = CHAIN_MOVE_PATTERN.match(line_upper)
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

        # 새로운 패턴: DATA 선언 (TYPE)
        data_type_match = DATA_TYPE_PATTERN.match(line_upper)
        if data_type_match:
            source_var = data_type_match.group("source").strip().lower()
            target_var = data_type_match.group("target").strip().lower()
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: DATA (TYPE) declaration '{source_var}' -> '{target_var}'"
                )

        # 새로운 패턴: DATA 선언 (LIKE)
        data_like_match = DATA_LIKE_PATTERN.match(line_upper)
        if data_like_match:
            source_var = data_like_match.group("source").strip().lower()
            target_var = data_like_match.group("target").strip().lower()
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: DATA (LIKE) declaration '{source_var}' -> '{target_var}'"
                )

        # 새로운 패턴: SELECT-OPTIONS
        select_options_match = SELECT_OPTIONS_PATTERN.match(line_upper)
        if select_options_match:
            source_var = select_options_match.group("source").strip().lower()
            target_var = select_options_match.group("target").strip().lower()
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: SELECT-OPTIONS declaration '{source_var}' -> '{target_var}'"
                )

        # 1-1. MOVE-CORRESPONDING 분석
        move_corr_match = MOVE_CORRESPONDING_PATTERN.match(line_upper)
        if move_corr_match:
            source_var = move_corr_match.group("source").strip().lower()
            target_var = move_corr_match.group("target").strip().lower()

            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: MOVE-CORRESPONDING '{source_var}' -> '{target_var}'"
                )

        # 1-2. CONCATENATE 분석
        concat_match = CONCATENATE_PATTERN.match(line_upper)
        if concat_match:
            sources_str = concat_match.group("sources").strip()
            target_var = concat_match.group("target").strip().lower()

            # CONCATENATE의 소스 변수들을 분석
            source_vars = [s.strip().lower() for s in sources_str.split() if s.strip()]
            for source_var in source_vars:
                if source_var in tainted_vars and target_var not in tainted_vars:
                    tainted_vars.add(target_var)
                    trace_path.append(
                        f"Line {line_num+1}: CONCATENATE '{source_var}' -> '{target_var}'"
                    )
                    break

        # 1-3. REPLACE 분석 (IN 절의 변수가 오염되는 경우)
        replace_match = REPLACE_PATTERN.match(line_upper)
        if replace_match:
            target_var = replace_match.group("target").strip().lower()
            source_var = replace_match.group("source").strip().lower()

            # REPLACE는 target 변수를 수정하고, source를 사용
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(
                    f"Line {line_num+1}: REPLACE using '{source_var}' in '{target_var}'"
                )

        # 1-4. SPLIT 분석
        split_match = SPLIT_PATTERN.match(line_upper)
        if split_match:
            source_var = split_match.group("source").strip().lower()
            targets_str = split_match.group("targets").strip()

            if source_var in tainted_vars:
                # SPLIT의 타겟 변수들을 분석
                target_vars = [
                    t.strip().lower() for t in targets_str.split() if t.strip()
                ]
                for target_var in target_vars:
                    if target_var not in tainted_vars:
                        tainted_vars.add(target_var)
                        trace_path.append(
                            f"Line {line_num+1}: SPLIT '{source_var}' -> '{target_var}'"
                        )

        # 1-5. SELECT INTO 분석
        select_match = SELECT_INTO_PATTERN.match(line_upper)
        if select_match:
            target_var = select_match.group("target").strip().lower()
            # SELECT INTO는 새로운 데이터를 가져오므로 오염 추적하지 않음
            # 하지만 WHERE 절에서 오염된 변수를 사용할 수 있음

        # 2. 구조체 필드 할당 분석
        # 예: gs_po_list-lifnr = lv_lifnr.
        struct_match = STRUCTURE_ASSIGN_PATTERN.match(line_upper)
        if struct_match:
            source_var = struct_match.group("source").strip().lower()
            target_field = struct_match.group("target").strip().lower()

            if source_var in tainted_vars:
                tainted_vars.add(target_field)
                trace_path.append(
                    f"Line {line_num+1}: Structure assignment '{source_var}' -> '{target_field}'"
                )

        # 3. PERFORM 호출 분석
        # 예: PERFORM call_remote_system USING p_bukrs p_ekorg lv_lifnr
        perform_match = PERFORM_PATTERN.match(line_upper)
        if perform_match:
            subroutine = perform_match.group("subroutine").strip()
            params_str = perform_match.group("params")

            # USING 파라미터 분석
            using_match = USING_PARAM_PATTERN.search(params_str)
            if using_match:
                using_params = [
                    p.strip().lower() for p in using_match.group("using_params").split()
                ]
                for i, param in enumerate(using_params):
                    if param in tainted_vars:
                        trace_path.append(
                            f"Line {line_num+1}: PERFORM '{subroutine}' USING parameter {i+1}: '{param}'"
                        )
                        # FORM 정의에서 해당 파라미터에 매핑된 변수를 오염시킴
                        form_param_key = f"USING_PARAM_{i}"
                        if form_param_key in form_params:
                            mapped_var = form_params[form_param_key]
                            tainted_vars.add(mapped_var)
                            trace_path.append(
                                f"Line {line_num+1}: FORM parameter mapping: '{param}' -> '{mapped_var}'"
                            )

            # CHANGING 파라미터 분석
            changing_match = CHANGING_PARAM_PATTERN.search(params_str)
            if changing_match:
                changing_params = [
                    p.strip().lower()
                    for p in changing_match.group("changing_params").split()
                ]
                for i, param in enumerate(changing_params):
                    if param in tainted_vars:
                        trace_path.append(
                            f"Line {line_num+1}: PERFORM '{subroutine}' CHANGING parameter {i+1}: '{param}'"
                        )

        # 4. WHERE 조건절에서 변수 사용 분석 (SELECT 문 등)
        where_match = WHERE_CONDITION_PATTERN.search(line_upper)
        if where_match:
            variable = where_match.group("variable").strip().lower()
            if variable in tainted_vars:
                trace_path.append(
                    f"Line {line_num+1}: WHERE condition uses tainted variable: '{variable}'"
                )

        # 새로운 패턴: READ TABLE WITH KEY
        read_table_match = READ_TABLE_WITH_KEY_PATTERN.search(line_upper)
        if read_table_match:
            source_var = read_table_match.group("source").strip().lower()
            if source_var in tainted_vars:
                trace_path.append(
                    f"Line {line_num+1}: READ TABLE WITH KEY uses tainted variable: '{source_var}'"
                )

        # 5. APPEND 등 내부 테이블 조작
        append_match = APPEND_PATTERN.match(line_upper)
        if append_match:
            source_var = append_match.group("source").strip().lower()
            target_table = append_match.group("target").strip().lower()

            if source_var in tainted_vars:
                tainted_vars.add(target_table)
                trace_path.append(
                    f"Line {line_num+1}: APPEND '{source_var}' -> '{target_table}'"
                )

        # 6. 종착점 (Sink) 분석 - RFC 호출
        # 예: CALL FUNCTION 'Z_RFC_NAME' EXPORTING iv_user = lv_uname.
        rfc_match = RFC_CALL_PATTERN.search(line_upper)
        if rfc_match:
            rfc_name = rfc_match.group("rfc_name")
            params_str = rfc_match.group("params")

            # 현재 라인에서 RFC 파라미터 확인
            param_matches = RFC_PARAM_PATTERN.finditer(params_str)
            for p_match in param_matches:
                param_name = p_match.group("param_name").strip()
                param_value = p_match.group("param_value").strip().lower()

                # 파라미터 값으로 오염된 변수가 사용되었다면, 종착점을 찾은 것!
                if param_value in tainted_vars:
                    return {
                        "status": "Found",
                        "type": "RFC",
                        "name": rfc_name,
                        "parameter": param_name,
                        "final_variable": param_value,
                        "path": trace_path,
                        "tainted_variables": list(tainted_vars),
                    }

            # 현재 라인에서 파라미터를 찾지 못한 경우, 다음 몇 줄을 확인
            # (여러 줄에 걸친 RFC 호출 처리)
            for next_offset in range(1, min(10, len(snippet) - line_num)):
                if line_num + next_offset < len(snippet):
                    next_line = snippet[line_num + next_offset].strip().upper()

                    # 다른 CALL FUNCTION이 나오거나 프로시저가 끝나면 중단
                    if (
                        "CALL FUNCTION" in next_line
                        or next_line.strip().endswith(".")
                        and "EXCEPTIONS" not in next_line
                    ):
                        break

                    # 다음 라인에서 RFC 파라미터 찾기
                    next_param_matches = RFC_PARAM_PATTERN.finditer(next_line)
                    for p_match in next_param_matches:
                        param_name = p_match.group("param_name").strip()
                        param_value = p_match.group("param_value").strip().lower()

                        if param_value in tainted_vars:
                            return {
                                "status": "Found",
                                "type": "RFC",
                                "name": rfc_name,
                                "parameter": param_name,
                                "final_variable": param_value,
                                "path": trace_path,
                                "tainted_variables": list(tainted_vars),
                            }

        # 7. 데이터베이스 작업 분석 (UPDATE, INSERT, MODIFY, DELETE) - Z/Y 테이블만 해당

        # 8-1. UPDATE...SET... 문 분석 (필드별 분석)
        update_set_match = UPDATE_SET_PATTERN.match(line_upper)
        if update_set_match:
            table = update_set_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                assignments = update_set_match.group("assignments").strip()
                updated_fields = []
                assignment_parts = [part.strip() for part in assignments.split(",")]

                for assignment in assignment_parts:
                    if "=" in assignment:
                        field_part, value_part = assignment.split("=", 1)
                        field_name = field_part.strip().upper()
                        value_var = value_part.strip().lower()
                        if not (value_var.startswith("'") or value_var.startswith('"')):
                            if value_var in tainted_vars:
                                updated_fields.append(field_name)
                if updated_fields:
                    return {
                        "status": "Found",
                        "type": "DATABASE_UPDATE_FIELD",
                        "table": table,
                        "fields": updated_fields,
                        "operation": "UPDATE",
                        "final_variable": None,
                        "path": trace_path,
                        "tainted_variables": list(tainted_vars),
                        "description": f"테이블 {table}의 {', '.join(updated_fields)} 필드에 사용자 정보 UPDATE",
                    }
                for var in tainted_vars:
                    if var.upper() in assignments.upper():
                        return {
                            "status": "Found",
                            "type": "DATABASE_UPDATE",
                            "table": table,
                            "operation": "UPDATE",
                            "final_variable": var,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table} UPDATE에서 사용자 정보 사용",
                        }
        
        # 8-1b. UPDATE ... FROM ... 문 분석
        update_from_match = UPDATE_FROM_PATTERN.match(line_upper)
        if update_from_match:
            table = update_from_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source = update_from_match.group("source")
                if source:
                    source_var = source.strip().lower()
                    base_structure_name = source_var.replace('[]', '')
                    if base_structure_name in tainted_vars:
                        return {
                            "status": "Found",
                            "type": "DATABASE_UPDATE",
                            "table": table,
                            "operation": "UPDATE",
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table} UPDATE FROM에서 전체 구조/테이블 '{base_structure_name}' 사용",
                        }
                    structure_fields = []
                    for tainted_var in tainted_vars:
                        if tainted_var.startswith(base_structure_name + "-"):
                            field_name = tainted_var.split("-", 1)[1].upper()
                            structure_fields.append(field_name)
                    if structure_fields:
                        return {
                            "status": "Found",
                            "type": "DATABASE_UPDATE_FIELD",
                            "table": table,
                            "fields": list(set(structure_fields)),
                            "operation": "UPDATE",
                            "source_structure": base_structure_name,
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table}의 {', '.join(list(set(structure_fields)))} 필드에 사용자 정보 UPDATE",
                        }

        # 8-2. INSERT 문 분석 (구조체-테이블 매핑 포함)
        insert_match = INSERT_PATTERN.match(line_upper)
        if insert_match:
            table = insert_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source = insert_match.group("source")
                values = insert_match.group("values")
                if source:
                    source_var = source.strip().lower()
                    base_structure_name = source_var.replace('[]', '')
                    if base_structure_name in tainted_vars:
                        return {
                            "status": "Found",
                            "type": "DATABASE_INSERT",
                            "table": table,
                            "operation": "INSERT",
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table} INSERT에서 전체 구조/테이블 '{base_structure_name}' 사용",
                        }
                    structure_fields = []
                    for tainted_var in tainted_vars:
                        if tainted_var.startswith(base_structure_name + "-"):
                            field_name = tainted_var.split("-", 1)[1].upper()
                            structure_fields.append(field_name)
                    if structure_fields:
                        return {
                            "status": "Found",
                            "type": "DATABASE_INSERT_FIELD",
                            "table": table,
                            "fields": list(set(structure_fields)),
                            "operation": "INSERT",
                            "source_structure": base_structure_name,
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table}의 {', '.join(list(set(structure_fields)))} 필드에 사용자 정보 INSERT",
                        }
                elif values:
                    for var in tainted_vars:
                        if var.upper() in values.upper():
                            return {
                                "status": "Found",
                                "type": "DATABASE_INSERT",
                                "table": table,
                                "operation": "INSERT",
                                "final_variable": var,
                                "path": trace_path,
                                "tainted_variables": list(tainted_vars),
                                "description": f"테이블 {table} INSERT VALUES에서 사용자 정보 사용",
                            }

        # 8-3. MODIFY 문 분석 (구조체-테이블 매핑 포함)
        modify_match = MODIFY_PATTERN.match(line_upper)
        if modify_match:
            table = modify_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source = modify_match.group("source")
                if source:
                    source_var = source.strip().lower()
                    base_structure_name = source_var.replace('[]', '')
                    if base_structure_name in tainted_vars:
                        return {
                            "status": "Found",
                            "type": "DATABASE_MODIFY",
                            "table": table,
                            "operation": "MODIFY",
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table} MODIFY에서 전체 구조/테이블 '{base_structure_name}' 사용",
                        }
                    structure_fields = []
                    for tainted_var in tainted_vars:
                        if tainted_var.startswith(base_structure_name + "-"):
                            field_name = tainted_var.split("-", 1)[1].upper()
                            structure_fields.append(field_name)
                    if structure_fields:
                        return {
                            "status": "Found",
                            "type": "DATABASE_MODIFY_FIELD",
                            "table": table,
                            "fields": list(set(structure_fields)),
                            "operation": "MODIFY",
                            "source_structure": base_structure_name,
                            "final_variable": base_structure_name,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table}의 {', '.join(list(set(structure_fields)))} 필드에 사용자 정보 MODIFY",
                        }
        
        # 새로운 패턴: MODIFY table.
        modify_table_match = MODIFY_TABLE_PATTERN.match(line_upper)
        if modify_table_match:
            table_name = modify_table_match.group("table").strip().lower()
            if table_name.upper().startswith(('Z', 'Y')):
                if table_name in tainted_vars:
                    return {
                        "status": "Found",
                        "type": "DATABASE_MODIFY",
                        "table": table_name.upper(),
                        "operation": "MODIFY",
                        "final_variable": table_name,
                        "path": trace_path,
                        "tainted_variables": list(tainted_vars),
                        "description": f"테이블 {table_name.upper()} MODIFY에서 사용자 정보 사용 (work area)",
                    }

        # 8-4. DELETE 문 분석
        delete_match = DELETE_PATTERN.match(line_upper)
        if delete_match:
            table = delete_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                conditions = delete_match.group("conditions").strip()
                for var in tainted_vars:
                    if var.upper() in conditions.upper():
                        return {
                            "status": "Found",
                            "type": "DATABASE_DELETE",
                            "table": table,
                            "operation": "DELETE",
                            "final_variable": var,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                            "description": f"테이블 {table} DELETE WHERE 조건에서 사용자 정보 사용",
                        }

        # 9. CALL TRANSACTION 분석
        call_trans_match = CALL_TRANSACTION_PATTERN.match(line_upper)
        if call_trans_match:
            tcode = call_trans_match.group("tcode").strip()
            params = call_trans_match.group("params").strip()

            # 파라미터에서 오염된 변수 사용 확인
            for var in tainted_vars:
                if var.upper() in params.upper():
                    return {
                        "status": "Found",
                        "type": "CALL_TRANSACTION",
                        "transaction": tcode,
                        "final_variable": var,
                        "path": trace_path,
                        "tainted_variables": list(tainted_vars),
                        "description": f"트랜잭션 {tcode} 호출에서 사용자 정보 사용",
                    }

        # 10. 감사 필드 할당 분석 (ERDAT, AENAM 등 중요 필드들) - 낮은 우선순위
        # 바로 return하지 않고 저장만 함 (더 중요한 DB 작업이 나중에 발견될 수 있음)
        audit_match = AUDIT_FIELD_PATTERN.match(line_upper)
        if audit_match:
            structure = audit_match.group("structure").strip().lower()
            field = audit_match.group("field").strip().upper()
            source_var = audit_match.group("source").strip().lower()

            if source_var in tainted_vars:
                # 감사 필드는 임시로 저장만 하고 계속 스캔
                temp_audit_result = {
                    "status": "Found",
                    "type": "AUDIT_FIELD",
                    "structure": structure,
                    "field": field,
                    "final_variable": source_var,
                    "path": trace_path.copy(),
                    "tainted_variables": list(tainted_vars),
                    "description": f"감사 필드 {structure}-{field}에 사용자 정보 할당",
                }

        # 11. SELECT WHERE 절 분석 (데이터베이스 조회 조건)
        select_where_match = SELECT_WHERE_PATTERN.search(line_upper)
        if select_where_match:
            table = select_where_match.group("table").strip().upper()
            field = select_where_match.group("field").strip().upper()
            variable = select_where_match.group("variable").strip().lower()
            if variable in tainted_vars:
                return {
                    "status": "Found",
                    "type": "DATABASE_SELECT_WHERE",
                    "table": table,
                    "fields": [field],
                    "operation": "SELECT",
                    "final_variable": variable,
                    "path": trace_path,
                    "tainted_variables": list(tainted_vars),
                    "description": f"테이블 {table}의 {field} 필드를 WHERE 조건으로 사용자 정보 조회",
                }

        # 12. 단독 WHERE 조건 분석 (여러 줄 SELECT문에서)
        standalone_where_match = STANDALONE_WHERE_PATTERN.match(line_upper)
        if standalone_where_match:
            field = standalone_where_match.group("field").strip().upper()
            variable = standalone_where_match.group("variable").strip().lower()
            if variable in tainted_vars:
                # 이전 라인들에서 FROM 절을 찾아서 테이블명 추출
                current_table = None
                for prev_offset in range(1, min(15, line_num + 1)):
                    prev_line_idx = line_num - prev_offset
                    if prev_line_idx >= 0:
                        prev_line = snippet[prev_line_idx].strip().upper()
                        from_match = re.search(r"FROM\s+(\w+)", prev_line)
                        if from_match:
                            current_table = from_match.group(1).strip().upper()
                            break
                        # SELECT 문의 시작을 발견하면 중단
                        if "SELECT" in prev_line and "FROM" in prev_line:
                            break

                if current_table:
                    return {
                        "status": "Found",
                        "type": "DATABASE_SELECT_WHERE",
                        "table": current_table,
                        "fields": [field],
                        "operation": "SELECT",
                        "final_variable": variable,
                        "path": trace_path,
                        "tainted_variables": list(tainted_vars),
                        "description": f"테이블 {current_table}의 {field} 필드를 WHERE 조건으로 사용자 정보 조회",
                    }

        # 13. 단독 RFC 파라미터 라인 처리 (EXPORTING, IMPORTING 등)
        if any(
            keyword in line_upper
            for keyword in ["EXPORTING", "IMPORTING", "CHANGING", "TABLES"]
        ):
            # 현재 RFC 컨텍스트에서 파라미터 라인인지 확인
            # 이전 몇 라인에서 CALL FUNCTION을 찾기
            current_rfc_name = None
            for prev_offset in range(1, min(10, line_num + 1)):
                prev_line_idx = line_num - prev_offset
                if prev_line_idx >= 0:
                    prev_line = snippet[prev_line_idx].strip().upper()
                    prev_rfc_match = RFC_CALL_PATTERN.search(prev_line)
                    if prev_rfc_match:
                        current_rfc_name = prev_rfc_match.group("rfc_name")
                        break
                    # 다른 CALL FUNCTION이나 완전히 다른 문장이 나오면 중단
                    if "CALL FUNCTION" in prev_line or (
                        prev_line.strip().endswith(".")
                        and "EXCEPTIONS" not in prev_line
                        and "DESTINATION" not in prev_line
                    ):
                        break

            # RFC 컨텍스트에서 파라미터 확인
            if current_rfc_name:
                param_matches = RFC_PARAM_PATTERN.finditer(line_upper)
                for p_match in param_matches:
                    param_name = p_match.group("param_name").strip()
                    param_value = p_match.group("param_value").strip().lower()

                    if param_value in tainted_vars:
                        return {
                            "status": "Found",
                            "type": "RFC",
                            "name": current_rfc_name,
                            "parameter": param_name,
                            "final_variable": param_value,
                            "path": trace_path,
                            "tainted_variables": list(tainted_vars),
                        }

    # 전체 스캔 완료 후 - 데이터베이스 작업이 없으면 감사 필드 결과 반환
    if temp_audit_result:
        result = temp_audit_result
        temp_audit_result = None  # 초기화
        return result

    return {
        "status": "Not Found in Snippet",
        "tainted_variables": list(tainted_vars),
        "path": trace_path,
    }
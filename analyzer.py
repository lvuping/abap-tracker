
import re
from patterns import *
from extended_patterns import *

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
        if line.strip().startswith('*'):
            continue
        line = line.split('"')[0]
        
        clean_line = line.strip()
        if not clean_line:
            continue

        if statement_start_line == -1:
            statement_start_line = i

        current_statement += " " + clean_line
        
        if clean_line.endswith('.'):
            statements.append((current_statement.strip(), statement_start_line))
            current_statement = ""
            statement_start_line = -1
            
    return statements

def trace_sy_uname_in_snippet(snippet, start_line_in_snippet):
    global temp_audit_result
    temp_audit_result = None
    
    statements = group_statements(snippet)
    tainted_vars = {"sy-uname"}
    trace_path = []

    # sy-uname이 포함된 문장을 찾아서 그 지점부터 분석 시작
    start_statement_idx = 0
    for i, (stmt, line_num) in enumerate(statements):
        if line_num <= start_line_in_snippet < (statements[i+1][1] if i+1 < len(statements) else float('inf')):
            if 'SY-UNAME' in stmt.upper():
                start_statement_idx = i
                break

    for i in range(start_statement_idx, len(statements)):
        statement, line_num = statements[i]
        statement_upper = statement.upper()

        # --- 모든 패턴 매칭은 이제 statement_upper를 대상으로 수행 ---

        # 연속 할당 구문 처리: a = b = c.
        chain_assign_match = CHAIN_ASSIGN_PATTERN.match(statement_upper)
        if chain_assign_match:
            source_var = chain_assign_match.group("source").strip().lower()
            targets_str = chain_assign_match.group("targets")
            target_vars = [v.strip().lower() for v in targets_str.split('=') if v.strip()]
            chain = target_vars + [source_var]
            for j in range(len(chain) - 2, -1, -1):
                right_var, left_var = chain[j+1], chain[j]
                if right_var in tainted_vars and left_var not in tainted_vars:
                    tainted_vars.add(left_var)
                    trace_path.append(f"Line {line_num+1}: Chained Assignment '{right_var}' -> '{left_var}'")

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
                    trace_path.append(f"Line {line_num+1}: Chained MOVE '{source_var}' -> '{target_var}'")

        # 기본 변수 전파
        match = MOVE_PATTERN.match(statement_upper) or ASSIGN_PATTERN.match(statement_upper)
        if match:
            source_var = match.group("source").strip().lower()
            target_var = match.group("target").strip().lower()
            if source_var in tainted_vars and target_var not in tainted_vars:
                tainted_vars.add(target_var)
                trace_path.append(f"Line {line_num+1}: Assignment '{source_var}' -> '{target_var}'")

        # 구조체 필드 할당
        struct_match = STRUCTURE_ASSIGN_PATTERN.match(statement_upper)
        if struct_match:
            source_var = struct_match.group("source").strip().lower()
            target_field = struct_match.group("target").strip().lower()
            if source_var in tainted_vars:
                tainted_vars.add(target_field)
                trace_path.append(f"Line {line_num+1}: Structure assignment '{source_var}' -> '{target_field}'")

        # --- Sink 분석 (DB, RFC, 조건문 등) ---

        # 1. DB 작업 분석
        # UPDATE SET
        update_set_match = UPDATE_SET_PATTERN.match(statement_upper)
        if update_set_match:
            table = update_set_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                assignments = update_set_match.group("assignments")
                for var in tainted_vars:
                    # 필드 할당에서 오염된 변수 찾기
                    field_match = re.search(f"([\w\d_]+)\s*=\s*{re.escape(var)}", assignments, re.IGNORECASE)
                    if field_match:
                        field = field_match.group(1).upper()
                        return {"status": "Found", "type": "DATABASE_UPDATE_FIELD", "table": table, "fields": [field]}
        
        # UPDATE FROM
        update_from_match = UPDATE_FROM_PATTERN.match(statement_upper)
        if update_from_match:
            table = update_from_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source_var = update_from_match.group("source").strip().lower()
                base_structure_name = source_var.replace('[]', '')
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + '-'):
                        field = tainted_var.split('-', 1)[1].upper()
                        return {"status": "Found", "type": "DATABASE_UPDATE_FIELD", "table": table, "fields": [field]}

        # INSERT (FROM or VALUES)
        insert_match = INSERT_PATTERN.match(statement_upper)
        if insert_match:
            table = insert_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source = insert_match.group("source") or insert_match.group("values")
                if source:
                    source_var = source.strip().lower()
                    base_structure_name = source_var.replace('[]', '')
                    for tainted_var in tainted_vars:
                        if tainted_var.startswith(base_structure_name + '-'):
                            field = tainted_var.split('-', 1)[1].upper()
                            return {"status": "Found", "type": "DATABASE_INSERT_FIELD", "table": table, "fields": [field]}

        # MODIFY (FROM or table.)
        modify_match = MODIFY_PATTERN.match(statement_upper)
        if modify_match:
            table = modify_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                source_var = modify_match.group("source").strip().lower()
                base_structure_name = source_var.replace('[]', '')
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + '-'):
                        field = tainted_var.split('-', 1)[1].upper()
                        return {"status": "Found", "type": "DATABASE_MODIFY_FIELD", "table": table, "fields": [field]}
        
        modify_table_match = MODIFY_TABLE_PATTERN.match(statement_upper)
        if modify_table_match:
            table = modify_table_match.group("table").strip().upper()
            if table.startswith(('Z', 'Y')):
                base_structure_name = table.lower()
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + '-'):
                        field = tainted_var.split('-', 1)[1].upper()
                        return {"status": "Found", "type": "DATABASE_MODIFY_FIELD", "table": table, "fields": [field]}

        # 2. RFC 호출 분석
        rfc_match = RFC_CALL_PATTERN.search(statement_upper)
        if rfc_match:
            rfc_name = rfc_match.group("rfc_name")
            params_str = rfc_match.group("params")
            param_matches = RFC_PARAM_PATTERN.finditer(params_str)
            for p_match in param_matches:
                param_value = p_match.group("param_value").strip().lower()
                base_structure_name = param_value.replace('[]', '')
                if param_value in tainted_vars:
                     return {"status": "Found", "type": "RFC", "name": rfc_name, "parameter": p_match.group("param_name").strip()}
                for tainted_var in tainted_vars:
                    if tainted_var.startswith(base_structure_name + '-'):
                        return {"status": "Found", "type": "RFC", "name": rfc_name, "parameter": p_match.group("param_name").strip()}

        # 3. 조건문 분석
        conditional_match = CONDITIONAL_CHECK_PATTERN.match(statement_upper)
        if conditional_match:
            variable = conditional_match.group("variable").strip().lower()
            if variable in tainted_vars:
                value = conditional_match.group("value").strip()
                return {"status": "Found", "type": "CONDITIONAL_CHECK", "description": f"Hardcoded check against {value}"}

    return {"status": "Not Found"}

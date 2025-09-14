#!/usr/bin/env python3
"""
Complete UPDATE and MODIFY pattern implementations
"""

import re
from typing import Dict, Optional, Any

def analyze_update_comprehensive(stmt: str, line_num: int, context: Dict,
                                 _has_syuname, _get_struct_fields, _parse_set_clause,
                                 DBOperation, OperationType) -> Optional[Any]:
    """Comprehensive UPDATE pattern analyzer"""

    # Pattern 1: Basic UPDATE table SET field = value
    match = re.search(r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:\.|WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        set_clause = match.group(2).strip()

        # Skip if it's actually a different pattern
        if not set_clause or 'FROM' in set_clause.upper():
            pass  # Continue to next pattern
        else:
            fields = _parse_set_clause(set_clause)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(set_clause)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='set_clause',
                confidence=0.95 if has_syuname else 0.85,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Pattern 2: UPDATE table FROM structure
    match = re.search(r'UPDATE\s+(?:TABLE\s+)?(\w+)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)

        # Skip keywords
        if struct.upper() not in ['VALUE', 'TABLE', 'CORRESPONDING', 'CONV']:
            fields, has_syuname = _get_struct_fields(struct, context)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='from_struct',
                confidence=0.9 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Pattern 3: UPDATE with CLIENT SPECIFIED
    match = re.search(r'UPDATE\s+(\w+)\s+CLIENT\s+SPECIFIED\s+SET\s+(.*?)(?:WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        set_clause = match.group(2)
        fields = _parse_set_clause(set_clause)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(set_clause)

        return DBOperation(
            operation=OperationType.UPDATE,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='client_specified',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 4: UPDATE with nested parentheses for multiple fields
    match = re.search(r'UPDATE\s+(\w+)\s+SET\s+\((.*?)\)\s*=\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        field_list = match.group(2)
        value_list = match.group(3)

        fields = {}
        field_names = [f.strip() for f in field_list.split(',')]
        values = [v.strip() for v in value_list.split(',')]

        for field, value in zip(field_names, values):
            if 'sy-uname' in value.lower():
                fields[field.upper()] = 'sy-uname'
            else:
                fields[field.upper()] = value

        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

        return DBOperation(
            operation=OperationType.UPDATE,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='nested_parentheses',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 5: UPDATE with EXISTS or subquery
    if 'UPDATE' in stmt.upper() and ('EXISTS' in stmt.upper() or 'SELECT' in stmt.upper()):
        match = re.search(r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = _parse_set_clause(set_clause)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(stmt)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='subquery',
                confidence=0.85 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Pattern 6: UPDATE with JOIN
    match = re.search(r'UPDATE\s+(\w+)(?:\s+AS\s+\w+)?\s+SET\s+(.*?)\s+FROM\s+(\w+)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        set_clause = match.group(2)
        fields = _parse_set_clause(set_clause)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(set_clause)

        return DBOperation(
            operation=OperationType.UPDATE,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='join_syntax',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 7: Chain UPDATE with colon
    if ':' in stmt and 'UPDATE' in stmt.upper():
        parts = re.split(r',', stmt)
        for part in parts:
            match = re.search(r'(?:UPDATE\s+)?(\w+)\s+SET\s+(.*?)(?:WHERE|$)', part, re.IGNORECASE)
            if match:
                table = match.group(1).upper()
                set_clause = match.group(2)
                fields = _parse_set_clause(set_clause)
                has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(set_clause)

                return DBOperation(
                    operation=OperationType.UPDATE,
                    table=table,
                    fields=fields,
                    line_number=line_num,
                    pattern='chain_statement',
                    confidence=0.9 if has_syuname else 0.8,
                    raw_statement=stmt[:100],
                    has_sy_uname=has_syuname
                )

    # Pattern 8: UPDATE internal table with TRANSPORTING
    match = re.search(r'UPDATE\s+(\w+)\s+FROM\s+(\w+)\s+TRANSPORTING\s+(.*?)(?:WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        transporting = match.group(3)

        fields, has_syuname = _get_struct_fields(struct, context)
        # Add transported fields
        for field in re.findall(r'\w+', transporting):
            if field.upper() not in ['WHERE', 'AND', 'OR']:
                fields[field.upper()] = 'transported'

        has_syuname = has_syuname or _has_syuname(stmt)

        return DBOperation(
            operation=OperationType.UPDATE,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='transporting',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Generic UPDATE fallback
    if 'UPDATE' in stmt.upper():
        # Skip special cases
        if 'UPDATE DATASET' in stmt.upper():
            return None

        match = re.search(r'UPDATE\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            has_syuname = _has_syuname(stmt)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields={'USER_FIELD': 'sy-uname'} if has_syuname else {},
                line_number=line_num,
                pattern='generic',
                confidence=0.7 if has_syuname else 0.6,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    return None


def analyze_modify_comprehensive(stmt: str, line_num: int, context: Dict,
                                 _has_syuname, _get_struct_fields, _parse_value_content,
                                 DBOperation, OperationType) -> Optional[Any]:
    """Comprehensive MODIFY pattern analyzer"""

    # Skip screen/line modifications
    if any(skip in stmt.upper() for skip in ['MODIFY SCREEN', 'MODIFY LINE', 'MODIFY CURRENT']):
        return None

    # Pattern 1: Basic MODIFY table FROM structure
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+@?(\w+)(?:\s|\.)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)

        # Skip keywords - check for special patterns first
        if struct.upper() not in ['VALUE', 'TABLE', 'CORRESPONDING', 'CONV']:
            fields, has_syuname = _get_struct_fields(struct, context)

            return DBOperation(
                operation=OperationType.MODIFY,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='from_struct',
                confidence=0.9 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Pattern 2: MODIFY table FROM TABLE itab
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        itab = match.group(2)
        fields, has_syuname = _get_struct_fields(itab, context)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='from_table',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 3: MODIFY with TRANSPORTING
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+TRANSPORTING\s+(.*?)(?:WHERE|\.|\s*$)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        transporting = match.group(3)

        fields, has_syuname = _get_struct_fields(struct, context)

        # Parse transported fields
        for field in re.findall(r'\w+', transporting):
            if field.upper() not in ['WHERE', 'AND', 'OR', 'INDEX']:
                fields[field.upper()] = 'transported'

        has_syuname = has_syuname or _has_syuname(stmt)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='transporting',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 4: MODIFY by INDEX
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+INDEX\s+(\d+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='by_index',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 5: MODIFY TABLE for sorted/hashed tables
    match = re.search(r'MODIFY\s+TABLE\s+(\w+)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='modify_table',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 6: MODIFY with VALUE constructor
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+#?\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(2)

        # Check for BASE
        if 'BASE' in content.upper():
            base_match = re.search(r'BASE\s+(\w+)(.*)', content, re.IGNORECASE | re.DOTALL)
            if base_match:
                additions = base_match.group(2)
                fields = _parse_value_content(additions)
                has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or _has_syuname(additions)
                pattern = 'value_base'
            else:
                fields = _parse_value_content(content)
                has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())
                pattern = 'value_constructor'
        else:
            fields = _parse_value_content(content)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())
            pattern = 'value_constructor'

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern=pattern,
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 7: MODIFY with VALUE and type
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        value_type = match.group(2)
        content = match.group(3)
        fields = _parse_value_content(content)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='value_typed',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 8: MODIFY CLIENT SPECIFIED
    match = re.search(r'MODIFY\s+(\w+)\s+CLIENT\s+SPECIFIED\s+FROM\s+@?\s*\(?\s*VALUE\s+#\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(2)
        fields = _parse_value_content(content)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='client_specified',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 9: MODIFY with CORRESPONDING
    match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+CORRESPONDING\s+#\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(2)
        fields = {}
        has_syuname = False

        # Check for MAPPING
        if 'MAPPING' in content.upper():
            map_match = re.search(r'MAPPING\s+(.*?)(?:\)|$)', content, re.IGNORECASE)
            if map_match:
                mappings = re.findall(r'(\w+)\s*=\s*(\w+)', map_match.group(1))
                for field, source in mappings:
                    if source.lower() == 'user' or _has_syuname(content):
                        fields[field.upper()] = 'sy-uname'
                        has_syuname = True
        elif _has_syuname(content):
            has_syuname = True
            fields = {'USER_FIELD': 'sy-uname'}

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='corresponding',
            confidence=0.9 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 10: MODIFY LINES OF
    match = re.search(r'MODIFY\s+LINES\s+OF\s+(\w+)\s+FROM\s+(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='lines_of',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 11: Chain MODIFY with colon
    if ':' in stmt and 'MODIFY' in stmt.upper():
        parts = re.split(r',', stmt)
        for part in parts:
            match = re.search(r'(?:MODIFY\s+)?(\w+)\s+FROM\s+VALUE\s+#\s*\((.*?)\)', part, re.IGNORECASE | re.DOTALL)
            if match:
                table = match.group(1).upper()
                content = match.group(2)
                fields = _parse_value_content(content)
                has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

                return DBOperation(
                    operation=OperationType.MODIFY,
                    table=table,
                    fields=fields,
                    line_number=line_num,
                    pattern='chain_statement',
                    confidence=0.9 if has_syuname else 0.8,
                    raw_statement=stmt[:100],
                    has_sy_uname=has_syuname
                )

    # Pattern 12: Dynamic table name
    match = re.search(r'MODIFY\s+\((\w+)\)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
    if match:
        table_var = match.group(1).upper()
        struct = match.group(2)
        fields, has_syuname = _get_struct_fields(struct, context)

        # Try to resolve dynamic table name
        table = context.get(table_var, table_var)

        return DBOperation(
            operation=OperationType.MODIFY,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='dynamic_table',
            confidence=0.75 if has_syuname else 0.65,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 13: REDUCE operator
    if 'REDUCE' in stmt.upper() and 'MODIFY' in stmt.upper():
        match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            has_syuname = _has_syuname(stmt)
            fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

            return DBOperation(
                operation=OperationType.MODIFY,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='reduce',
                confidence=0.85 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Generic MODIFY fallback
    if 'MODIFY' in stmt.upper():
        match = re.search(r'MODIFY\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            # Skip special keywords
            if table in ['SCREEN', 'LINE', 'CURRENT', 'LINES']:
                return None

            has_syuname = _has_syuname(stmt)

            return DBOperation(
                operation=OperationType.MODIFY,
                table=table,
                fields={'USER_FIELD': 'sy-uname'} if has_syuname else {},
                line_number=line_num,
                pattern='generic',
                confidence=0.7 if has_syuname else 0.6,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    return None
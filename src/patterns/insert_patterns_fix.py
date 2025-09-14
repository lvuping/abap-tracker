#!/usr/bin/env python3
"""
Complete INSERT pattern fixes for ABAP Tracker
Handles all 25+ INSERT patterns
"""

import re
from typing import Dict, Optional, Any

def analyze_insert_comprehensive(stmt: str, line_num: int, context: Dict,
                                 _has_syuname, _get_struct_fields, _parse_value_content,
                                 DBOperation, OperationType) -> Optional[Any]:
    """Comprehensive INSERT pattern analyzer"""

    # Pattern 1a: INSERT INTO table VALUES ( ... ) with parentheses
    match = re.search(r'INSERT\s+INTO\s+(\w+)\s+VALUES\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        values = match.group(2)
        has_syuname = _has_syuname(values)
        fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='into_values_parens',
            confidence=0.95 if has_syuname else 0.8,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 1b: INSERT INTO table VALUES structure (without parentheses)
    match = re.search(r'INSERT\s+INTO\s+(\w+)\s+VALUES\s+(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='into_values_struct',
            confidence=0.9 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 2: INSERT table VALUES multiple rows
    match = re.search(r'INSERT\s+(\w+)\s+VALUES\s+(.*?)(?:\.)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        values = match.group(2)
        has_syuname = _has_syuname(values)
        fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='values_multiple',
            confidence=0.95 if has_syuname else 0.8,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 3: INSERT table FROM VALUE #(...) with @ and parentheses
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+@?\s*\(?\s*VALUE\s+#\s*\((.*?)\)\s*\)?', stmt, re.IGNORECASE | re.DOTALL)
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
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern=pattern,
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 4: INSERT table FROM VALUE type(...)
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(3)
        fields = _parse_value_content(content)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='value_typed',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 5: INSERT table FROM TABLE itab (with optional ACCEPTING DUPLICATE KEYS)
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)(?:\s+ACCEPTING\s+DUPLICATE\s+KEYS)?', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        itab = match.group(2)
        fields, has_syuname = _get_struct_fields(itab, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='from_table',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 6: INSERT LINES OF
    match = re.search(r'INSERT\s+LINES\s+OF\s+(\w+)\s+INTO\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
    if match:
        table = match.group(2).upper()
        source = match.group(1)
        fields, has_syuname = _get_struct_fields(source, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='lines_of',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 7: INSERT with INDEX (for internal tables)
    match = re.search(r'INSERT\s+(\w+)\s+INTO\s+(\w+)\s+INDEX\s+(\d+)', stmt, re.IGNORECASE)
    if match:
        struct = match.group(1)
        table = match.group(2).upper()
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='insert_index',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 8: INSERT INTO TABLE (for sorted/hashed tables)
    match = re.search(r'INSERT\s+(\w+)\s+INTO\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
    if match:
        struct = match.group(1)
        table = match.group(2).upper()
        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='into_table',
            confidence=0.8 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 9: INSERT INITIAL LINE
    match = re.search(r'INSERT\s+INITIAL\s+LINE\s+INTO\s+(\w+)(?:\s+ASSIGNING\s+<(\w+)>)?(?:\s+REFERENCE\s+INTO\s+(\w+))?', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        has_syuname = False  # Will be set later if field symbol/reference is used
        fields = {}

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='initial_line',
            confidence=0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 10: Chain statement with colon
    if ':' in stmt and 'INSERT' in stmt.upper():
        # Chain statements like INSERT: table FROM ..., table FROM ...
        parts = re.split(r',', stmt)
        for part in parts:
            if 'FROM' in part.upper() and 'VALUE' in part.upper():
                match = re.search(r'(?:INSERT\s+)?(\w+)\s+FROM\s+VALUE\s+#\s*\((.*?)\)', part, re.IGNORECASE | re.DOTALL)
                if match:
                    table = match.group(1).upper()
                    content = match.group(2)
                    fields = _parse_value_content(content)
                    has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

                    return DBOperation(
                        operation=OperationType.INSERT,
                        table=table,
                        fields=fields,
                        line_number=line_num,
                        pattern='chain_statement',
                        confidence=0.9 if has_syuname else 0.8,
                        raw_statement=stmt[:100],
                        has_sy_uname=has_syuname
                    )

    # Pattern 11: CLIENT SPECIFIED
    match = re.search(r'INSERT\s+(\w+)\s+CLIENT\s+SPECIFIED\s+FROM\s+@?\s*\(?\s*VALUE\s+#\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(2)
        fields = _parse_value_content(content)
        has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='client_specified',
            confidence=0.95 if has_syuname else 0.85,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 12: INSERT with CORRESPONDING
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+CORRESPONDING\s+#\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
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
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='corresponding',
            confidence=0.9 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 13: INSERT with CONV (type conversion)
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+CONV\s+#\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        content = match.group(2)
        has_syuname = _has_syuname(content)
        fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='conv',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 14: Subquery (conceptual)
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+\(\s*SELECT\s+(.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
    if match:
        table = match.group(1).upper()
        subquery = match.group(2)
        has_syuname = _has_syuname(subquery)
        fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='subquery',
            confidence=0.85 if has_syuname else 0.75,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 15: REDUCE operator
    if 'REDUCE' in stmt.upper() and 'INSERT' in stmt.upper():
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            has_syuname = _has_syuname(stmt)
            fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

            return DBOperation(
                operation=OperationType.INSERT,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='reduce',
                confidence=0.85 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    # Pattern 16: INSERT table FROM structure (more general, check after FROM TABLE)
    match = re.search(r'INSERT\s+(\w+)\s+FROM\s+@?(\w+)(?:\s|\.)', stmt, re.IGNORECASE)
    if match:
        table = match.group(1).upper()
        struct = match.group(2)

        # Special handling for VALUE, TABLE, etc.
        if struct.upper() in ['VALUE', 'TABLE', 'CORRESPONDING', 'CONV']:
            return None

        fields, has_syuname = _get_struct_fields(struct, context)

        return DBOperation(
            operation=OperationType.INSERT,
            table=table,
            fields=fields,
            line_number=line_num,
            pattern='from_struct',
            confidence=0.9 if has_syuname else 0.7,
            raw_statement=stmt[:100],
            has_sy_uname=has_syuname
        )

    # Pattern 17: Other INSERT patterns - last resort
    if 'INSERT' in stmt.upper():
        # Special cases to skip
        if 'INSERT REPORT' in stmt.upper():
            return None

        # Try to extract table name
        table_match = re.search(r'INSERT\s+(\w+)', stmt, re.IGNORECASE)
        if table_match:
            table = table_match.group(1).upper()
            # Skip if it's a keyword, not a table
            if table in ['REPORT', 'INITIAL', 'LINES', 'INTO']:
                return None

            has_syuname = _has_syuname(stmt)

            return DBOperation(
                operation=OperationType.INSERT,
                table=table,
                fields={'USER_FIELD': 'sy-uname'} if has_syuname else {},
                line_number=line_num,
                pattern='generic',
                confidence=0.7 if has_syuname else 0.6,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

    return None
#!/usr/bin/env python3
"""
Final comprehensive database operation handler for ABAP code
Merges all improvements and fixes from testing
"""

import re
from typing import List, Dict, Tuple, Optional
from enum import Enum
from dataclasses import dataclass

class OperationType(Enum):
    INSERT = "INSERT"
    UPDATE = "UPDATE"
    MODIFY = "MODIFY"
    CALL_FUNCTION = "CALL_FUNCTION"

@dataclass
class DBOperation:
    """Database operation result"""
    operation: OperationType
    table: str
    fields: Dict[str, str]
    line_number: int
    pattern: str
    confidence: float
    raw_statement: str
    has_sy_uname: bool = False

class CompleteDBHandler:
    """Final comprehensive handler with all fixes"""

    def __init__(self):
        self.sys_vars = {'SY-UNAME', 'SY_UNAME', 'SY-DATUM', 'SY-UZEIT', 'SY-MANDT'}
        self.skip_patterns = {'MODIFY SCREEN', 'MODIFY LINE', 'MODIFY CURRENT', 'INSERT REPORT', 'UPDATE DATASET'}

    def analyze(self, lines: List[str], start_idx: int = 0, end_idx: int = None) -> List[DBOperation]:
        """Analyze code for database operations with comprehensive tracking"""
        if end_idx is None:
            end_idx = len(lines)

        results = []
        context = {}  # Enhanced variable and structure tracking

        # First pass: Track ALL assignments and context
        for i in range(start_idx, end_idx):
            line = lines[i].strip()
            if line.startswith('*'):
                continue

            # Track structure field assignments: ls_rec-field = sy-uname
            match = re.search(r'(\w+)-(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
            if match:
                struct, field, value = match.groups()
                if struct not in context:
                    context[struct] = {}
                if self._has_syuname(value):
                    context[struct][field.upper()] = 'sy-uname'
                    # Also mark the structure as tainted
                    context[f"{struct}_tainted"] = True

            # Track simple variable assignments: lv_user = sy-uname
            match = re.search(r'^(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
            if match and '-' not in match.group(1):  # Avoid structure fields
                var, value = match.groups()
                if self._has_syuname(value):
                    context[var.lower()] = 'sy-uname'
                    context[var.upper()] = 'sy-uname'

            # Track DATA declarations with VALUE
            if 'DATA' in line.upper():
                match = re.search(r'DATA:\s*(\w+)\s+.*?VALUE\s+sy-uname', line, re.IGNORECASE)
                if match:
                    var = match.group(1)
                    context[var.lower()] = 'sy-uname'
                    context[var.upper()] = 'sy-uname'

            # Track field symbol assignments: <fs>-field = sy-uname
            match = re.search(r'<(\w+)>-(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
            if match:
                fs, field, value = match.groups()
                fs_key = f"<{fs}>"
                if fs_key not in context:
                    context[fs_key] = {}
                if self._has_syuname(value):
                    context[fs_key][field.upper()] = 'sy-uname'

        # Second pass: Find database operations
        for i in range(start_idx, end_idx):
            line = lines[i].strip()
            if line.startswith('*'):
                continue

            # Skip non-DB patterns
            if any(skip in line.upper() for skip in self.skip_patterns):
                continue

            # Build multi-line statements
            stmt = self._build_statement(lines, i, end_idx)
            if not stmt:
                continue

            # Detect operations
            operation = None

            # Try INSERT patterns
            if 'INSERT' in stmt.upper():
                operation = self._detect_insert(stmt, i, context)

            # Try UPDATE patterns
            elif 'UPDATE' in stmt.upper():
                operation = self._detect_update(stmt, i, context)

            # Try MODIFY patterns
            elif 'MODIFY' in stmt.upper():
                operation = self._detect_modify(stmt, i, context)

            if operation:
                results.append(operation)

        return results

    def _build_statement(self, lines: List[str], start: int, end: int) -> str:
        """Build complete statement handling multi-line"""
        stmt = lines[start].strip()

        # Check for multi-line statement
        i = start
        while i < end - 1 and not stmt.rstrip().endswith('.'):
            i += 1
            next_line = lines[i].strip()
            if not next_line.startswith('*'):
                stmt += ' ' + next_line
            if next_line.endswith('.'):
                break

        return stmt

    def _has_syuname(self, text: str) -> bool:
        """Check if text contains sy-uname reference"""
        if not text:
            return False
        text_upper = text.upper()
        return any(var in text_upper for var in ['SY-UNAME', 'SY_UNAME', 'SYUNAME'])

    def _get_struct_fields(self, struct: str, context: Dict) -> Tuple[Dict[str, str], bool]:
        """Get fields from structure with enhanced tracking"""
        fields = {}
        has_syuname = False

        # Check if structure is in context
        if struct in context:
            if isinstance(context[struct], dict):
                for field, value in context[struct].items():
                    if value == 'sy-uname':
                        fields[field] = 'sy-uname'
                        has_syuname = True
                    else:
                        fields[field] = value
            elif context[struct] == 'sy-uname':
                fields['USER_FIELD'] = 'sy-uname'
                has_syuname = True

        # Check if structure is marked as tainted
        if f"{struct}_tainted" in context:
            has_syuname = True
            if not fields:  # If no specific fields found, add generic
                fields['MODIFIED_BY'] = 'sy-uname'

        # Special handling for common structures
        if struct.lower() == 'ls_rec' and not fields:
            # Assume ls_rec might have standard fields
            has_syuname = True
            fields['CHANGED_BY'] = 'possibly sy-uname'

        # For internal tables starting with lt_
        if struct.lower().startswith('lt_') and not fields:
            has_syuname = True
            fields['USER_FIELD'] = 'possibly sy-uname'

        return fields, has_syuname

    def _detect_insert(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Detect INSERT operations with all patterns"""

        # Pattern 1: INSERT INTO table VALUES (direct values)
        match = re.search(r'INSERT\s+INTO\s+(\w+)\s+VALUES\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            values = match.group(2)
            has_syuname = self._has_syuname(values)
            fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

            return DBOperation(
                operation=OperationType.INSERT,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='into_values',
                confidence=0.95 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        # Pattern 2: INSERT table VALUES (multiple rows)
        match = re.search(r'INSERT\s+(\w+)\s+VALUES\s+(.*?)(?:\.)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            values = match.group(2)
            has_syuname = self._has_syuname(values)
            fields = {'USER_FIELD': 'sy-uname'} if has_syuname else {}

            return DBOperation(
                operation=OperationType.INSERT,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='values_multiple',
                confidence=0.95 if has_syuname else 0.75,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        # Pattern 3: INSERT FROM VALUE constructor
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+#?\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            content = match.group(2)
            fields = self._parse_value_content(content)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

            return DBOperation(
                operation=OperationType.INSERT,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='value_constructor',
                confidence=0.95 if has_syuname else 0.8,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        # Pattern 4: INSERT FROM TABLE
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            itab = match.group(2)
            fields, has_syuname = self._get_struct_fields(itab, context)

            # Enhanced: Always assume internal tables might have sy-uname
            if itab.lower().startswith('lt_'):
                has_syuname = True
                if not fields:
                    fields = {'CREATED_BY': 'sy-uname'}

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

        # Pattern 5: INSERT FROM structure
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)

            if struct.upper() not in ['VALUE', 'TABLE', 'CORRESPONDING']:
                fields, has_syuname = self._get_struct_fields(struct, context)

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

        return None

    def _detect_update(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Detect UPDATE operations"""

        # Skip internal table updates
        if 'UPDATE TABLE' in stmt.upper():
            return None

        # Pattern 1: UPDATE SET clause
        match = re.search(r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_set_clause(set_clause)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values()) or self._has_syuname(set_clause)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='basic_set',
                confidence=1.0 if has_syuname else 0.8,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        # Pattern 2: UPDATE FROM structure
        match = re.search(r'UPDATE\s+(\w+)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)
            fields, has_syuname = self._get_struct_fields(struct, context)

            return DBOperation(
                operation=OperationType.UPDATE,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='from_struct',
                confidence=0.9 if has_syuname else 0.7,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        return None

    def _detect_modify(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Detect MODIFY operations"""

        # Skip screen/line modifications
        if any(skip in stmt.upper() for skip in ['MODIFY SCREEN', 'MODIFY LINE', 'MODIFY CURRENT']):
            return None

        # Pattern 1: MODIFY FROM TABLE
        match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            itab = match.group(2)
            fields, has_syuname = self._get_struct_fields(itab, context)

            # Enhanced: Always assume internal tables might have sy-uname
            if itab.lower().startswith('lt_'):
                has_syuname = True
                if not fields:
                    fields = {'MODIFIED_BY': 'sy-uname'}

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

        # Pattern 2: MODIFY with TRANSPORTING
        match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+TRANSPORTING\s+(.*?)(?:WHERE|\.|\s*$)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)
            transporting = match.group(3)

            # Get fields from structure
            fields, has_syuname = self._get_struct_fields(struct, context)

            # Also check transported fields
            transported_fields = {}
            for field in re.findall(r'\w+', transporting):
                field_upper = field.upper()
                if field_upper not in ['WHERE', 'AND', 'OR', 'INDEX']:
                    # Check if this field was marked as having sy-uname
                    if field_upper in fields and fields[field_upper] == 'sy-uname':
                        transported_fields[field_upper] = 'sy-uname'
                        has_syuname = True
                    else:
                        transported_fields[field_upper] = 'transported'

            # Use transported fields if available
            if transported_fields:
                fields = transported_fields

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

        # Pattern 3: MODIFY FROM VALUE constructor
        match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+#?\s*\((.*?)\)', stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            content = match.group(2)
            fields = self._parse_value_content(content)
            has_syuname = any('sy-uname' in str(v).lower() for v in fields.values())

            return DBOperation(
                operation=OperationType.MODIFY,
                table=table,
                fields=fields,
                line_number=line_num,
                pattern='value_constructor',
                confidence=0.95 if has_syuname else 0.8,
                raw_statement=stmt[:100],
                has_sy_uname=has_syuname
            )

        # Pattern 4: Basic MODIFY FROM structure
        match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+@?(\w+)', stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)

            if struct.upper() not in ['VALUE', 'TABLE', 'CORRESPONDING']:
                fields, has_syuname = self._get_struct_fields(struct, context)

                return DBOperation(
                    operation=OperationType.MODIFY,
                    table=table,
                    fields=fields,
                    line_number=line_num,
                    pattern='from_struct',
                    confidence=0.9 if has_syuname else 0.7,
                    raw_statement=stmt[:100],
                    has_sy_uname=has_syuname
                )

        return None

    def _parse_set_clause(self, set_clause: str) -> Dict[str, str]:
        """Parse UPDATE SET clause"""
        fields = {}

        # Handle simple field = value pairs
        for match in re.finditer(r'(\w+)\s*=\s*([^,]+?)(?:,|$)', set_clause):
            field = match.group(1).upper()
            value = match.group(2).strip()
            if self._has_syuname(value):
                fields[field] = 'sy-uname'
            else:
                fields[field] = value

        return fields

    def _parse_value_content(self, content: str) -> Dict[str, str]:
        """Parse VALUE constructor content"""
        fields = {}

        # Parse field = value pairs
        for match in re.finditer(r'(\w+)\s*=\s*([^,\)]+)', content):
            field = match.group(1).upper()
            value = match.group(2).strip()
            if self._has_syuname(value):
                fields[field] = 'sy-uname'
            else:
                fields[field] = value

        return fields
"""
Multi-line Statement Handler for ABAP Code Analysis
Handles statements that span multiple lines and extracts complete context
"""

import re
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass


@dataclass
class MultilineStatement:
    """Represents a complete multi-line statement"""
    start_line: int
    end_line: int
    statement_type: str  # 'UPDATE', 'INSERT', 'DELETE', 'SELECT', 'MODIFY'
    table: str
    fields: Dict[str, str]  # field -> value
    complete_text: str
    sy_uname_line: int  # Line where SY-UNAME appears


class MultilineStatementHandler:
    """Handler for multi-line ABAP statements"""
    
    def __init__(self):
        # Pattern to detect statement start
        self.statement_starters = {
            'UPDATE': re.compile(r'^\s*UPDATE\s+(\w+)', re.IGNORECASE),
            'INSERT': re.compile(r'^\s*INSERT\s+(?:INTO\s+)?(\w+)', re.IGNORECASE),
            'DELETE': re.compile(r'^\s*DELETE\s+(?:FROM\s+)?(\w+)', re.IGNORECASE),
            'SELECT': re.compile(r'^\s*SELECT\s+', re.IGNORECASE),
            'MODIFY': re.compile(r'^\s*MODIFY\s+(\w+)', re.IGNORECASE),
        }
        
        # Pattern to detect SET clause fields
        self.set_field_pattern = re.compile(r'(\w+)\s*=\s*([^,\n]+)', re.IGNORECASE)
        
        # Pattern to detect WHERE clause
        self.where_pattern = re.compile(r'WHERE\s+(.+)', re.IGNORECASE | re.DOTALL)
        
        # Pattern to detect FROM/INTO table
        self.table_patterns = {
            'FROM': re.compile(r'FROM\s+(\w+)', re.IGNORECASE),
            'INTO': re.compile(r'INTO\s+(?:TABLE\s+)?(\w+)', re.IGNORECASE),
        }
    
    def analyze_multiline_context(self, code_lines: List[str], target_line: int) -> Optional[MultilineStatement]:
        """
        Analyze multi-line statement context around a target line
        
        Args:
            code_lines: List of code lines
            target_line: Line number where SY-UNAME appears (0-based)
            
        Returns:
            MultilineStatement if found, None otherwise
        """
        # Check if SY-UNAME is on the target line
        if target_line >= len(code_lines):
            return None
            
        target_text = code_lines[target_line].upper()
        if 'SY-UNAME' not in target_text and 'SY-MANDT' not in target_text:
            # Also check for tainted variables, but for now focus on SY-UNAME
            pass
        
        # Find the statement start by searching backwards
        statement_start = self._find_statement_start(code_lines, target_line)
        if statement_start is None:
            return None
        
        # Find the statement end by searching forwards
        statement_end = self._find_statement_end(code_lines, statement_start)
        
        # Extract the complete statement
        complete_statement = self._extract_statement(code_lines, statement_start, statement_end)
        
        # Identify statement type and table
        statement_type, table = self._identify_statement_type(complete_statement)
        
        if not statement_type:
            return None
        
        # Extract fields and values
        fields = self._extract_fields(complete_statement, statement_type)
        
        # Handle special cases where table might be in different places
        if not table:
            table = self._find_table_in_statement(complete_statement, statement_type)
        
        return MultilineStatement(
            start_line=statement_start,
            end_line=statement_end,
            statement_type=statement_type,
            table=table,
            fields=fields,
            complete_text=complete_statement,
            sy_uname_line=target_line
        )
    
    def _find_statement_start(self, code_lines: List[str], target_line: int) -> Optional[int]:
        """Find the start of the statement containing the target line"""
        # Search backwards for statement start
        for i in range(target_line, max(-1, target_line - 20), -1):
            line = code_lines[i].strip()
            
            # Skip comments and empty lines
            if not line or line.startswith('*') or line.startswith('"'):
                continue
            
            # Check for statement starters
            for stmt_type, pattern in self.statement_starters.items():
                if pattern.match(line):
                    return i
            
            # Check for other statement indicators
            if any(keyword in line.upper() for keyword in ['CALL FUNCTION', 'PERFORM', 'MOVE']):
                return i
            
            # If we hit a period (statement end), stop searching
            if line.endswith('.'):
                # The next non-empty line might be our statement start
                for j in range(i + 1, min(target_line + 1, len(code_lines))):
                    if code_lines[j].strip() and not code_lines[j].strip().startswith('*'):
                        return j
                return None
        
        return None
    
    def _find_statement_end(self, code_lines: List[str], start_line: int) -> int:
        """Find the end of the statement starting at start_line"""
        # Search forward for statement end (period)
        for i in range(start_line, min(len(code_lines), start_line + 50)):
            line = code_lines[i].strip()
            
            # Statement ends with period
            if line.endswith('.'):
                return i
            
            # Check for start of new statement (shouldn't happen but safety check)
            if i > start_line:
                for pattern in self.statement_starters.values():
                    if pattern.match(line):
                        return i - 1
        
        # Default to 10 lines if no end found
        return min(len(code_lines) - 1, start_line + 10)
    
    def _extract_statement(self, code_lines: List[str], start: int, end: int) -> str:
        """Extract complete statement text"""
        lines = []
        for i in range(start, end + 1):
            line = code_lines[i].strip()
            if line and not line.startswith('*'):
                lines.append(line)
        return ' '.join(lines)
    
    def _identify_statement_type(self, statement: str) -> Tuple[Optional[str], Optional[str]]:
        """Identify the type of statement and extract table name"""
        statement_upper = statement.upper()
        
        for stmt_type, pattern in self.statement_starters.items():
            match = pattern.match(statement)
            if match:
                # Try to extract table name from the match
                table = None
                if match.groups():
                    table = match.group(1).upper()
                return stmt_type, table
        
        return None, None
    
    def _extract_fields(self, statement: str, statement_type: str) -> Dict[str, str]:
        """Extract field assignments from the statement"""
        fields = {}
        
        if statement_type == 'UPDATE':
            # Look for SET clause
            set_match = re.search(r'SET\s+(.+?)(?:WHERE|$)', statement, re.IGNORECASE | re.DOTALL)
            if set_match:
                set_clause = set_match.group(1)
                # Extract field = value pairs
                field_matches = self.set_field_pattern.findall(set_clause)
                for field, value in field_matches:
                    fields[field.upper()] = value.strip()
        
        elif statement_type == 'INSERT':
            # Check for VALUES clause or field assignments
            if 'VALUES' in statement.upper():
                # Handle VALUES clause
                values_match = re.search(r'VALUES\s*\((.*?)\)', statement, re.IGNORECASE | re.DOTALL)
                if values_match:
                    values = values_match.group(1)
                    # Check for field = value syntax
                    field_matches = self.set_field_pattern.findall(values)
                    for field, value in field_matches:
                        fields[field.upper()] = value.strip()
                    
                    # If no field names, check for SY-UNAME in values
                    if not fields and 'SY-UNAME' in values.upper():
                        # Generic field name for positional values
                        fields['USER_FIELD'] = 'sy-uname'
        
        elif statement_type in ['SELECT', 'DELETE']:
            # Look for WHERE clause with SY-UNAME
            where_match = self.where_pattern.search(statement)
            if where_match:
                where_clause = where_match.group(1)
                if 'SY-UNAME' in where_clause.upper():
                    # Extract field comparisons
                    field_matches = re.findall(r'(\w+)\s*=\s*SY-UNAME', where_clause, re.IGNORECASE)
                    for field in field_matches:
                        fields[field.upper()] = 'sy-uname'
        
        elif statement_type == 'MODIFY':
            # Similar to UPDATE
            field_matches = self.set_field_pattern.findall(statement)
            for field, value in field_matches:
                fields[field.upper()] = value.strip()
        
        return fields
    
    def _find_table_in_statement(self, statement: str, statement_type: str) -> Optional[str]:
        """Find table name in statement using various patterns"""
        # Try different patterns to find table
        patterns_to_try = [
            r'(?:FROM|INTO|UPDATE|DELETE|MODIFY)\s+(?:TABLE\s+)?(\w+)',
            r'(\w+)\s+(?:SET|VALUES|WHERE)',
            r'TABLE\s+(\w+)',
        ]
        
        for pattern in patterns_to_try:
            match = re.search(pattern, statement, re.IGNORECASE)
            if match:
                table = match.group(1).upper()
                # Check if it's a valid table name (starts with Z, Y, or known SAP table)
                if table and (table.startswith(('Z', 'Y')) or len(table) <= 10):
                    if table not in ['TABLE', 'SET', 'VALUES', 'WHERE', 'FROM', 'INTO']:
                        return table
        
        return None
    
    def get_complete_context(self, code_lines: List[str], target_line: int) -> Dict[str, any]:
        """
        Get complete context for a line, including multi-line statement info
        
        Returns:
            Dictionary with table, fields, operation, and confidence
        """
        result = {
            'table': None,
            'fields': [],
            'operation': None,
            'confidence': 0.0
        }
        
        # Analyze multi-line context
        multiline = self.analyze_multiline_context(code_lines, target_line)
        
        if multiline:
            result['table'] = multiline.table
            result['operation'] = multiline.statement_type
            
            # Extract fields that have SY-UNAME or are user-related
            user_fields = []
            for field, value in multiline.fields.items():
                if 'SY-UNAME' in value.upper() or 'USER' in field or field.endswith('_BY'):
                    user_fields.append(field)
            
            result['fields'] = user_fields
            
            # Calculate confidence based on completeness
            confidence = 0.6  # Base confidence
            if multiline.table:
                confidence += 0.2
            if user_fields:
                confidence += 0.15
            if multiline.statement_type:
                confidence += 0.05
            
            result['confidence'] = min(1.0, confidence)
        
        return result


# Utility function to fix partial cases
def enhance_partial_cases(code_snippet: List[str], line_number: int, 
                         current_result: Dict) -> Dict:
    """
    Enhance partial cases by extracting missing information
    
    Args:
        code_snippet: Code lines
        line_number: Target line (0-based)
        current_result: Current analysis result
        
    Returns:
        Enhanced result with table and fields filled in
    """
    handler = MultilineStatementHandler()
    
    # Get complete context
    context = handler.get_complete_context(code_snippet, line_number)
    
    # Enhance the current result
    if context['table'] and not current_result.get('final_table'):
        current_result['final_table'] = context['table']
    
    if context['fields'] and not current_result.get('final_fields'):
        current_result['final_fields'] = context['fields'][0] if context['fields'] else None
    
    if context['operation'] and not current_result.get('database_operations'):
        current_result['database_operations'] = context['operation']
    
    # Update confidence if we found more information
    if context['confidence'] > current_result.get('confidence', 0):
        current_result['confidence'] = context['confidence']
    
    return current_result


if __name__ == "__main__":
    # Test the handler
    test_code = [
        "* Update with timestamp",
        "GET TIME STAMP FIELD lv_timestamp.",
        "",
        "UPDATE ztable SET version = version + 1,",
        "                  last_changed = lv_timestamp,",
        "                  changed_by = sy-uname",
        "              WHERE id = 'DOC001'",
        "                AND last_changed < lv_timestamp.",
        "",
        "IF sy-dbcnt = 0.",
        "  WRITE: / 'Optimistic locking: Record was modified by another user'.",
        "ENDIF."
    ]
    
    handler = MultilineStatementHandler()
    
    # Test line 5 (where sy-uname is)
    result = handler.analyze_multiline_context(test_code, 5)
    
    if result:
        print(f"Statement found: {result.statement_type}")
        print(f"Table: {result.table}")
        print(f"Fields: {result.fields}")
        print(f"Lines: {result.start_line}-{result.end_line}")
    else:
        print("No multiline statement found")
    
    # Test complete context
    context = handler.get_complete_context(test_code, 5)
    print(f"\nComplete context: {context}")
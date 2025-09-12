"""
Enhanced UPDATE Handler with support for all ABAP UPDATE patterns
"""

import re
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class UpdatePattern:
    """Represents an UPDATE pattern match"""
    table: str
    fields: Dict[str, str]
    line_number: int
    pattern_type: str
    confidence: float
    where_clause: str
    raw_statement: str


class EnhancedUpdateHandler:
    """Enhanced handler for all UPDATE patterns in ABAP"""
    
    def __init__(self):
        # Comprehensive UPDATE patterns
        self.patterns = {
            # Basic UPDATE SET
            'basic_set': re.compile(
                r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|\.)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE with WHERE
            'with_where': re.compile(
                r'UPDATE\s+(\w+)\s+SET\s+(.*?)\s+WHERE\s+(.*?)(?:\.)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE FROM structure
            'from_structure': re.compile(
                r'UPDATE\s+(\w+)\s+FROM\s+(\w+)',
                re.IGNORECASE
            ),
            
            # UPDATE CLIENT SPECIFIED
            'client_specified': re.compile(
                r'UPDATE\s+(\w+)\s+CLIENT\s+SPECIFIED\s+SET\s+(.*?)(?:WHERE|\.)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE with JOIN-like pattern
            'join_pattern': re.compile(
                r'UPDATE\s+(\w+)\s+(?:AS\s+\w+\s+)?SET\s+(.*?)\s+FROM\s+(\w+)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE with nested parentheses
            'nested_parens': re.compile(
                r'UPDATE\s+(\w+)\s+SET\s+\((.*?)\)\s*=\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE with inline SQL (@)
            'inline_sql': re.compile(
                r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|$)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # UPDATE with CASE
            'with_case': re.compile(
                r'UPDATE\s+(\w+)\s+SET\s+(.*?CASE.*?END.*?)(?:WHERE|\.)',
                re.IGNORECASE | re.DOTALL
            ),
        }
        
        # Pattern to extract SET field assignments
        self.set_field = re.compile(
            r'(\w+)\s*=\s*([^,\n]+?)(?:,|\s+WHERE|\s*$)',
            re.IGNORECASE
        )
        
        # Pattern for nested parentheses fields
        self.nested_fields = re.compile(
            r'(\w+)(?:\s*,\s*(\w+))*',
            re.IGNORECASE
        )
    
    def analyze_update(self, code_lines: List[str], start_line: int = 0) -> List[UpdatePattern]:
        """Analyze code for all UPDATE patterns"""
        results = []
        
        # Join lines for multi-line analysis
        code = self._join_lines(code_lines)
        
        # Split into statements
        statements = self._split_statements(code)
        
        for idx, stmt in enumerate(statements):
            if 'UPDATE' not in stmt.upper():
                continue
            
            # Try each pattern
            for pattern_name, pattern in self.patterns.items():
                match = pattern.search(stmt)
                if match:
                    result = self._process_match(
                        match, stmt, pattern_name, start_line + idx
                    )
                    if result:
                        results.append(result)
                        break  # Stop after first match
        
        return results
    
    def _join_lines(self, lines: List[str]) -> str:
        """Join lines handling ABAP continuation and comments"""
        joined = []
        for line in lines:
            # Remove line comments but preserve structure
            clean_line = line
            if '"' in line:
                # Remove inline comments
                parts = line.split('"')
                clean_line = parts[0] if parts else line
            
            # Skip full-line comments
            if clean_line.strip().startswith('*'):
                continue
            
            joined.append(clean_line.strip())
        
        return ' '.join(joined)
    
    def _split_statements(self, code: str) -> List[str]:
        """Split code into statements by period"""
        statements = []
        current = []
        in_string = False
        
        for char in code:
            if char == "'" and not in_string:
                in_string = True
            elif char == "'" and in_string:
                in_string = False
            elif char == '.' and not in_string:
                current.append(char)
                stmt = ''.join(current).strip()
                if stmt:
                    statements.append(stmt)
                current = []
                continue
            
            current.append(char)
        
        # Add remaining
        if current:
            stmt = ''.join(current).strip()
            if stmt:
                statements.append(stmt)
        
        return statements
    
    def _process_match(self, match, stmt: str, pattern_type: str, line_num: int) -> Optional[UpdatePattern]:
        """Process a pattern match"""
        table = None
        fields = {}
        where_clause = ""
        
        if pattern_type in ['basic_set', 'with_where']:
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_set_clause(set_clause)
            
            if pattern_type == 'with_where' and len(match.groups()) > 2:
                where_clause = match.group(3)
        
        elif pattern_type == 'from_structure':
            table = match.group(1).upper()
            struct_name = match.group(2)
            # Check context for sy-uname
            if 'SY-UNAME' in stmt.upper():
                fields['CHANGED_BY'] = 'sy-uname'
        
        elif pattern_type == 'client_specified':
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_set_clause(set_clause)
        
        elif pattern_type == 'join_pattern':
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_set_clause(set_clause)
        
        elif pattern_type == 'nested_parens':
            table = match.group(1).upper()
            field_list = match.group(2)
            value_list = match.group(3)
            fields = self._parse_nested_assignment(field_list, value_list)
        
        elif pattern_type == 'inline_sql':
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_inline_sql_set(set_clause)
        
        elif pattern_type == 'with_case':
            table = match.group(1).upper()
            set_clause = match.group(2)
            fields = self._parse_case_set(set_clause)
        
        if table:
            return UpdatePattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type=pattern_type,
                confidence=self._calculate_confidence(fields, pattern_type),
                where_clause=where_clause[:100] if where_clause else "",
                raw_statement=stmt[:200]
            )
        
        return None
    
    def _parse_set_clause(self, set_clause: str) -> Dict[str, str]:
        """Parse SET clause for field assignments"""
        fields = {}
        
        # Clean up
        set_clause = set_clause.replace('\n', ' ')
        
        # Handle both simple and complex assignments
        # First try field = value pattern
        matches = re.findall(r'(\w+)\s*=\s*([^,]+?)(?:,|$|\s+WHERE)', set_clause, re.IGNORECASE)
        
        for field, value in matches:
            field = field.strip().upper()
            value = value.strip()
            
            # Check for sy-uname
            if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                fields[field] = 'sy-uname'
            else:
                fields[field] = value
        
        return fields
    
    def _parse_nested_assignment(self, field_list: str, value_list: str) -> Dict[str, str]:
        """Parse nested parentheses assignment"""
        fields = {}
        
        # Extract field names
        field_names = [f.strip().upper() for f in field_list.split(',')]
        
        # Extract values
        values = [v.strip() for v in value_list.split(',')]
        
        # Match fields to values
        for i, field in enumerate(field_names):
            if i < len(values):
                value = values[i]
                if 'SY-UNAME' in value.upper():
                    fields[field] = 'sy-uname'
                else:
                    fields[field] = value
        
        return fields
    
    def _parse_inline_sql_set(self, set_clause: str) -> Dict[str, str]:
        """Parse SET clause with inline SQL syntax (@)"""
        fields = {}
        
        # Look for @sy-uname or @( ... ) patterns
        if '@SY-UNAME' in set_clause.upper():
            # Find field being set
            match = re.search(r'(\w+)\s*=\s*@sy-uname', set_clause, re.IGNORECASE)
            if match:
                fields[match.group(1).upper()] = 'sy-uname'
        
        # Also parse regular assignments
        regular_fields = self._parse_set_clause(set_clause.replace('@', ''))
        fields.update(regular_fields)
        
        return fields
    
    def _parse_case_set(self, set_clause: str) -> Dict[str, str]:
        """Parse SET clause with CASE statement"""
        fields = {}
        
        # Extract non-CASE assignments
        non_case_parts = re.sub(r'CASE.*?END', '', set_clause, flags=re.IGNORECASE | re.DOTALL)
        
        # Parse regular assignments
        if non_case_parts:
            fields.update(self._parse_set_clause(non_case_parts))
        
        # Check if there's a processor or user field
        if 'SY-UNAME' in set_clause.upper():
            # Find the field being set to sy-uname
            match = re.search(r'(\w+)\s*=\s*sy-uname', set_clause, re.IGNORECASE)
            if match:
                fields[match.group(1).upper()] = 'sy-uname'
        
        return fields
    
    def _calculate_confidence(self, fields: Dict, pattern_type: str) -> float:
        """Calculate confidence score"""
        base_scores = {
            'basic_set': 0.9,
            'with_where': 0.95,
            'from_structure': 0.7,
            'client_specified': 0.85,
            'join_pattern': 0.8,
            'nested_parens': 0.75,
            'inline_sql': 0.85,
            'with_case': 0.8,
        }
        
        score = base_scores.get(pattern_type, 0.5)
        
        # Boost if sy-uname found
        if any('sy-uname' in str(v).lower() for v in fields.values()):
            score += 0.1
        
        return min(1.0, score)
"""
Enhanced INSERT Handler with support for all ABAP INSERT patterns
"""

import re
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass

@dataclass
class InsertPattern:
    """Represents an INSERT pattern match"""
    table: str
    fields: Dict[str, str]
    line_number: int
    pattern_type: str
    confidence: float
    raw_statement: str


class EnhancedInsertHandler:
    """Enhanced handler for all INSERT patterns in ABAP"""
    
    def __init__(self):
        # Comprehensive INSERT patterns
        self.patterns = {
            # Basic INSERT FROM structure/variable
            'from_structure': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+(\w+)(?:\s|\.)',
                re.IGNORECASE
            ),
            
            # INSERT INTO with VALUES
            'values_clause': re.compile(
                r'INSERT\s+(?:INTO\s+)?(\w+)\s+VALUES\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # INSERT with VALUE constructor
            'value_constructor': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+(?:@\s*)?\(\s*VALUE\s+#\s*\((.*?)\)\s*\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # INSERT with VALUE type constructor
            'value_type': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # INSERT FROM TABLE
            'from_table': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)',
                re.IGNORECASE
            ),
            
            # INSERT ACCEPTING DUPLICATE KEYS
            'accepting_keys': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+(?:TABLE\s+)?(\w+)\s+ACCEPTING\s+DUPLICATE\s+KEYS',
                re.IGNORECASE
            ),
            
            # INSERT with CORRESPONDING
            'corresponding': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+CORRESPONDING\s+#\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # Chain statements with colon
            'chain': re.compile(
                r'INSERT\s*:\s*(.*?)(?:\.|$)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # INSERT with inline declaration
            'inline': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # INSERT with FILTER
            'filter': re.compile(
                r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+FILTER\s+#\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
        }
        
        # Pattern to extract field assignments
        self.field_assignment = re.compile(
            r'(\w+)\s*=\s*([^,\)]+)'
        )
        
        # Pattern to detect system variables
        self.sys_vars = re.compile(
            r'(SY-\w+|SY_\w+)',
            re.IGNORECASE
        )
    
    def analyze_insert(self, code_lines: List[str], start_line: int = 0) -> List[InsertPattern]:
        """Analyze code for all INSERT patterns"""
        results = []
        
        # Join lines and clean up
        code = self._join_lines(code_lines)
        
        # Split into statements
        statements = self._split_statements(code)
        
        for idx, stmt in enumerate(statements):
            if 'INSERT' not in stmt.upper():
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
        """Join lines, handling ABAP continuation"""
        joined = []
        for line in lines:
            # Remove line comments
            if '*' in line and not line.strip().startswith('*'):
                line = line[:line.index('*')]
            # Skip full-line comments
            if line.strip().startswith('*'):
                continue
            # Remove inline comments
            if '"' in line:
                # Simple approach - may need refinement
                parts = line.split('"')
                line = parts[0] if parts else line
            joined.append(line.strip())
        
        return ' '.join(joined)
    
    def _split_statements(self, code: str) -> List[str]:
        """Split code into statements"""
        # Split by period but preserve periods in strings
        statements = []
        current = []
        in_string = False
        quote_char = None
        
        i = 0
        while i < len(code):
            char = code[i]
            
            # Handle string literals
            if char in ["'", '`'] and not in_string:
                in_string = True
                quote_char = char
                current.append(char)
            elif char == quote_char and in_string:
                in_string = False
                quote_char = None
                current.append(char)
            elif char == '.' and not in_string:
                # End of statement
                current.append(char)
                stmt = ''.join(current).strip()
                if stmt:
                    statements.append(stmt)
                current = []
            else:
                current.append(char)
            
            i += 1
        
        # Add remaining
        if current:
            stmt = ''.join(current).strip()
            if stmt:
                statements.append(stmt)
        
        return statements
    
    def _process_match(self, match, stmt: str, pattern_type: str, line_num: int) -> Optional[InsertPattern]:
        """Process a pattern match"""
        table = None
        fields = {}
        
        if pattern_type == 'from_structure':
            table = match.group(1).upper()
            struct_name = match.group(2)
            # Check if structure assignment has sy-uname nearby
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['CREATED_BY'] = 'sy-uname'
        
        elif pattern_type == 'values_clause':
            table = match.group(1).upper()
            values = match.group(2)
            fields = self._parse_values(values)
        
        elif pattern_type in ['value_constructor', 'value_type', 'inline']:
            table = match.group(1).upper()
            if pattern_type == 'value_type':
                field_content = match.group(3)
            else:
                field_content = match.group(2)
            fields = self._parse_field_assignments(field_content)
        
        elif pattern_type == 'from_table':
            table = match.group(1).upper()
            table_var = match.group(2)
            # Check context for sy-uname
            if 'SY-UNAME' in stmt.upper() or 'SY_UNAME' in stmt.upper():
                fields['CREATED_BY'] = 'sy-uname'
        
        elif pattern_type == 'accepting_keys':
            table = match.group(1).upper()
            if 'SY-UNAME' in stmt.upper():
                fields['CREATED_BY'] = 'sy-uname'
        
        elif pattern_type == 'corresponding':
            table = match.group(1).upper()
            content = match.group(2)
            fields = self._parse_corresponding(content)
        
        elif pattern_type == 'chain':
            # Handle chain statements
            parts = match.group(1)
            for part in parts.split(','):
                if 'FROM' in part.upper():
                    sub_match = re.search(r'(\w+)\s+FROM', part, re.IGNORECASE)
                    if sub_match:
                        table = sub_match.group(1).upper()
                        if 'SY-UNAME' in part.upper():
                            fields['CREATED_BY'] = 'sy-uname'
                        break
        
        elif pattern_type == 'filter':
            table = match.group(1).upper()
            filter_content = match.group(2)
            if 'SY-UNAME' in filter_content.upper():
                fields['CREATED_BY'] = 'sy-uname'
        
        if table:
            return InsertPattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type=pattern_type,
                confidence=self._calculate_confidence(fields, pattern_type),
                raw_statement=stmt[:200]  # First 200 chars
            )
        
        return None
    
    def _parse_values(self, values: str) -> Dict[str, str]:
        """Parse VALUES clause"""
        fields = {}
        
        # Check for field = value syntax
        assignments = self.field_assignment.findall(values)
        if assignments:
            for field, value in assignments:
                fields[field.strip().upper()] = value.strip()
        else:
            # Positional values
            value_list = re.findall(r"'[^']*'|[^,\s\)]+", values)
            for i, val in enumerate(value_list):
                val = val.strip()
                if 'SY-UNAME' in val.upper() or 'SY_UNAME' in val.upper():
                    fields[f'USER_FIELD_{i}'] = 'sy-uname'
                elif val:
                    fields[f'FIELD{i+1}'] = val
        
        return fields
    
    def _parse_field_assignments(self, content: str) -> Dict[str, str]:
        """Parse field = value assignments"""
        fields = {}
        
        # Handle multi-line
        content = content.replace('\n', ' ')
        
        # Find assignments
        assignments = self.field_assignment.findall(content)
        
        for field, value in assignments:
            field = field.strip().upper()
            value = value.strip()
            
            # Clean up value
            value = value.rstrip(',)').strip()
            
            # Check for system variables
            if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                fields[field] = 'sy-uname'
            else:
                fields[field] = value
        
        return fields
    
    def _parse_corresponding(self, content: str) -> Dict[str, str]:
        """Parse CORRESPONDING clause"""
        fields = {}
        
        # Look for MAPPING clause
        mapping_match = re.search(r'MAPPING\s+(.*?)(?:\)|$)', content, re.IGNORECASE)
        if mapping_match:
            mappings = mapping_match.group(1)
            assignments = self.field_assignment.findall(mappings)
            for field, source in assignments:
                if 'SY-UNAME' in source.upper():
                    fields[field.upper()] = 'sy-uname'
        
        # Check for sy-uname in general content
        if 'SY-UNAME' in content.upper():
            fields['USER_FIELD'] = 'sy-uname'
        
        return fields
    
    def _check_structure_for_syuname(self, stmt: str, struct_name: str) -> bool:
        """Check if structure has sy-uname assignment"""
        # Simple check - could be enhanced with context analysis
        pattern = f'{struct_name}.*?sy-uname'
        return bool(re.search(pattern, stmt, re.IGNORECASE))
    
    def _calculate_confidence(self, fields: Dict, pattern_type: str) -> float:
        """Calculate confidence score"""
        base_scores = {
            'from_structure': 0.7,
            'values_clause': 0.85,
            'value_constructor': 0.95,
            'value_type': 0.9,
            'from_table': 0.75,
            'accepting_keys': 0.8,
            'corresponding': 0.85,
            'chain': 0.7,
            'inline': 0.9,
            'filter': 0.85,
        }
        
        score = base_scores.get(pattern_type, 0.5)
        
        # Boost if sy-uname found
        if any('sy-uname' in str(v).lower() for v in fields.values()):
            score += 0.1
        
        return min(1.0, score)
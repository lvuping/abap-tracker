"""
Enhanced MODIFY Handler with support for all ABAP MODIFY patterns
"""

import re
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class ModifyPattern:
    """Represents a MODIFY pattern match"""
    table: str
    fields: Dict[str, str]
    line_number: int
    pattern_type: str
    confidence: float
    transporting: List[str]
    raw_statement: str


class EnhancedModifyHandler:
    """Enhanced handler for all MODIFY patterns in ABAP"""
    
    def __init__(self):
        # Comprehensive MODIFY patterns
        self.patterns = {
            # Basic MODIFY FROM structure
            'from_structure': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+(\w+)(?:\s|\.)',
                re.IGNORECASE
            ),
            
            # MODIFY FROM TABLE
            'from_table': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+TABLE\s+(\w+)',
                re.IGNORECASE
            ),
            
            # MODIFY with TRANSPORTING
            'transporting': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+TRANSPORTING\s+(.*?)(?:WHERE|\.)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # MODIFY internal table by INDEX
            'by_index': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+INDEX\s+(\d+)',
                re.IGNORECASE
            ),
            
            # MODIFY with WHERE
            'with_where': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+(\w+).*?WHERE\s+(.*?)(?:\.)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # MODIFY TABLE (for sorted/hashed tables)
            'modify_table': re.compile(
                r'MODIFY\s+TABLE\s+(\w+)\s+FROM\s+(\w+)',
                re.IGNORECASE
            ),
            
            # MODIFY with VALUE constructor
            'value_constructor': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+#?\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # MODIFY with inline VALUE type
            'value_type': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # MODIFY with dynamic table name
            'dynamic': re.compile(
                r'MODIFY\s+\((\w+)\)\s+FROM\s+(\w+)',
                re.IGNORECASE
            ),
            
            # MODIFY with COND
            'with_cond': re.compile(
                r'MODIFY\s+(\w+)\s+FROM\s+VALUE.*?COND\s+#\s*\((.*?)\)',
                re.IGNORECASE | re.DOTALL
            ),
            
            # MODIFY in LOOP with field symbols
            'loop_fs': re.compile(
                r'LOOP.*?ASSIGNING\s+<(\w+)>.*?MODIFY\s+(\w+)',
                re.IGNORECASE | re.DOTALL
            ),
        }
        
        # Pattern to extract field assignments
        self.field_assignment = re.compile(
            r'(\w+)\s*=\s*([^,\)]+)'
        )
    
    def analyze_modify(self, code_lines: List[str], start_line: int = 0) -> List[ModifyPattern]:
        """Analyze code for all MODIFY patterns"""
        results = []
        
        # Join lines for analysis
        code = self._join_lines(code_lines)
        
        # Split into statements
        statements = self._split_statements(code)
        
        for idx, stmt in enumerate(statements):
            if 'MODIFY' not in stmt.upper():
                continue
            
            # Skip MODIFY permissions, MODIFY SCREEN, etc.
            if any(skip in stmt.upper() for skip in ['MODIFY SCREEN', 'MODIFY LINE']):
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
        """Join lines handling ABAP continuation"""
        joined = []
        for line in lines:
            # Remove comments
            clean_line = line
            if '"' in line:
                parts = line.split('"')
                clean_line = parts[0] if parts else line
            
            # Skip full-line comments
            if clean_line.strip().startswith('*'):
                continue
            
            joined.append(clean_line.strip())
        
        return ' '.join(joined)
    
    def _split_statements(self, code: str) -> List[str]:
        """Split code into statements"""
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
        
        if current:
            stmt = ''.join(current).strip()
            if stmt:
                statements.append(stmt)
        
        return statements
    
    def _process_match(self, match, stmt: str, pattern_type: str, line_num: int) -> Optional[ModifyPattern]:
        """Process a pattern match"""
        table = None
        fields = {}
        transporting = []
        
        if pattern_type == 'from_structure':
            table = match.group(1).upper()
            struct_name = match.group(2)
            # Check for sy-uname in context
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type == 'from_table':
            table = match.group(1).upper()
            table_var = match.group(2)
            if 'SY-UNAME' in stmt.upper():
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type == 'transporting':
            table = match.group(1).upper()
            struct_name = match.group(2)
            transport_fields = match.group(3)
            
            # Parse transported fields
            transporting = self._parse_transporting(transport_fields)
            
            # Check for sy-uname
            if self._check_structure_for_syuname(stmt, struct_name):
                for field in transporting:
                    if 'BY' in field.upper() or 'USER' in field.upper():
                        fields[field] = 'sy-uname'
        
        elif pattern_type == 'by_index':
            table = match.group(1).upper()
            struct_name = match.group(2)
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type == 'with_where':
            table = match.group(1).upper()
            struct_name = match.group(2)
            where_clause = match.group(3)
            
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['PROCESSOR'] = 'sy-uname'
        
        elif pattern_type == 'modify_table':
            table = match.group(1).upper()
            struct_name = match.group(2)
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type in ['value_constructor', 'value_type']:
            table = match.group(1).upper()
            if pattern_type == 'value_type':
                field_content = match.group(3)
            else:
                field_content = match.group(2)
            fields = self._parse_field_assignments(field_content)
        
        elif pattern_type == 'dynamic':
            table_var = match.group(1)  # Dynamic table name
            struct_name = match.group(2)
            table = 'DYNAMIC_TABLE'  # Placeholder
            if self._check_structure_for_syuname(stmt, struct_name):
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type == 'with_cond':
            table = match.group(1).upper()
            cond_content = match.group(2)
            # Parse COND content for sy-uname
            if 'SY-UNAME' in cond_content.upper():
                fields['MODIFIED_BY'] = 'sy-uname'
        
        elif pattern_type == 'loop_fs':
            fs_name = match.group(1)
            table = match.group(2).upper()
            # Check for sy-uname assignments to field symbol
            if 'SY-UNAME' in stmt.upper():
                fields['LAST_CHANGED_BY'] = 'sy-uname'
        
        if table:
            return ModifyPattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type=pattern_type,
                confidence=self._calculate_confidence(fields, pattern_type),
                transporting=transporting,
                raw_statement=stmt[:200]
            )
        
        return None
    
    def _check_structure_for_syuname(self, stmt: str, struct_name: str) -> bool:
        """Check if structure has sy-uname assignment in context"""
        # Look for patterns like: struct-field = sy-uname
        patterns = [
            f'{struct_name}.*?sy-uname',
            f'{struct_name}.*?sy_uname',
            f'sy-uname.*?{struct_name}',
            'modified_by.*?sy-uname',
            'changed_by.*?sy-uname',
        ]
        
        for pattern in patterns:
            if re.search(pattern, stmt, re.IGNORECASE):
                return True
        
        return False
    
    def _parse_transporting(self, transport_clause: str) -> List[str]:
        """Parse TRANSPORTING clause"""
        # Clean up
        transport_clause = transport_clause.replace('\n', ' ')
        
        # Extract field names
        fields = re.findall(r'\b(\w+)\b', transport_clause)
        
        # Filter out keywords
        keywords = ['WHERE', 'AND', 'OR', 'NOT', 'EQ', 'NE', 'GT', 'LT']
        fields = [f.upper() for f in fields if f.upper() not in keywords]
        
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
            value = value.strip().rstrip(',)')
            
            # Check for system variables
            if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                fields[field] = 'sy-uname'
            else:
                fields[field] = value
        
        return fields
    
    def _calculate_confidence(self, fields: Dict, pattern_type: str) -> float:
        """Calculate confidence score"""
        base_scores = {
            'from_structure': 0.8,
            'from_table': 0.85,
            'transporting': 0.9,
            'by_index': 0.75,
            'with_where': 0.85,
            'modify_table': 0.9,
            'value_constructor': 0.95,
            'value_type': 0.9,
            'dynamic': 0.7,
            'with_cond': 0.85,
            'loop_fs': 0.8,
        }
        
        score = base_scores.get(pattern_type, 0.5)
        
        # Boost if sy-uname found
        if any('sy-uname' in str(v).lower() for v in fields.values()):
            score += 0.1
        
        return min(1.0, score)
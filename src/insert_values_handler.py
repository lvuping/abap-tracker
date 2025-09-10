"""
Enhanced INSERT VALUES Pattern Handler for ABAP Code Analysis
Handles modern ABAP syntax including VALUE # constructors
"""

import re
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from enum import Enum


@dataclass
class InsertValuesPattern:
    """Represents an INSERT VALUES pattern match"""
    table: str
    fields: Dict[str, str]  # field_name -> value
    line_number: int
    pattern_type: str  # 'direct_values', 'value_constructor', 'multi_values'
    confidence: float


class InsertValuesHandler:
    """Enhanced handler for INSERT VALUES patterns in ABAP"""
    
    def __init__(self):
        # Pattern 1: INSERT INTO table VALUES (field1, field2, ...)
        self.direct_values_pattern = re.compile(
            r"INSERT\s+(?:INTO\s+)?(\w+)\s+VALUES\s*\((.*?)\)",
            re.IGNORECASE | re.DOTALL
        )
        
        # Pattern 2: INSERT table FROM VALUE #( field = value ... )
        self.value_constructor_pattern = re.compile(
            r"INSERT\s+(\w+)\s+FROM\s+(?:@\s*)?\(\s*VALUE\s+#\s*\((.*?)\)\s*\)",
            re.IGNORECASE | re.DOTALL
        )
        
        # Pattern 3: INSERT table FROM VALUE type( field = value ... )
        self.value_type_pattern = re.compile(
            r"INSERT\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)",
            re.IGNORECASE | re.DOTALL
        )
        
        # Pattern 4: Multiple VALUES in one INSERT
        self.multi_values_pattern = re.compile(
            r"INSERT\s+(?:INTO\s+)?(\w+)\s+VALUES\s+(.*?)(?:\.|\s*$)",
            re.IGNORECASE | re.DOTALL
        )
        
        # Field assignment pattern within VALUE constructor
        self.field_assignment_pattern = re.compile(
            r"(\w+)\s*=\s*([^,\)]+)"
        )
        
        # Direct values parsing (positional)
        self.values_list_pattern = re.compile(
            r"'[^']*'|\"[^\"]*\"|[^,\s\)]+",
            re.IGNORECASE
        )
        
        # System variables we're tracking
        self.system_variables = [
            'SY-UNAME', 'SY-DATUM', 'SY-UZEIT', 'SY-MANDT', 
            'SY-LANGU', 'SY-SUBRC', 'SY-TABIX'
        ]
        
    def analyze_insert_values(self, code_lines: List[str], start_line: int = 0) -> List[InsertValuesPattern]:
        """
        Analyze code for INSERT VALUES patterns
        
        Args:
            code_lines: List of code lines to analyze
            start_line: Starting line number for reporting
            
        Returns:
            List of detected INSERT VALUES patterns
        """
        patterns_found = []
        
        # Join lines for multi-line statement analysis
        code_text = '\n'.join(code_lines)
        
        # Remove comments
        code_text = self._remove_comments(code_text)
        
        # Split into statements
        statements = self._split_statements(code_text)
        
        for stmt_idx, statement in enumerate(statements):
            # Try each pattern type
            result = self._try_value_constructor(statement, start_line + stmt_idx)
            if result:
                patterns_found.append(result)
                continue
                
            result = self._try_direct_values(statement, start_line + stmt_idx)
            if result:
                patterns_found.append(result)
                continue
                
            result = self._try_value_type(statement, start_line + stmt_idx)
            if result:
                patterns_found.append(result)
                continue
                
            result = self._try_multi_values(statement, start_line + stmt_idx)
            if result:
                patterns_found.extend(result)
        
        return patterns_found
    
    def _try_value_constructor(self, statement: str, line_num: int) -> Optional[InsertValuesPattern]:
        """Try to match VALUE # constructor pattern"""
        match = self.value_constructor_pattern.search(statement)
        if not match:
            return None
            
        table = match.group(1).upper()
        fields_content = match.group(2)
        
        # Parse field assignments
        fields = self._parse_field_assignments(fields_content)
        
        if fields:
            return InsertValuesPattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type='value_constructor',
                confidence=0.95
            )
        
        return None
    
    def _try_direct_values(self, statement: str, line_num: int) -> Optional[InsertValuesPattern]:
        """Try to match direct VALUES pattern"""
        match = self.direct_values_pattern.search(statement)
        if not match:
            return None
            
        table = match.group(1).upper()
        values_content = match.group(2)
        
        # Parse values list (positional)
        values = self._parse_values_list(values_content)
        
        if values:
            # Create field mapping (generic field names for positional)
            fields = {f"FIELD{i+1}": val for i, val in enumerate(values)}
            
            # Try to identify specific fields if we see patterns
            fields = self._identify_known_fields(fields, table)
            
            return InsertValuesPattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type='direct_values',
                confidence=0.85
            )
        
        return None
    
    def _try_value_type(self, statement: str, line_num: int) -> Optional[InsertValuesPattern]:
        """Try to match VALUE type( ... ) pattern"""
        match = self.value_type_pattern.search(statement)
        if not match:
            return None
            
        table = match.group(1).upper()
        value_type = match.group(2).upper()
        fields_content = match.group(3)
        
        # Parse field assignments
        fields = self._parse_field_assignments(fields_content)
        
        if fields:
            return InsertValuesPattern(
                table=table,
                fields=fields,
                line_number=line_num,
                pattern_type='value_type',
                confidence=0.90
            )
        
        return None
    
    def _try_multi_values(self, statement: str, line_num: int) -> List[InsertValuesPattern]:
        """Try to match multiple VALUES in one INSERT"""
        patterns = []
        
        # Check for pattern like: VALUES (row1), (row2), (row3)
        if 'VALUES' in statement.upper():
            # Extract table name
            table_match = re.search(r'INSERT\s+(?:INTO\s+)?(\w+)', statement, re.IGNORECASE)
            if table_match:
                table = table_match.group(1).upper()
                
                # Find all value sets
                value_sets = re.findall(r'\((.*?)\)', statement)
                
                for idx, value_set in enumerate(value_sets):
                    if value_set and not value_set.startswith('VALUE'):
                        values = self._parse_values_list(value_set)
                        if values:
                            fields = {f"FIELD{i+1}": val for i, val in enumerate(values)}
                            fields = self._identify_known_fields(fields, table)
                            
                            patterns.append(InsertValuesPattern(
                                table=table,
                                fields=fields,
                                line_number=line_num,
                                pattern_type='multi_values',
                                confidence=0.80
                            ))
        
        return patterns
    
    def _parse_field_assignments(self, content: str) -> Dict[str, str]:
        """Parse field = value assignments"""
        fields = {}
        
        # Handle multi-line content
        content = content.replace('\n', ' ')
        
        # Find all field = value patterns
        matches = self.field_assignment_pattern.findall(content)
        
        for field_name, field_value in matches:
            field_name = field_name.strip().upper()
            field_value = field_value.strip()
            
            # Remove trailing commas or parentheses
            field_value = field_value.rstrip(',)').strip()
            
            fields[field_name] = field_value
        
        return fields
    
    def _parse_values_list(self, content: str) -> List[str]:
        """Parse comma-separated values list"""
        values = []
        
        # Remove newlines
        content = content.replace('\n', ' ')
        
        # Find all values (quoted strings or unquoted values)
        matches = self.values_list_pattern.findall(content)
        
        for match in matches:
            value = match.strip()
            if value and value not in [',', '(', ')']:
                values.append(value)
        
        return values
    
    def _identify_known_fields(self, fields: Dict[str, str], table: str) -> Dict[str, str]:
        """Try to identify known field names based on values"""
        identified = {}
        
        for field_name, value in fields.items():
            # Check for system variables
            value_upper = value.upper()
            
            if 'SY-UNAME' in value_upper or 'SY-MANDT' in value_upper:
                # Common user tracking fields
                if 'SY-UNAME' in value_upper:
                    if 'CREATED' not in identified and 'CHANGED' not in identified:
                        identified['CREATED_BY'] = value
                    else:
                        identified['CHANGED_BY'] = value
                elif 'SY-MANDT' in value_upper:
                    identified['CLIENT'] = value
            elif 'SY-DATUM' in value_upper:
                # Date fields
                if 'CREATED' not in identified:
                    identified['CREATED_DATE'] = value
                else:
                    identified['CHANGED_DATE'] = value
            else:
                # Keep original field name
                identified[field_name] = value
        
        return identified if identified else fields
    
    def _remove_comments(self, code: str) -> str:
        """Remove ABAP comments from code"""
        # Remove full-line comments
        code = re.sub(r'^\s*\*.*$', '', code, flags=re.MULTILINE)
        # Remove inline comments
        code = re.sub(r'"[^"]*"', '', code)
        return code
    
    def _split_statements(self, code: str) -> List[str]:
        """Split code into individual statements"""
        # Split by period (ABAP statement terminator)
        statements = re.split(r'\.\s*\n', code)
        
        # Clean and filter statements
        cleaned = []
        for stmt in statements:
            stmt = stmt.strip()
            if stmt and 'INSERT' in stmt.upper():
                cleaned.append(stmt)
        
        return cleaned
    
    def find_tainted_fields(self, pattern: InsertValuesPattern, tainted_vars: List[str]) -> List[str]:
        """
        Find which fields are tainted by system variables or tainted variables
        
        Args:
            pattern: The INSERT VALUES pattern to check
            tainted_vars: List of known tainted variables
            
        Returns:
            List of tainted field names
        """
        tainted_fields = []
        
        for field_name, value in pattern.fields.items():
            value_upper = value.upper()
            
            # Check for system variables
            if any(sys_var in value_upper for sys_var in self.system_variables):
                tainted_fields.append(field_name)
                continue
            
            # Check for tainted variables
            for tainted_var in tainted_vars:
                if tainted_var.upper() in value_upper:
                    tainted_fields.append(field_name)
                    break
        
        return tainted_fields
    
    def get_affected_table_fields(self, pattern: InsertValuesPattern) -> Tuple[str, List[str]]:
        """
        Get the affected table and fields from an INSERT VALUES pattern
        
        Returns:
            Tuple of (table_name, list_of_affected_fields)
        """
        tainted_fields = []
        
        for field_name, value in pattern.fields.items():
            value_upper = value.upper()
            
            # Check if this field contains SY-UNAME or related system variables
            if 'SY-UNAME' in value_upper:
                # Map generic field names to likely actual names
                if field_name.startswith('FIELD'):
                    # Try to guess the actual field name
                    if 'CREATED' in pattern.table or not tainted_fields:
                        tainted_fields.append('CREATED_BY')
                    else:
                        tainted_fields.append('CHANGED_BY')
                else:
                    tainted_fields.append(field_name)
        
        return pattern.table, tainted_fields


# Example usage and testing
if __name__ == "__main__":
    handler = InsertValuesHandler()
    
    # Test cases
    test_code = [
        "INSERT ztable FROM @( VALUE #( field1 = 'INLINE'",
        "                                field2 = 'INSERT'",
        "                                created_by = sy-uname",
        "                                created_on = sy-datum ) ).",
        "",
        "INSERT INTO zuser_log VALUES ( sy-mandt, sy-uname, 'LOGIN', sy-uzeit ).",
        "",
        "INSERT zuser_records VALUES",
        "  ( '001', sy-uname, 'CREATE', sy-datum ),",
        "  ( '002', lv_user, 'UPDATE', sy-datum )."
    ]
    
    patterns = handler.analyze_insert_values(test_code)
    
    for pattern in patterns:
        print(f"Line {pattern.line_number}: {pattern.pattern_type}")
        print(f"  Table: {pattern.table}")
        print(f"  Fields: {pattern.fields}")
        print(f"  Confidence: {pattern.confidence}")
        
        # Check for tainted fields
        tainted = handler.find_tainted_fields(pattern, ['LV_USER'])
        if tainted:
            print(f"  Tainted fields: {tainted}")
        print()
"""
Comprehensive Database Operations Handler
Handles INSERT, UPDATE, MODIFY with full context awareness
"""

import re
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass

@dataclass
class DBOperation:
    """Represents a database operation"""
    operation: str  # INSERT, UPDATE, MODIFY
    table: str
    fields: Dict[str, str]
    line_number: int
    pattern_type: str
    confidence: float
    context: str


class ComprehensiveDBHandler:
    """Comprehensive handler for all database operations"""
    
    def __init__(self):
        # Pattern to remove comments
        self.comment_pattern = re.compile(r'^\s*\*.*$', re.MULTILINE)
        
        # System variables
        self.sys_vars = ['SY-UNAME', 'SY_UNAME', 'SY-DATUM', 'SY-UZEIT', 'SY-MANDT']
    
    def analyze_operations(self, lines: List[str], start_idx: int = 0, end_idx: int = None) -> List[DBOperation]:
        """Analyze code for database operations"""
        if end_idx is None:
            end_idx = len(lines)
        
        results = []
        
        # Track variable assignments for context
        variable_context = {}
        
        # Process lines
        for i in range(start_idx, end_idx):
            line = lines[i]
            
            # Skip comments
            if line.strip().startswith('*'):
                continue
            
            # Track variable assignments
            self._track_assignments(line, variable_context)
            
            # Check for INSERT
            if self._is_insert(line):
                op = self._analyze_insert(lines, i, variable_context)
                if op:
                    results.append(op)
            
            # Check for UPDATE
            elif self._is_update(line):
                op = self._analyze_update(lines, i, variable_context)
                if op:
                    results.append(op)
            
            # Check for MODIFY
            elif self._is_modify(line):
                op = self._analyze_modify(lines, i, variable_context)
                if op:
                    results.append(op)
        
        return results
    
    def _is_insert(self, line: str) -> bool:
        """Check if line contains INSERT"""
        return bool(re.search(r'\bINSERT\b', line, re.IGNORECASE))
    
    def _is_update(self, line: str) -> bool:
        """Check if line contains UPDATE"""
        return bool(re.search(r'\bUPDATE\b', line, re.IGNORECASE))
    
    def _is_modify(self, line: str) -> bool:
        """Check if line contains MODIFY"""
        return bool(re.search(r'\bMODIFY\b', line, re.IGNORECASE))
    
    def _track_assignments(self, line: str, context: Dict):
        """Track variable assignments for context"""
        # Track structure field assignments like: ls_record-created_by = sy-uname
        match = re.search(r'(\w+)-(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
        if match:
            struct = match.group(1)
            field = match.group(2)
            value = match.group(3).strip()
            
            if struct not in context:
                context[struct] = {}
            
            # Check if value contains sy-uname
            if any(sv in value.upper() for sv in self.sys_vars):
                context[struct][field] = value
        
        # Track simple variable assignments
        match = re.search(r'^(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
        if match:
            var = match.group(1)
            value = match.group(2).strip()
            if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                context[var] = 'sy-uname'
    
    def _analyze_insert(self, lines: List[str], line_idx: int, context: Dict) -> Optional[DBOperation]:
        """Analyze INSERT operation"""
        line = lines[line_idx]
        
        # Pattern 1: INSERT table FROM structure
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+(@?\s*)?(\w+)', line, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(3)
            
            # Check if struct has sy-uname in context
            fields = {}
            if struct in context:
                if isinstance(context[struct], dict):
                    for field, value in context[struct].items():
                        if any(sv in value.upper() for sv in self.sys_vars):
                            fields[field.upper()] = 'sy-uname'
                elif context[struct] == 'sy-uname':
                    fields['CREATED_BY'] = 'sy-uname'
            
            # Check for field symbols
            if struct.startswith('<') and struct.endswith('>'):
                # Look back for field symbol assignments
                for i in range(max(0, line_idx - 10), line_idx):
                    if 'sy-uname' in lines[i].lower() and struct in lines[i]:
                        fields['CREATED_BY'] = 'sy-uname'
            
            return DBOperation(
                operation='INSERT',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='from_structure',
                confidence=0.8 if fields else 0.6,
                context=line[:100]
            )
        
        # Pattern 2: INSERT with VALUES
        match = re.search(r'INSERT\s+(?:INTO\s+)?(\w+)\s+VALUES\s*\((.*?)\)', line, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            values = match.group(2)
            
            fields = {}
            if 'sy-uname' in values.lower():
                fields['USER_FIELD'] = 'sy-uname'
            
            return DBOperation(
                operation='INSERT',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='values',
                confidence=0.9 if fields else 0.7,
                context=line[:100]
            )
        
        # Pattern 3: INSERT with VALUE constructor - handle multiline
        if 'VALUE' in line.upper():
            # Collect full statement
            full_stmt = self._collect_statement(lines, line_idx)
            
            # Parse VALUE constructor
            match = re.search(r'INSERT\s+(\w+)\s+FROM\s+(?:@\s*)?\(?VALUE\s+#?\s*\((.*?)\)\)?', full_stmt, re.IGNORECASE | re.DOTALL)
            if match:
                table = match.group(1).upper()
                content = match.group(2)
                
                fields = self._parse_value_constructor(content)
                
                return DBOperation(
                    operation='INSERT',
                    table=table,
                    fields=fields,
                    line_number=line_idx,
                    pattern_type='value_constructor',
                    confidence=0.95 if fields else 0.8,
                    context=full_stmt[:100]
                )
        
        # Pattern 4: INSERT FROM TABLE
        match = re.search(r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', line, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            table_var = match.group(2)
            
            fields = {}
            # Check if table variable has sy-uname in context
            if table_var in context:
                fields['CREATED_BY'] = 'sy-uname'
            
            return DBOperation(
                operation='INSERT',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='from_table',
                confidence=0.85 if fields else 0.7,
                context=line[:100]
            )
        
        return None
    
    def _analyze_update(self, lines: List[str], line_idx: int, context: Dict) -> Optional[DBOperation]:
        """Analyze UPDATE operation"""
        # Collect full statement
        full_stmt = self._collect_statement(lines, line_idx)
        
        # Parse UPDATE
        match = re.search(r'UPDATE\s+(\w+)\s+(?:CLIENT\s+SPECIFIED\s+)?SET\s+(.*?)(?:WHERE|$)', full_stmt, re.IGNORECASE | re.DOTALL)
        if match:
            table = match.group(1).upper()
            set_clause = match.group(2)
            
            fields = self._parse_set_clause(set_clause)
            
            return DBOperation(
                operation='UPDATE',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='set_clause',
                confidence=0.95 if fields else 0.8,
                context=full_stmt[:100]
            )
        
        # UPDATE FROM structure
        match = re.search(r'UPDATE\s+(\w+)\s+FROM\s+(\w+)', full_stmt, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)
            
            fields = {}
            if struct in context and isinstance(context[struct], dict):
                for field, value in context[struct].items():
                    if any(sv in value.upper() for sv in self.sys_vars):
                        fields[field.upper()] = 'sy-uname'
            
            return DBOperation(
                operation='UPDATE',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='from_structure',
                confidence=0.8 if fields else 0.6,
                context=full_stmt[:100]
            )
        
        return None
    
    def _analyze_modify(self, lines: List[str], line_idx: int, context: Dict) -> Optional[DBOperation]:
        """Analyze MODIFY operation"""
        line = lines[line_idx]
        
        # Skip MODIFY SCREEN, MODIFY LINE
        if 'MODIFY SCREEN' in line.upper() or 'MODIFY LINE' in line.upper():
            return None
        
        # MODIFY FROM structure
        match = re.search(r'MODIFY\s+(?:TABLE\s+)?(\w+)\s+FROM\s+(\w+)', line, re.IGNORECASE)
        if match:
            table = match.group(1).upper()
            struct = match.group(2)
            
            fields = {}
            if struct in context:
                if isinstance(context[struct], dict):
                    for field, value in context[struct].items():
                        if any(sv in value.upper() for sv in self.sys_vars):
                            fields[field.upper()] = 'sy-uname'
                elif context[struct] == 'sy-uname':
                    fields['MODIFIED_BY'] = 'sy-uname'
            
            return DBOperation(
                operation='MODIFY',
                table=table,
                fields=fields,
                line_number=line_idx,
                pattern_type='from_structure',
                confidence=0.85 if fields else 0.7,
                context=line[:100]
            )
        
        # MODIFY with VALUE constructor
        if 'VALUE' in line.upper():
            full_stmt = self._collect_statement(lines, line_idx)
            
            match = re.search(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+#?\s*\((.*?)\)', full_stmt, re.IGNORECASE | re.DOTALL)
            if match:
                table = match.group(1).upper()
                content = match.group(2)
                
                fields = self._parse_value_constructor(content)
                
                return DBOperation(
                    operation='MODIFY',
                    table=table,
                    fields=fields,
                    line_number=line_idx,
                    pattern_type='value_constructor',
                    confidence=0.95 if fields else 0.8,
                    context=full_stmt[:100]
                )
        
        return None
    
    def _collect_statement(self, lines: List[str], start_idx: int) -> str:
        """Collect full statement (handles multiline)"""
        stmt = []
        
        # Look for statement start and end
        for i in range(start_idx, min(len(lines), start_idx + 20)):
            line = lines[i].strip()
            
            # Skip comments
            if line.startswith('*'):
                continue
            
            stmt.append(line)
            
            # Check for statement end
            if line.endswith('.'):
                break
        
        return ' '.join(stmt)
    
    def _parse_value_constructor(self, content: str) -> Dict[str, str]:
        """Parse VALUE constructor content"""
        fields = {}
        
        # Find field assignments
        matches = re.findall(r'(\w+)\s*=\s*([^,\)]+)', content, re.IGNORECASE)
        
        for field, value in matches:
            field = field.strip().upper()
            value = value.strip()
            
            if any(sv in value.upper() for sv in self.sys_vars):
                fields[field] = 'sy-uname'
        
        return fields
    
    def _parse_set_clause(self, set_clause: str) -> Dict[str, str]:
        """Parse SET clause"""
        fields = {}
        
        # Find field = value assignments
        matches = re.findall(r'(\w+)\s*=\s*([^,]+?)(?:,|$|\s+WHERE)', set_clause, re.IGNORECASE)
        
        for field, value in matches:
            field = field.strip().upper()
            value = value.strip()
            
            if any(sv in value.upper() for sv in self.sys_vars):
                fields[field] = 'sy-uname'
        
        return fields
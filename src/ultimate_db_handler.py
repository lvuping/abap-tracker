"""
Ultimate Database Operations Handler
Complete support for ALL INSERT, UPDATE, MODIFY patterns in ABAP
"""

import re
from typing import Dict, List, Optional, Tuple, Set
from dataclasses import dataclass
from enum import Enum

class OperationType(Enum):
    INSERT = "INSERT"
    UPDATE = "UPDATE"
    MODIFY = "MODIFY"

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

class UltimateDBHandler:
    """Ultimate handler for all ABAP database operations"""
    
    def __init__(self):
        # System variables to track
        self.sys_vars = {
            'SY-UNAME', 'SY_UNAME', 'SY-DATUM', 'SY-UZEIT', 
            'SY-MANDT', 'SY-TABIX', 'SY-SUBRC', 'SY-LANGU'
        }
        
        # Keywords to skip
        self.skip_patterns = {
            'MODIFY SCREEN', 'MODIFY LINE', 'MODIFY CURRENT',
            'INSERT REPORT', 'INSERT TEXTPOOL', 'UPDATE TASK'
        }
        
        # Initialize pattern matchers
        self._init_patterns()
    
    def _init_patterns(self):
        """Initialize all regex patterns"""
        
        # INSERT patterns
        self.insert_patterns = {
            # Basic patterns
            'from_struct': re.compile(r'INSERT\s+(\w+)\s+FROM\s+@?(\w+)', re.IGNORECASE),
            'into_values': re.compile(r'INSERT\s+INTO\s+(\w+)\s+VALUES\s+(\w+|\(.*?\))', re.IGNORECASE | re.DOTALL),
            'values_list': re.compile(r'INSERT\s+(\w+)\s+VALUES\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'from_table': re.compile(r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', re.IGNORECASE),
            'lines_of': re.compile(r'INSERT\s+LINES\s+OF\s+(\w+)\s+INTO\s+TABLE\s+(\w+)', re.IGNORECASE),
            'into_table': re.compile(r'INSERT\s+(\w+)\s+INTO\s+TABLE\s+(\w+)', re.IGNORECASE),
            'accepting': re.compile(r'INSERT\s+(\w+)\s+FROM\s+TABLE\s+(\w+)\s+ACCEPTING', re.IGNORECASE),
            'client_spec': re.compile(r'INSERT\s+(\w+)\s+CLIENT\s+SPECIFIED', re.IGNORECASE),
            
            # VALUE constructors
            'value_inline': re.compile(r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+#\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'value_typed': re.compile(r'INSERT\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'value_at': re.compile(r'INSERT\s+(\w+)\s+FROM\s+@\s*\(\s*VALUE\s+#\s*\((.*?)\)\s*\)', re.IGNORECASE | re.DOTALL),
            
            # Advanced patterns
            'corresponding': re.compile(r'INSERT\s+(\w+)\s+FROM\s+CORRESPONDING\s+#\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'conv': re.compile(r'INSERT\s+(\w+)\s+FROM\s+CONV\s+#\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'reduce': re.compile(r'INSERT\s+(\w+)\s+FROM.*?REDUCE.*?created_by\s*=\s*sy-uname', re.IGNORECASE | re.DOTALL),
            'cond': re.compile(r'INSERT\s+(\w+)\s+FROM.*?COND\s+#.*?sy-uname', re.IGNORECASE | re.DOTALL),
            
            # Internal table patterns
            'into_index': re.compile(r'INSERT\s+(\w+)\s+INTO\s+(\w+)\s+INDEX\s+(\d+)', re.IGNORECASE),
            'initial_line': re.compile(r'INSERT\s+INITIAL\s+LINE\s+INTO\s+(\w+)', re.IGNORECASE),
            'assigning': re.compile(r'INSERT.*?INTO\s+(\w+)\s+ASSIGNING\s+<(\w+)>', re.IGNORECASE),
            'reference': re.compile(r'INSERT.*?INTO\s+(\w+)\s+REFERENCE\s+INTO\s+(\w+)', re.IGNORECASE),
        }
        
        # UPDATE patterns
        self.update_patterns = {
            # Basic patterns
            'set_simple': re.compile(r'UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|$)', re.IGNORECASE | re.DOTALL),
            'from_struct': re.compile(r'UPDATE\s+(\w+)\s+FROM\s+@?(\w+)', re.IGNORECASE),
            'client_spec': re.compile(r'UPDATE\s+(\w+)\s+CLIENT\s+SPECIFIED\s+SET', re.IGNORECASE),
            
            # Complex WHERE
            'set_where': re.compile(r'UPDATE\s+(\w+)\s+SET\s+(.*?)\s+WHERE\s+(.*?)(?:\.|$)', re.IGNORECASE | re.DOTALL),
            'subquery': re.compile(r'UPDATE\s+(\w+)\s+SET\s+(.*?)\s+WHERE.*?IN\s*\(.*?SELECT', re.IGNORECASE | re.DOTALL),
            'exists': re.compile(r'UPDATE\s+(\w+)\s+SET\s+(.*?)\s+WHERE\s+EXISTS', re.IGNORECASE | re.DOTALL),
            
            # Advanced patterns
            'case_when': re.compile(r'UPDATE\s+(\w+)\s+SET.*?CASE.*?WHEN.*?END', re.IGNORECASE | re.DOTALL),
            'nested_paren': re.compile(r'UPDATE\s+(\w+)\s+SET\s+\((.*?)\)\s*=\s*\((.*?)\)', re.IGNORECASE),
            'join_like': re.compile(r'UPDATE\s+(\w+)\s+(?:AS\s+\w+\s+)?SET.*?FROM\s+(\w+)', re.IGNORECASE | re.DOTALL),
            'inline_sql': re.compile(r'UPDATE\s+(\w+)\s+SET\s+.*?=\s*@', re.IGNORECASE | re.DOTALL),
            
            # Internal table
            'table_from': re.compile(r'UPDATE\s+TABLE\s+(\w+)\s+FROM\s+(\w+)', re.IGNORECASE),
            'transporting': re.compile(r'UPDATE\s+(\w+).*?TRANSPORTING\s+(.*?)(?:WHERE|$)', re.IGNORECASE | re.DOTALL),
        }
        
        # MODIFY patterns
        self.modify_patterns = {
            # Basic patterns
            'from_struct': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+@?(\w+)(?:\s|\.)', re.IGNORECASE),
            'from_table': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+TABLE\s+(\w+)', re.IGNORECASE),
            'client_spec': re.compile(r'MODIFY\s+(\w+)\s+CLIENT\s+SPECIFIED', re.IGNORECASE),
            
            # TRANSPORTING
            'transporting': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+TRANSPORTING\s+(.*?)(?:WHERE|\.)', re.IGNORECASE | re.DOTALL),
            'trans_where': re.compile(r'MODIFY\s+(\w+).*?TRANSPORTING\s+(.*?)\s+WHERE\s+(.*?)(?:\.)', re.IGNORECASE | re.DOTALL),
            
            # Internal table
            'by_index': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+(\w+)\s+INDEX\s+(\d+)', re.IGNORECASE),
            'index_trans': re.compile(r'MODIFY\s+(\w+).*?INDEX\s+(\d+)\s+TRANSPORTING\s+(.*?)(?:\.)', re.IGNORECASE | re.DOTALL),
            'table_modify': re.compile(r'MODIFY\s+TABLE\s+(\w+)\s+FROM\s+(\w+)', re.IGNORECASE),
            'table_trans': re.compile(r'MODIFY\s+TABLE\s+(\w+).*?TRANSPORTING\s+(.*?)(?:\.)', re.IGNORECASE | re.DOTALL),
            
            # VALUE constructors
            'value_inline': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+#\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'value_typed': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+VALUE\s+(\w+)\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            
            # Advanced
            'corresponding': re.compile(r'MODIFY\s+(\w+)\s+FROM\s+CORRESPONDING\s+#\s*\((.*?)\)', re.IGNORECASE | re.DOTALL),
            'cond': re.compile(r'MODIFY\s+(\w+)\s+FROM.*?COND\s+#.*?sy-uname', re.IGNORECASE | re.DOTALL),
            'reduce': re.compile(r'MODIFY\s+(\w+)\s+FROM.*?REDUCE.*?modified_by\s*=\s*sy-uname', re.IGNORECASE | re.DOTALL),
            'dynamic': re.compile(r'MODIFY\s+\((\w+)\)\s+FROM\s+(\w+)', re.IGNORECASE),
            
            # Special
            'lines_of': re.compile(r'MODIFY\s+LINES\s+OF\s+(\w+)', re.IGNORECASE),
        }
        
        # Chain statement pattern
        self.chain_pattern = re.compile(r'(INSERT|UPDATE|MODIFY)\s*:(.*?)(?:\.)', re.IGNORECASE | re.DOTALL)
        
        # Field assignment patterns
        self.field_assign = re.compile(r'(\w+)\s*=\s*([^,\)]+)', re.IGNORECASE)
        self.struct_field = re.compile(r'(\w+)-(\w+)\s*=\s*(.*?)(?:\.|,|$)', re.IGNORECASE)
    
    def analyze(self, lines: List[str], start_idx: int = 0, end_idx: int = None) -> List[DBOperation]:
        """Analyze lines for database operations"""
        if end_idx is None:
            end_idx = len(lines)
        
        results = []
        context = {}  # Track variable assignments
        
        # Pre-process to track assignments
        for i in range(start_idx, end_idx):
            line = lines[i].strip()
            
            # Skip comments
            if line.startswith('*'):
                continue
            
            # Track structure field assignments
            matches = self.struct_field.findall(line)
            for struct, field, value in matches:
                if struct not in context:
                    context[struct] = {}
                if self._contains_sy_uname(value):
                    context[struct][field] = 'sy-uname'
            
            # Track simple assignments
            if '=' in line and not any(op in line.upper() for op in ['INSERT', 'UPDATE', 'MODIFY']):
                parts = line.split('=')
                if len(parts) == 2:
                    var = parts[0].strip()
                    val = parts[1].strip().rstrip('.')
                    if self._contains_sy_uname(val):
                        context[var] = 'sy-uname'
        
        # Analyze operations
        i = start_idx
        while i < end_idx:
            line = lines[i]
            
            # Skip comments and special cases
            if line.strip().startswith('*') or self._should_skip(line):
                i += 1
                continue
            
            # Collect full statement
            stmt, end_line = self._collect_statement(lines, i, end_idx)
            
            # Check for chain statements
            if ':' in stmt and any(op in stmt.upper() for op in ['INSERT:', 'UPDATE:', 'MODIFY:']):
                chain_ops = self._analyze_chain(stmt, i, context)
                results.extend(chain_ops)
            else:
                # Analyze single statement
                op = self._analyze_statement(stmt, i, context)
                if op:
                    results.append(op)
            
            i = end_line + 1
        
        return results
    
    def _collect_statement(self, lines: List[str], start: int, end: int) -> Tuple[str, int]:
        """Collect complete statement including multiline"""
        stmt_lines = []
        end_line = start
        
        for i in range(start, end):
            line = lines[i].strip()
            
            # Skip full-line comments
            if line.startswith('*'):
                continue
            
            # Remove inline comments
            if '"' in line:
                line = line.split('"')[0]
            
            stmt_lines.append(line)
            
            # Check for statement end
            if line.endswith('.'):
                end_line = i
                break
            
            end_line = i
        
        return ' '.join(stmt_lines), end_line
    
    def _analyze_statement(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Analyze a single statement"""
        stmt_upper = stmt.upper()
        
        # Determine operation type
        if 'INSERT' in stmt_upper and 'INSERT REPORT' not in stmt_upper:
            return self._analyze_insert(stmt, line_num, context)
        elif 'UPDATE' in stmt_upper and 'UPDATE TASK' not in stmt_upper:
            return self._analyze_update(stmt, line_num, context)
        elif 'MODIFY' in stmt_upper and not self._should_skip(stmt):
            return self._analyze_modify(stmt, line_num, context)
        
        return None
    
    def _analyze_insert(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Analyze INSERT statement"""
        
        # Try each pattern
        for pattern_name, pattern in self.insert_patterns.items():
            match = pattern.search(stmt)
            if match:
                table = self._extract_table(match, pattern_name)
                fields = self._extract_fields(stmt, match, pattern_name, context)
                
                if table:
                    has_sy_uname = self._check_sy_uname(stmt, fields, context, match)
                    
                    return DBOperation(
                        operation=OperationType.INSERT,
                        table=table,
                        fields=fields,
                        line_number=line_num,
                        pattern=pattern_name,
                        confidence=0.9 if has_sy_uname else 0.7,
                        raw_statement=stmt[:200],
                        has_sy_uname=has_sy_uname
                    )
        
        return None
    
    def _analyze_update(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Analyze UPDATE statement"""
        
        for pattern_name, pattern in self.update_patterns.items():
            match = pattern.search(stmt)
            if match:
                table = match.group(1).upper() if match.groups() else None
                fields = {}
                
                # Extract fields from SET clause
                if 'SET' in stmt.upper():
                    set_match = re.search(r'SET\s+(.*?)(?:WHERE|$)', stmt, re.IGNORECASE | re.DOTALL)
                    if set_match:
                        fields = self._parse_set_clause(set_match.group(1))
                
                # Check for structure update
                elif pattern_name == 'from_struct' and len(match.groups()) > 1:
                    struct = match.group(2)
                    if struct in context and isinstance(context[struct], dict):
                        fields = {k: v for k, v in context[struct].items() if v == 'sy-uname'}
                
                if table:
                    has_sy_uname = any('sy-uname' in str(v).lower() for v in fields.values()) or self._contains_sy_uname(stmt)
                    
                    return DBOperation(
                        operation=OperationType.UPDATE,
                        table=table,
                        fields=fields,
                        line_number=line_num,
                        pattern=pattern_name,
                        confidence=0.95 if has_sy_uname else 0.8,
                        raw_statement=stmt[:200],
                        has_sy_uname=has_sy_uname
                    )
        
        return None
    
    def _analyze_modify(self, stmt: str, line_num: int, context: Dict) -> Optional[DBOperation]:
        """Analyze MODIFY statement"""
        
        for pattern_name, pattern in self.modify_patterns.items():
            match = pattern.search(stmt)
            if match:
                table = match.group(1).upper() if match.groups() else None
                fields = {}
                
                # Handle different pattern types
                if 'value' in pattern_name:
                    # Parse VALUE constructor
                    content = match.group(2) if len(match.groups()) > 1 else match.group(1)
                    fields = self._parse_value_constructor(content)
                
                elif 'transporting' in pattern_name:
                    # Parse TRANSPORTING fields
                    if len(match.groups()) > 2:
                        trans_fields = match.group(3) if pattern_name == 'trans_where' else match.group(2)
                        field_list = re.findall(r'\b(\w+)\b', trans_fields)
                        # Check context for these fields
                        struct = match.group(2) if len(match.groups()) > 1 else None
                        if struct in context and isinstance(context[struct], dict):
                            for field in field_list:
                                if field in context[struct]:
                                    fields[field] = context[struct][field]
                
                elif pattern_name == 'from_struct':
                    # Check structure in context
                    if len(match.groups()) > 1:
                        struct = match.group(2)
                        if struct in context:
                            if isinstance(context[struct], dict):
                                fields = {k: v for k, v in context[struct].items() if v == 'sy-uname'}
                            elif context[struct] == 'sy-uname':
                                fields = {'MODIFIED_BY': 'sy-uname'}
                
                if table:
                    has_sy_uname = any('sy-uname' in str(v).lower() for v in fields.values()) or self._contains_sy_uname(stmt)
                    
                    return DBOperation(
                        operation=OperationType.MODIFY,
                        table=table,
                        fields=fields,
                        line_number=line_num,
                        pattern=pattern_name,
                        confidence=0.9 if has_sy_uname else 0.75,
                        raw_statement=stmt[:200],
                        has_sy_uname=has_sy_uname
                    )
        
        return None
    
    def _analyze_chain(self, stmt: str, line_num: int, context: Dict) -> List[DBOperation]:
        """Analyze chain statements (with colon)"""
        results = []
        
        match = self.chain_pattern.search(stmt)
        if match:
            op_type = match.group(1).upper()
            chain_content = match.group(2)
            
            # Split by comma
            parts = self._split_chain(chain_content)
            
            for part in parts:
                # Create pseudo-statement
                pseudo_stmt = f"{op_type} {part}."
                
                if op_type == 'INSERT':
                    op = self._analyze_insert(pseudo_stmt, line_num, context)
                elif op_type == 'UPDATE':
                    op = self._analyze_update(pseudo_stmt, line_num, context)
                elif op_type == 'MODIFY':
                    op = self._analyze_modify(pseudo_stmt, line_num, context)
                else:
                    op = None
                
                if op:
                    results.append(op)
        
        return results
    
    def _split_chain(self, content: str) -> List[str]:
        """Split chain content by comma, respecting parentheses"""
        parts = []
        current = []
        paren_depth = 0
        
        for char in content:
            if char == '(':
                paren_depth += 1
            elif char == ')':
                paren_depth -= 1
            elif char == ',' and paren_depth == 0:
                parts.append(''.join(current).strip())
                current = []
                continue
            
            current.append(char)
        
        if current:
            parts.append(''.join(current).strip())
        
        return parts
    
    def _extract_table(self, match, pattern_name: str) -> Optional[str]:
        """Extract table name from match"""
        if 'lines_of' in pattern_name or 'into_table' in pattern_name:
            return match.group(2).upper() if len(match.groups()) > 1 else match.group(1).upper()
        else:
            return match.group(1).upper() if match.groups() else None
    
    def _extract_fields(self, stmt: str, match, pattern_name: str, context: Dict) -> Dict[str, str]:
        """Extract fields from statement"""
        fields = {}
        
        if 'value' in pattern_name or 'VALUES' in stmt.upper():
            # Parse VALUE constructor or VALUES clause
            if len(match.groups()) > 1:
                content = match.group(2)
                fields = self._parse_value_constructor(content)
        
        elif 'from_struct' in pattern_name or 'from_table' in pattern_name:
            # Check context for structure fields
            if len(match.groups()) > 1:
                struct = match.group(2)
                if struct in context:
                    if isinstance(context[struct], dict):
                        fields = {k: v for k, v in context[struct].items() if v == 'sy-uname'}
                    elif context[struct] == 'sy-uname':
                        fields = {'CREATED_BY': 'sy-uname'}
        
        elif 'corresponding' in pattern_name:
            # Parse CORRESPONDING
            if 'MAPPING' in stmt.upper():
                map_match = re.search(r'MAPPING\s+(.*?)(?:\)|$)', stmt, re.IGNORECASE)
                if map_match:
                    mappings = self.field_assign.findall(map_match.group(1))
                    for field, source in mappings:
                        if self._contains_sy_uname(source):
                            fields[field.upper()] = 'sy-uname'
        
        return fields
    
    def _parse_value_constructor(self, content: str) -> Dict[str, str]:
        """Parse VALUE constructor content"""
        fields = {}
        
        # Find all field assignments
        matches = self.field_assign.findall(content)
        
        for field, value in matches:
            field = field.strip().upper()
            value = value.strip().rstrip(',)')
            
            if self._contains_sy_uname(value):
                fields[field] = 'sy-uname'
        
        # Also check for positional values
        if not fields and self._contains_sy_uname(content):
            fields['USER_FIELD'] = 'sy-uname'
        
        return fields
    
    def _parse_set_clause(self, set_content: str) -> Dict[str, str]:
        """Parse UPDATE SET clause"""
        fields = {}
        
        # Handle nested parentheses assignment
        nested_match = re.search(r'\((.*?)\)\s*=\s*\((.*?)\)', set_content)
        if nested_match:
            field_list = [f.strip().upper() for f in nested_match.group(1).split(',')]
            value_list = [v.strip() for v in nested_match.group(2).split(',')]
            
            for i, field in enumerate(field_list):
                if i < len(value_list) and self._contains_sy_uname(value_list[i]):
                    fields[field] = 'sy-uname'
        else:
            # Regular field = value parsing
            # Split by comma but respect parentheses
            assignments = self._split_assignments(set_content)
            
            for assignment in assignments:
                match = re.search(r'(\w+)\s*=\s*(.+)', assignment)
                if match:
                    field = match.group(1).upper()
                    value = match.group(2)
                    
                    if self._contains_sy_uname(value):
                        fields[field] = 'sy-uname'
        
        return fields
    
    def _split_assignments(self, content: str) -> List[str]:
        """Split SET clause assignments respecting parentheses and CASE statements"""
        assignments = []
        current = []
        paren_depth = 0
        in_case = False
        
        tokens = content.replace('\n', ' ').split()
        
        for token in tokens:
            if token.upper() == 'CASE':
                in_case = True
            elif token.upper() == 'END':
                in_case = False
            
            current.append(token)
            
            # Check for assignment boundary
            if not in_case and paren_depth == 0:
                # Look for comma at end of token
                if token.endswith(','):
                    assignments.append(' '.join(current[:-1]) + token[:-1])
                    current = []
            
            # Track parentheses
            paren_depth += token.count('(') - token.count(')')
        
        if current:
            assignments.append(' '.join(current))
        
        return assignments
    
    def _check_sy_uname(self, stmt: str, fields: Dict, context: Dict, match) -> bool:
        """Check if statement contains sy-uname reference"""
        # Check fields
        if any('sy-uname' in str(v).lower() for v in fields.values()):
            return True
        
        # Check statement directly
        if self._contains_sy_uname(stmt):
            return True
        
        # Check context for structures used
        if match and len(match.groups()) > 1:
            struct = match.group(2) if isinstance(match.group(2), str) else match.group(1)
            if struct in context:
                if isinstance(context[struct], dict):
                    return any(v == 'sy-uname' for v in context[struct].values())
                else:
                    return context[struct] == 'sy-uname'
        
        return False
    
    def _contains_sy_uname(self, text: str) -> bool:
        """Check if text contains sy-uname or similar"""
        if not text:
            return False
        
        text_upper = text.upper()
        return any(var in text_upper for var in ['SY-UNAME', 'SY_UNAME'])
    
    def _should_skip(self, stmt: str) -> bool:
        """Check if statement should be skipped"""
        stmt_upper = stmt.upper()
        return any(skip in stmt_upper for skip in self.skip_patterns)
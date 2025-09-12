"""
System Variable Extractor for ABAP Code
Extracts all system variables (SY-*) and their usage context
"""

import re
from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass
from enum import Enum

class UsageContext(Enum):
    """Context where system variable is used"""
    ASSIGNMENT = "Assignment"           # lv_user = sy-uname
    DATABASE_INSERT = "Database Insert" # INSERT with sy-uname
    DATABASE_UPDATE = "Database Update" # UPDATE SET field = sy-uname
    DATABASE_MODIFY = "Database Modify" # MODIFY with sy-uname
    DATABASE_SELECT = "Database Select" # SELECT WHERE user = sy-uname
    CONDITION = "Condition"            # IF sy-subrc = 0
    FUNCTION_PARAM = "Function Parameter" # CALL FUNCTION with sy-uname
    METHOD_PARAM = "Method Parameter"   # CALL METHOD with sy-datum
    STRING_TEMPLATE = "String Template" # |User: { sy-uname }|
    CALCULATION = "Calculation"        # lv_date = sy-datum + 1
    COMPARISON = "Comparison"          # WHERE date >= sy-datum
    OUTPUT = "Output"                  # WRITE sy-uname
    FORM_PARAM = "Form Parameter"      # PERFORM using sy-uname

@dataclass
class SystemVariable:
    """Represents a system variable occurrence"""
    name: str                # e.g., 'SY-UNAME', 'SY-DATUM'
    line_number: int         # Line where it appears
    context: UsageContext    # How it's being used
    statement: str          # The full statement
    target: Optional[str]   # Target variable/field if applicable
    confidence: float       # Detection confidence

class SystemVariableExtractor:
    """Extracts system variables from ABAP code"""
    
    def __init__(self):
        # Common ABAP system variables
        self.system_variables = {
            # User and System Info
            'SY-UNAME': 'Current user name',
            'SY-MANDT': 'Client',
            'SY-LANGU': 'Logon language',
            'SY-SYSID': 'System ID',
            'SY-HOST': 'Host name',
            
            # Date and Time
            'SY-DATUM': 'Current date',
            'SY-UZEIT': 'Current time',
            'SY-TIMLO': 'Local time',
            'SY-DATLO': 'Local date',
            'SY-ZONLO': 'Time zone',
            
            # Program Flow
            'SY-SUBRC': 'Return code',
            'SY-INDEX': 'Loop index',
            'SY-TABIX': 'Table line index',
            'SY-TFILL': 'Table entries',
            'SY-TLENG': 'Table line length',
            
            # Program Info
            'SY-REPID': 'Program name',
            'SY-CPROG': 'Calling program',
            'SY-TCODE': 'Transaction code',
            'SY-DYNNR': 'Screen number',
            
            # Database
            'SY-DBCNT': 'Database operations counter',
            'SY-FDPOS': 'Field position',
            'SY-COLNO': 'Column number',
            'SY-LINCT': 'List lines',
            
            # Messages
            'SY-MSGID': 'Message ID',
            'SY-MSGNO': 'Message number',
            'SY-MSGTY': 'Message type',
            'SY-MSGV1': 'Message variable 1',
            'SY-MSGV2': 'Message variable 2',
            'SY-MSGV3': 'Message variable 3',
            'SY-MSGV4': 'Message variable 4',
            
            # Batch Input
            'SY-BATCH': 'Background processing',
            'SY-BINPT': 'Batch input active',
            'SY-CALLD': 'CALL mode',
            
            # Others
            'SY-PAGNO': 'Page number',
            'SY-LINNO': 'Line number',
            'SY-CUCOL': 'Cursor column',
            'SY-CUROW': 'Cursor row',
            'SY-LSIND': 'List level',
            'SY-LILLI': 'List line',
            'SY-TMAXL': 'Max lines',
            'SY-TVAR0': 'Text variable 0',
            'SY-TVAR1': 'Text variable 1',
        }
        
        # Alternative syntax patterns (SY-UNAME vs SY_UNAME vs SYST-UNAME)
        self.syntax_variations = [
            r'SY-(\w+)',      # SY-UNAME
            r'SY_(\w+)',      # SY_UNAME
            r'SYST-(\w+)',    # SYST-UNAME
            r'SYST_(\w+)',    # SYST_UNAME
        ]
        
        # Context detection patterns
        self.context_patterns = {
            UsageContext.ASSIGNMENT: [
                re.compile(r'(\w+)\s*=\s*.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
                re.compile(r'MOVE\s+(SY-\w+|SY_\w+|SYST-\w+)\s+TO\s+(\w+)', re.IGNORECASE),
            ],
            UsageContext.DATABASE_INSERT: [
                re.compile(r'INSERT.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE | re.DOTALL),
            ],
            UsageContext.DATABASE_UPDATE: [
                re.compile(r'UPDATE.*?SET.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE | re.DOTALL),
            ],
            UsageContext.DATABASE_MODIFY: [
                re.compile(r'MODIFY.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE | re.DOTALL),
            ],
            UsageContext.DATABASE_SELECT: [
                re.compile(r'SELECT.*?WHERE.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE | re.DOTALL),
            ],
            UsageContext.CONDITION: [
                re.compile(r'IF\s+.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
                re.compile(r'ELSEIF\s+.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
                re.compile(r'WHILE\s+.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
                re.compile(r'CHECK\s+.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
            ],
            UsageContext.FUNCTION_PARAM: [
                re.compile(r'CALL\s+FUNCTION.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE | re.DOTALL),
            ],
            UsageContext.STRING_TEMPLATE: [
                re.compile(r'\|.*?\{\s*(SY-\w+|SY_\w+|SYST-\w+)\s*\}.*?\|', re.IGNORECASE),
            ],
            UsageContext.OUTPUT: [
                re.compile(r'WRITE:?\s+.*?(SY-\w+|SY_\w+|SYST-\w+)', re.IGNORECASE),
            ],
        }
    
    def extract_all(self, lines: List[str]) -> List[SystemVariable]:
        """Extract all system variables from code"""
        results = []
        
        for line_num, line in enumerate(lines, 1):
            # Skip comments
            if line.strip().startswith('*'):
                continue
            
            # Find all system variables in line
            sys_vars = self._find_system_variables(line)
            
            for var_name in sys_vars:
                context, target = self._determine_context(line, var_name)
                
                results.append(SystemVariable(
                    name=var_name.upper(),
                    line_number=line_num,
                    context=context,
                    statement=line.strip(),
                    target=target,
                    confidence=0.95 if context != UsageContext.CONDITION else 0.85
                ))
        
        return results
    
    def extract_specific(self, lines: List[str], target_vars: List[str]) -> List[SystemVariable]:
        """Extract specific system variables only"""
        results = []
        target_set = {v.upper() for v in target_vars}
        
        all_vars = self.extract_all(lines)
        
        for var in all_vars:
            # Check if it matches target variables
            var_base = var.name.replace('SY-', '').replace('SY_', '').replace('SYST-', '')
            if var.name in target_set or f"SY-{var_base}" in target_set:
                results.append(var)
        
        return results
    
    def get_usage_summary(self, lines: List[str]) -> Dict[str, Dict]:
        """Get summary of all system variable usage"""
        all_vars = self.extract_all(lines)
        summary = {}
        
        for var in all_vars:
            if var.name not in summary:
                summary[var.name] = {
                    'count': 0,
                    'contexts': set(),
                    'lines': [],
                    'description': self.system_variables.get(var.name, 'Unknown system variable')
                }
            
            summary[var.name]['count'] += 1
            summary[var.name]['contexts'].add(var.context.value)
            summary[var.name]['lines'].append(var.line_number)
        
        # Convert sets to lists for JSON serialization
        for var_name in summary:
            summary[var_name]['contexts'] = list(summary[var_name]['contexts'])
        
        return summary
    
    def find_database_impacts(self, lines: List[str]) -> List[SystemVariable]:
        """Find system variables used in database operations"""
        all_vars = self.extract_all(lines)
        
        db_contexts = {
            UsageContext.DATABASE_INSERT,
            UsageContext.DATABASE_UPDATE,
            UsageContext.DATABASE_MODIFY,
            UsageContext.DATABASE_SELECT
        }
        
        return [var for var in all_vars if var.context in db_contexts]
    
    def track_variable_flow(self, lines: List[str], sys_var: str) -> Dict[str, List]:
        """Track how a system variable flows through the code"""
        flow = {
            'assignments': [],  # Variables that receive the value
            'usage': [],       # Where the value is used
            'database': [],    # Database operations
            'functions': []    # Function/method calls
        }
        
        # First, find direct usage
        direct_vars = self.extract_specific(lines, [sys_var])
        
        # Track assignments
        assigned_vars = set()
        for var in direct_vars:
            if var.context == UsageContext.ASSIGNMENT and var.target:
                assigned_vars.add(var.target)
                flow['assignments'].append({
                    'line': var.line_number,
                    'variable': var.target,
                    'statement': var.statement
                })
        
        # Now track where assigned variables are used
        for line_num, line in enumerate(lines, 1):
            for assigned_var in assigned_vars:
                if assigned_var in line:
                    # Determine usage
                    if any(kw in line.upper() for kw in ['INSERT', 'UPDATE', 'MODIFY', 'DELETE']):
                        flow['database'].append({
                            'line': line_num,
                            'variable': assigned_var,
                            'statement': line.strip()
                        })
                    elif 'CALL FUNCTION' in line.upper():
                        flow['functions'].append({
                            'line': line_num,
                            'variable': assigned_var,
                            'statement': line.strip()
                        })
                    else:
                        flow['usage'].append({
                            'line': line_num,
                            'variable': assigned_var,
                            'statement': line.strip()
                        })
        
        return flow
    
    def _find_system_variables(self, line: str) -> Set[str]:
        """Find all system variables in a line"""
        found = set()
        
        for pattern in self.syntax_variations:
            matches = re.finditer(pattern, line, re.IGNORECASE)
            for match in matches:
                # Reconstruct the full system variable name
                if 'SYST' in pattern:
                    full_name = f"SYST-{match.group(1)}"
                else:
                    full_name = f"SY-{match.group(1)}"
                
                # Check if it's a known system variable
                if full_name.upper() in self.system_variables or \
                   f"SY-{match.group(1).upper()}" in self.system_variables:
                    found.add(full_name)
        
        return found
    
    def _determine_context(self, line: str, var_name: str) -> Tuple[UsageContext, Optional[str]]:
        """Determine the context of system variable usage"""
        line_upper = line.upper()
        
        # Check each context pattern
        for context, patterns in self.context_patterns.items():
            for pattern in patterns:
                if pattern.search(line):
                    # Try to extract target
                    target = self._extract_target(line, var_name, context)
                    return context, target
        
        # Default context based on keywords
        if 'INSERT' in line_upper:
            return UsageContext.DATABASE_INSERT, None
        elif 'UPDATE' in line_upper:
            return UsageContext.DATABASE_UPDATE, None
        elif 'MODIFY' in line_upper:
            return UsageContext.DATABASE_MODIFY, None
        elif 'SELECT' in line_upper:
            return UsageContext.DATABASE_SELECT, None
        elif '=' in line:
            return UsageContext.ASSIGNMENT, self._extract_assignment_target(line, var_name)
        elif 'WRITE' in line_upper:
            return UsageContext.OUTPUT, None
        elif any(kw in line_upper for kw in ['IF', 'WHILE', 'CHECK']):
            return UsageContext.CONDITION, None
        
        return UsageContext.ASSIGNMENT, None
    
    def _extract_target(self, line: str, var_name: str, context: UsageContext) -> Optional[str]:
        """Extract target variable or field"""
        if context == UsageContext.ASSIGNMENT:
            return self._extract_assignment_target(line, var_name)
        elif context in [UsageContext.DATABASE_UPDATE, UsageContext.DATABASE_INSERT]:
            # Try to extract field name
            match = re.search(r'(\w+)\s*=\s*' + re.escape(var_name), line, re.IGNORECASE)
            if match:
                return match.group(1)
        
        return None
    
    def _extract_assignment_target(self, line: str, var_name: str) -> Optional[str]:
        """Extract target variable from assignment"""
        # Pattern: target = sy-uname
        match = re.search(r'(\w+)\s*=\s*.*?' + re.escape(var_name), line, re.IGNORECASE)
        if match:
            return match.group(1)
        
        # Pattern: MOVE sy-uname TO target
        match = re.search(r'MOVE\s+' + re.escape(var_name) + r'\s+TO\s+(\w+)', line, re.IGNORECASE)
        if match:
            return match.group(1)
        
        return None


# Example usage and testing
if __name__ == "__main__":
    # Test code
    test_code = """
    DATA: lv_user TYPE sy-uname,
          lv_date TYPE sy-datum,
          lv_time TYPE sy-uzeit.
    
    * Get current user
    lv_user = sy-uname.
    lv_date = sy-datum.
    lv_time = sy-uzeit.
    
    * Check return code
    IF sy-subrc = 0.
      WRITE: / 'Success'.
    ENDIF.
    
    * Database operations
    INSERT ztable FROM VALUE #( 
        created_by = sy-uname
        created_date = sy-datum
        created_time = sy-uzeit ).
    
    UPDATE ztable SET changed_by = sy-uname
                      changed_date = sy-datum
                  WHERE id = '001'.
    
    * Function call
    CALL FUNCTION 'Z_PROCESS'
      EXPORTING
        user = sy-uname
        date = sy-datum.
    
    * String template
    DATA(lv_text) = |Current user: { sy-uname } on { sy-datum }|.
    
    * Loop with index
    LOOP AT lt_table INTO ls_table.
      WRITE: / sy-tabix, ls_table-name.
    ENDLOOP.
    
    * Select with client
    SELECT * FROM ztable
      WHERE client = sy-mandt
        AND created_by = sy-uname.
    """.split('\n')
    
    extractor = SystemVariableExtractor()
    
    print("=== All System Variables ===")
    all_vars = extractor.extract_all(test_code)
    for var in all_vars:
        print(f"Line {var.line_number:3}: {var.name:12} - {var.context.value:20} - {var.statement[:50]}")
    
    print("\n=== Usage Summary ===")
    summary = extractor.get_usage_summary(test_code)
    for var_name, info in summary.items():
        print(f"{var_name}: {info['count']} occurrences in {info['contexts']}")
    
    print("\n=== Database Impacts ===")
    db_vars = extractor.find_database_impacts(test_code)
    for var in db_vars:
        print(f"Line {var.line_number}: {var.name} in {var.context.value}")
    
    print("\n=== SY-UNAME Flow ===")
    flow = extractor.track_variable_flow(test_code, 'SY-UNAME')
    print(f"Assignments: {len(flow['assignments'])}")
    print(f"Database operations: {len(flow['database'])}")
    print(f"Function calls: {len(flow['functions'])}")
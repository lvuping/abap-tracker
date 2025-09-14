#!/usr/bin/env python3
"""
Advanced ABAP Pattern Detection Module
Handles complex patterns: EXEC SQL, CDS Views, AMDP, Authority Checks, etc.
"""

import re
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass
from enum import Enum


class AdvancedOperationType(Enum):
    """Extended operation types for advanced patterns"""
    EXEC_SQL = "EXEC_SQL"
    CDS_VIEW = "CDS_VIEW"
    AMDP = "AMDP"
    AUTHORITY_CHECK = "AUTHORITY_CHECK"
    BAPI_CALL = "BAPI_CALL"
    BDC_DATA = "BDC_DATA"
    OPEN_SQL_JOIN = "OPEN_SQL_JOIN"
    DYNAMIC_SQL = "DYNAMIC_SQL"
    FIELD_SYMBOL_SELECT = "FIELD_SYMBOL_SELECT"


@dataclass
class AdvancedPattern:
    """Advanced pattern definition"""
    pattern_id: str
    operation_type: AdvancedOperationType
    regex_pattern: str
    description: str
    confidence_score: float
    requires_context: bool = False


class AdvancedPatternDetector:
    """Detects advanced ABAP patterns that standard handlers might miss"""

    def __init__(self):
        self.patterns = self._initialize_patterns()
        self.compiled_patterns = {}
        self._compile_patterns()

    def _initialize_patterns(self) -> List[AdvancedPattern]:
        """Initialize advanced pattern definitions"""
        return [
            # EXEC SQL Patterns
            AdvancedPattern(
                "EXEC_SQL_01",
                AdvancedOperationType.EXEC_SQL,
                r'EXEC\s+SQL(?:\s+PERFORMING\s+\w+)?\s*\.(.*?)ENDEXEC',
                "Native SQL execution block",
                0.95,
                True
            ),
            AdvancedPattern(
                "EXEC_SQL_02",
                AdvancedOperationType.EXEC_SQL,
                r'EXEC\s+SQL\s*:\s*INSERT\s+INTO\s+(\w+).*?VALUES.*?\((.*?)\)',
                "Native SQL INSERT with VALUES",
                0.98,
                False
            ),
            AdvancedPattern(
                "EXEC_SQL_03",
                AdvancedOperationType.EXEC_SQL,
                r'EXEC\s+SQL\s*:\s*UPDATE\s+(\w+)\s+SET\s+(.*?)(?:WHERE|ENDEXEC)',
                "Native SQL UPDATE",
                0.98,
                False
            ),

            # CDS View Patterns
            AdvancedPattern(
                "CDS_01",
                AdvancedOperationType.CDS_VIEW,
                r'SELECT.*?FROM\s+@?(\w+)\s*\(\s*\)',
                "CDS View selection",
                0.90,
                False
            ),
            AdvancedPattern(
                "CDS_02",
                AdvancedOperationType.CDS_VIEW,
                r'INSERT\s+@?(\w+)\s*\(\s*.*?\)\s*FROM',
                "CDS View based INSERT",
                0.92,
                True
            ),

            # AMDP (ABAP Managed Database Procedures)
            AdvancedPattern(
                "AMDP_01",
                AdvancedOperationType.AMDP,
                r'CALL\s+DATABASE\s+PROCEDURE\s+(\w+)',
                "AMDP procedure call",
                0.88,
                True
            ),
            AdvancedPattern(
                "AMDP_02",
                AdvancedOperationType.AMDP,
                r'METHOD\s+(\w+)\s+BY\s+DATABASE\s+PROCEDURE',
                "AMDP method implementation",
                0.85,
                True
            ),

            # Authority Check Patterns
            AdvancedPattern(
                "AUTH_01",
                AdvancedOperationType.AUTHORITY_CHECK,
                r'AUTHORITY-CHECK\s+OBJECT\s+[\'"](\w+)[\'"].*?ID\s+[\'"](\w+)[\'"].*?FIELD\s+(.*?)(?:\.|ID)',
                "Authority check with SY-UNAME",
                0.95,
                False
            ),

            # BAPI/RFC Enhanced Patterns
            AdvancedPattern(
                "BAPI_01",
                AdvancedOperationType.BAPI_CALL,
                r'CALL\s+FUNCTION\s+[\'"](\w*USER\w*)[\'"].*?EXPORTING(.*?)(?:IMPORTING|TABLES|EXCEPTIONS|\.)',
                "BAPI call with USER in name",
                0.93,
                True
            ),
            AdvancedPattern(
                "BAPI_02",
                AdvancedOperationType.BAPI_CALL,
                r'CALL\s+FUNCTION\s+[\'"](\w*CREATE\w*)[\'"].*?DESTINATION',
                "Remote BAPI CREATE operation",
                0.90,
                True
            ),

            # BDC Data Patterns
            AdvancedPattern(
                "BDC_01",
                AdvancedOperationType.BDC_DATA,
                r'BDC_DATA-FVAL\s*=\s*(.*?)(?:\.|$)',
                "BDC field value assignment",
                0.85,
                False
            ),
            AdvancedPattern(
                "BDC_02",
                AdvancedOperationType.BDC_DATA,
                r'APPEND\s+(\w+)\s+TO\s+(\w*BDC\w*)',
                "BDC data append",
                0.87,
                False
            ),

            # Complex JOIN Patterns
            AdvancedPattern(
                "JOIN_01",
                AdvancedOperationType.OPEN_SQL_JOIN,
                r'SELECT.*?FROM\s+(\w+).*?(?:INNER|LEFT|RIGHT|OUTER)\s+JOIN\s+(\w+).*?ON(.*?)(?:WHERE|INTO)',
                "Complex SQL JOIN",
                0.92,
                True
            ),
            AdvancedPattern(
                "JOIN_02",
                AdvancedOperationType.OPEN_SQL_JOIN,
                r'UPDATE\s+\((.*?JOIN.*?)\)\s+SET',
                "UPDATE with JOIN",
                0.94,
                True
            ),

            # Dynamic SQL Patterns
            AdvancedPattern(
                "DYN_SQL_01",
                AdvancedOperationType.DYNAMIC_SQL,
                r'EXECUTE\s+IMMEDIATE\s+(.*?)(?:INTO|USING|\.)',
                "Dynamic SQL execution",
                0.88,
                False
            ),
            AdvancedPattern(
                "DYN_SQL_02",
                AdvancedOperationType.DYNAMIC_SQL,
                r'INSERT\s+\((.*?)\)\s+FROM\s+TABLE\s+@?(\w+)',
                "Dynamic table INSERT",
                0.90,
                False
            ),
            AdvancedPattern(
                "DYN_SQL_03",
                AdvancedOperationType.DYNAMIC_SQL,
                r'UPDATE\s+\((.*?)\)\s+FROM',
                "Dynamic table UPDATE",
                0.90,
                False
            ),

            # Field Symbol SELECT Patterns
            AdvancedPattern(
                "FS_SEL_01",
                AdvancedOperationType.FIELD_SYMBOL_SELECT,
                r'SELECT.*?INTO\s+(?:CORRESPONDING\s+FIELDS\s+OF\s+)?<(\w+)>',
                "SELECT INTO field symbol",
                0.91,
                False
            ),
            AdvancedPattern(
                "FS_SEL_02",
                AdvancedOperationType.FIELD_SYMBOL_SELECT,
                r'INSERT\s+<(\w+)>\s+INTO\s+TABLE\s+(\w+)',
                "INSERT field symbol into table",
                0.92,
                False
            ),
            AdvancedPattern(
                "FS_SEL_03",
                AdvancedOperationType.FIELD_SYMBOL_SELECT,
                r'MODIFY\s+(\w+)\s+FROM\s+<(\w+)>',
                "MODIFY from field symbol",
                0.92,
                False
            ),
        ]

    def _compile_patterns(self):
        """Pre-compile regex patterns for performance"""
        for pattern in self.patterns:
            self.compiled_patterns[pattern.pattern_id] = re.compile(
                pattern.regex_pattern,
                re.IGNORECASE | re.DOTALL | re.MULTILINE
            )

    def detect_patterns(self, code: str, context: Dict = None) -> List[Dict]:
        """
        Detect advanced patterns in ABAP code

        Args:
            code: ABAP code to analyze
            context: Optional context dictionary with tainted variables

        Returns:
            List of detected patterns with details
        """
        detections = []

        for pattern in self.patterns:
            regex = self.compiled_patterns[pattern.pattern_id]

            for match in regex.finditer(code):
                detection = {
                    'pattern_id': pattern.pattern_id,
                    'operation_type': pattern.operation_type.value,
                    'description': pattern.description,
                    'confidence': pattern.confidence_score,
                    'match_text': match.group(0),
                    'groups': match.groups(),
                    'position': match.span(),
                    'has_sy_uname': False
                }

                # Check for SY-UNAME in the matched text
                if self._contains_sy_uname(match.group(0)):
                    detection['has_sy_uname'] = True
                    detection['confidence'] = min(1.0, detection['confidence'] * 1.1)

                # Check context if provided
                if context and pattern.requires_context:
                    detection['context_analysis'] = self._analyze_context(
                        match.groups(), context
                    )

                detections.append(detection)

        return detections

    def _contains_sy_uname(self, text: str) -> bool:
        """Check if text contains SY-UNAME or variants"""
        if not text:
            return False
        text_upper = text.upper()
        return any(var in text_upper for var in ['SY-UNAME', 'SY_UNAME', 'SYUNAME'])

    def _analyze_context(self, groups: Tuple, context: Dict) -> Dict:
        """Analyze pattern groups against context"""
        analysis = {
            'tainted_variables_found': [],
            'confidence_adjustment': 0
        }

        if not context:
            return analysis

        for group in groups:
            if group and isinstance(group, str):
                # Check each word in the group
                for word in re.findall(r'\w+', group):
                    word_upper = word.upper()
                    if word_upper in context:
                        analysis['tainted_variables_found'].append(word_upper)
                        analysis['confidence_adjustment'] += 0.05

        return analysis

    def detect_authority_checks(self, code: str) -> List[Dict]:
        """Specialized detection for authority checks with SY-UNAME"""
        pattern = re.compile(
            r'AUTHORITY-CHECK\s+OBJECT\s+[\'"](\w+)[\'"](.+?)(?:IF\s+SY-SUBRC|\.)',
            re.IGNORECASE | re.DOTALL
        )

        checks = []
        for match in pattern.finditer(code):
            object_name = match.group(1)
            check_body = match.group(2)

            # Look for SY-UNAME in the check
            if self._contains_sy_uname(check_body):
                checks.append({
                    'object': object_name,
                    'check_text': match.group(0),
                    'uses_sy_uname': True,
                    'type': 'AUTHORITY_CHECK'
                })

        return checks

    def detect_bapi_calls(self, code: str, track_params: bool = True) -> List[Dict]:
        """Enhanced BAPI/RFC call detection"""
        pattern = re.compile(
            r'CALL\s+FUNCTION\s+[\'"]([^\'\"]+)[\'"]'
            r'(?:\s+DESTINATION\s+[\'"]([^\'\"]+)[\'"])?'
            r'(.*?)(?:CALL\s+FUNCTION|ENDFUNCTION|\.\s*$)',
            re.IGNORECASE | re.DOTALL
        )

        bapi_calls = []
        for match in pattern.finditer(code):
            function_name = match.group(1)
            destination = match.group(2) or 'LOCAL'
            parameters = match.group(3)

            call_info = {
                'function': function_name,
                'destination': destination,
                'has_sy_uname': False,
                'parameters': []
            }

            if track_params:
                # Extract EXPORTING parameters
                export_pattern = re.compile(
                    r'EXPORTING\s+(.*?)(?:IMPORTING|TABLES|CHANGING|EXCEPTIONS|\.)',
                    re.IGNORECASE | re.DOTALL
                )
                export_match = export_pattern.search(parameters)

                if export_match:
                    param_text = export_match.group(1)
                    # Check for SY-UNAME in parameters
                    if self._contains_sy_uname(param_text):
                        call_info['has_sy_uname'] = True

                    # Parse individual parameters
                    param_pattern = re.compile(r'(\w+)\s*=\s*([^\s,]+)')
                    for param_match in param_pattern.finditer(param_text):
                        call_info['parameters'].append({
                            'name': param_match.group(1),
                            'value': param_match.group(2)
                        })

            bapi_calls.append(call_info)

        return bapi_calls

    def detect_native_sql(self, code: str) -> List[Dict]:
        """Detect EXEC SQL blocks with operations"""
        pattern = re.compile(
            r'EXEC\s+SQL\s*(.*?)ENDEXEC',
            re.IGNORECASE | re.DOTALL
        )

        native_sql = []
        for match in pattern.finditer(code):
            sql_content = match.group(1)

            sql_info = {
                'type': 'EXEC_SQL',
                'content': sql_content,
                'operations': [],
                'has_sy_uname': self._contains_sy_uname(sql_content)
            }

            # Detect operations within EXEC SQL
            if re.search(r'\bINSERT\s+INTO\s+(\w+)', sql_content, re.IGNORECASE):
                sql_info['operations'].append('INSERT')
            if re.search(r'\bUPDATE\s+(\w+)\s+SET', sql_content, re.IGNORECASE):
                sql_info['operations'].append('UPDATE')
            if re.search(r'\bDELETE\s+FROM\s+(\w+)', sql_content, re.IGNORECASE):
                sql_info['operations'].append('DELETE')

            native_sql.append(sql_info)

        return native_sql


class PatternCache:
    """Pattern compilation cache for performance optimization"""

    def __init__(self):
        self._cache = {}
        self._hit_count = {}
        self._miss_count = 0

    def get_pattern(self, pattern_str: str, flags: int = 0) -> re.Pattern:
        """Get compiled pattern from cache or compile and cache it"""
        cache_key = (pattern_str, flags)

        if cache_key in self._cache:
            self._hit_count[cache_key] = self._hit_count.get(cache_key, 0) + 1
            return self._cache[cache_key]

        # Compile and cache
        self._miss_count += 1
        compiled = re.compile(pattern_str, flags)
        self._cache[cache_key] = compiled
        return compiled

    def get_stats(self) -> Dict:
        """Get cache statistics"""
        total_hits = sum(self._hit_count.values())
        return {
            'cache_size': len(self._cache),
            'total_hits': total_hits,
            'total_misses': self._miss_count,
            'hit_rate': total_hits / (total_hits + self._miss_count) if (total_hits + self._miss_count) > 0 else 0
        }

    def clear(self):
        """Clear the cache"""
        self._cache.clear()
        self._hit_count.clear()
        self._miss_count = 0
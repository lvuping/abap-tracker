#!/usr/bin/env python3
"""
Comprehensive test script for INSERT, UPDATE, MODIFY command parsing
Tests all variations and edge cases
"""

import sys
import os
import re
from pathlib import Path
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass
try:
    from colorama import init, Fore, Style
    init()
    HAS_COLOR = True
except ImportError:
    # Fallback if colorama is not available
    HAS_COLOR = False
    class Fore:
        GREEN = ''
        RED = ''
        YELLOW = ''
        CYAN = ''
        RESET = ''
    class Style:
        RESET_ALL = ''

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.patterns.insert_values_handler import InsertValuesHandler
from src.utils.multiline_handler import MultilineStatementHandler
from src.core.analyzer import CompleteAnalyzer
from src.patterns.enhanced_insert_handler import EnhancedInsertHandler
from src.patterns.enhanced_update_handler import EnhancedUpdateHandler
from src.patterns.enhanced_modify_handler import EnhancedModifyHandler
from src.core.db_handler import CompleteDBHandler

@dataclass
class TestCase:
    """Represents a test case"""
    test_id: str
    description: str
    operation: str  # INSERT, UPDATE, MODIFY
    expected_table: str
    expected_field: str
    line_range: Tuple[int, int]  # Start and end line numbers
    pattern_type: str  # Type of pattern being tested


class DatabaseOperationsTest:
    """Comprehensive tester for database operations"""
    
    def __init__(self):
        self.insert_handler = InsertValuesHandler()
        self.enhanced_insert_handler = EnhancedInsertHandler()
        self.multiline_handler = MultilineStatementHandler()
        self.enhanced_update_handler = EnhancedUpdateHandler()
        self.enhanced_modify_handler = EnhancedModifyHandler()
        self.comprehensive_handler = CompleteDBHandler()
        self.complete_db_handler = CompleteDBHandler()  # Add the fixed handler
        self.complete_analyzer = CompleteAnalyzer()
        self.test_cases = []
        self.results = []
        
    def load_test_file(self, filepath: str) -> List[str]:
        """Load test ABAP file"""
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.readlines()
    
    def define_test_cases(self) -> List[TestCase]:
        """Define all test cases based on the comprehensive test file"""
        test_cases = [
            # INSERT Tests
            TestCase("T01", "Basic INSERT with structure", "INSERT", "ZTABLE", "CREATED_BY", (19, 21), "structure"),
            TestCase("T02", "INSERT with VALUES clause - single line", "INSERT", "ZTABLE", "USER", (36, 36), "values"),
            TestCase("T03", "INSERT with VALUES - multiple values", "INSERT", "ZTABLE", "USER", (41, 43), "multi_values"),
            TestCase("T04", "INSERT with VALUE constructor", "INSERT", "ZTABLE", "CREATED_BY", (58, 58), "value_constructor"),
            TestCase("T05", "INSERT VALUE constructor multiline", "INSERT", "ZTABLE", "CREATED_BY", (63, 63), "value_constructor"),
            TestCase("T06", "INSERT FROM TABLE", "INSERT", "ZTABLE", "CREATED_BY", (48, 48), "from_table"),
            TestCase("T07", "INSERT ACCEPTING DUPLICATE KEYS", "INSERT", "ZTABLE", "CREATED_BY", (53, 53), "accepting_keys"),
            TestCase("T08", "INSERT with field symbols", "INSERT", "ZTABLE", "CREATED_BY", (102, 103), "field_symbol"),
            TestCase("T09", "INSERT with inline declaration", "INSERT", "ZTABLE", "CREATED_BY", (109, 110), "inline"),
            TestCase("T10", "INSERT complex VALUE constructor", "INSERT", "ZTABLE", "CREATED_BY", (68, 70), "complex_value"),
            
            # UPDATE Tests
            TestCase("T11", "Basic UPDATE with SET", "UPDATE", "ZTABLE", "CHANGED_BY", (175, 177), "basic_set"),
            TestCase("T12", "UPDATE multiple SET fields", "UPDATE", "ZTABLE", "CHANGED_BY", (186, 190), "multi_set"),
            TestCase("T13", "UPDATE multiline SET", "UPDATE", "ZTABLE", "CHANGED_BY", (193, 199), "multiline_set"),
            TestCase("T14", "UPDATE complex WHERE", "UPDATE", "ZUSER_TABLE", "LAST_MODIFIED_BY", (202, 205), "complex_where"),
            TestCase("T15", "UPDATE FROM structure", "UPDATE", "ZTABLE", "CHANGED_BY", (202, 208), "from_structure"),
            TestCase("T16", "UPDATE with subquery", "UPDATE", "ZTABLE", "PROCESSOR", (213, 217), "subquery"),
            TestCase("T17", "UPDATE CLIENT SPECIFIED", "UPDATE", "ZTABLE", "CHANGED_BY", (220, 224), "client_specified"),
            TestCase("T18", "UPDATE with calculations", "UPDATE", "ZTABLE", "UPDATED_BY", (227, 231), "calculations"),
            TestCase("T19", "UPDATE with CASE", "UPDATE", "ZTABLE", "PROCESSOR", (234, 242), "case_statement"),
            TestCase("T20", "UPDATE nested parentheses", "UPDATE", "ZTABLE", "CHANGED_BY", (245, 249), "nested_parens"),
            
            # MODIFY Tests
            TestCase("T21", "Basic MODIFY from structure", "MODIFY", "ZTABLE", "MODIFIED_BY", (316, 317), "basic"),
            TestCase("T22", "MODIFY FROM TABLE", "MODIFY", "ZTABLE", "MODIFIED_BY", (327, 327), "from_table"),
            TestCase("T23", "MODIFY with TRANSPORTING", "MODIFY", "ZTABLE", "MODIFIED_BY", (326, 329), "transporting"),
            TestCase("T24", "MODIFY internal table", "MODIFY", "LT_RECORDS", "PROCESSOR", (332, 336), "internal_table"),
            TestCase("T25", "MODIFY WHERE condition", "MODIFY", "LT_RECORDS", "PROCESSOR", (339, 342), "where_condition"),
            TestCase("T26", "MODIFY TABLE sorted", "MODIFY", "LT_SORTED", "MODIFIED_BY", (345, 348), "sorted_table"),
            TestCase("T27", "MODIFY from internal table", "MODIFY", "ZTABLE", "MODIFIED_BY", (351, 353), "from_itab"),
            TestCase("T28", "MODIFY VALUE constructor", "MODIFY", "ZTABLE", "MODIFIED_BY", (356, 358), "value_constructor"),
            TestCase("T29", "MODIFY inline work area", "MODIFY", "ZTABLE", "MODIFIED_BY", (361, 363), "inline"),
            TestCase("T30", "MODIFY LOOP field symbols", "MODIFY", "LT_TAB", "MODIFIED_BY", (396, 398), "loop_fs"),

            # Special Cases - These test cases are commented as they don't match current ABAP file
            # TestCase("T31", "Variable far from usage", "INSERT", "ZTABLE", "CREATED_BY", (200, 214), "far_variable"),
            # TestCase("T32", "Multiple system variables", "INSERT", "ZLOG_TABLE", "USER", (216, 223), "multi_sysvars"),
            # TestCase("T33", "Nested structures", "INSERT", "ZTABLE", "CREATED_BY", (225, 232), "nested"),
            # TestCase("T34", "Chain statements", "INSERT", "ZTABLE", "CREATED_BY", (234, 237), "chain"),
            # TestCase("T35", "UPDATE with JOIN pattern", "UPDATE", "ZTABLE", "CHANGED_BY", (239, 243), "join_like"),
            # TestCase("T36", "Conditional INSERT", "INSERT", "ZTABLE", "CREATED_BY", (245, 253), "conditional"),
            # TestCase("T37", "INSERT in FORM", "INSERT", "ZTABLE", "CREATED_BY", (255, 261), "form_routine"),
            # TestCase("T38", "Complex multiline comments", "UPDATE", "ZTABLE", "CHANGED_BY", (263, 270), "with_comments"),
            # TestCase("T39", "INSERT conversion exit", "INSERT", "ZTABLE", "CREATED_BY", (272, 276), "conversion"),
            # TestCase("T40", "MODIFY dynamic table", "MODIFY", "ZTABLE", "MODIFIED_BY", (278, 282), "dynamic"),

            # Modern ABAP - These test cases are commented as they don't match current ABAP file
            # TestCase("T44", "INSERT CORRESPONDING", "INSERT", "ZTABLE", "CREATED_BY", (296, 299), "corresponding"),
            # TestCase("T45", "UPDATE inline SQL", "UPDATE", "ZTABLE", "CHANGED_BY", (301, 303), "inline_sql"),
            # TestCase("T46", "MODIFY with REDUCE", "MODIFY", "ZTABLE", "MODIFIED_BY", (305, 311), "reduce"),
            # TestCase("T47", "INSERT with FILTER", "INSERT", "ZTABLE", "CREATED_BY", (313, 315), "filter"),
            # TestCase("T48", "UPDATE string templates", "UPDATE", "ZTABLE", "CHANGED_BY", (317, 320), "string_template"),
            # TestCase("T49", "MODIFY with COND", "MODIFY", "ZTABLE", "MODIFIED_BY", (322, 326), "cond"),
            # TestCase("T50", "Batch operations", "INSERT", "ZTABLE", "CREATED_BY", (328, 335), "batch"),
        ]
        return test_cases
    
    def test_insert_operation(self, lines: List[str], test_case: TestCase) -> Dict:
        """Test INSERT operation parsing"""
        # Extract relevant lines with context (include a few lines before for variable tracking)
        start_idx = max(0, test_case.line_range[0] - 5)  # Include 5 lines before for context
        end_idx = test_case.line_range[1]
        code_snippet = lines[start_idx:end_idx]

        # Use the fixed complete_db_handler
        complete_results = self.complete_db_handler.analyze(code_snippet)

        result = {
            'found': False,
            'table': None,
            'fields': [],
            'has_sy_uname': False,
            'details': None
        }

        # Use complete handler results
        if complete_results:
            op = complete_results[0]
            result['found'] = True
            result['table'] = op.table
            result['fields'] = list(op.fields.keys())
            result['has_sy_uname'] = op.has_sy_uname
            result['details'] = f"Pattern type: {op.pattern}, Confidence: {op.confidence}"
        else:
            # Fallback to enhanced handler
            enhanced_patterns = self.enhanced_insert_handler.analyze_insert(code_snippet, start_idx)

            if enhanced_patterns:
                pattern = enhanced_patterns[0]
                result['found'] = True
                result['table'] = pattern.table
                result['fields'] = list(pattern.fields.keys())

                # Check for sy-uname
                for field, value in pattern.fields.items():
                    if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                        result['has_sy_uname'] = True
                        break

                result['details'] = f"Pattern type: {pattern.pattern_type}, Confidence: {pattern.confidence}"

        return result
    
    def test_update_operation(self, lines: List[str], test_case: TestCase) -> Dict:
        """Test UPDATE operation parsing"""
        start_idx = test_case.line_range[0] - 1
        end_idx = test_case.line_range[1]
        code_snippet = lines[start_idx:end_idx]
        
        # Try enhanced handler first
        enhanced_patterns = self.enhanced_update_handler.analyze_update(code_snippet, start_idx)
        
        result = {
            'found': False,
            'table': None,
            'fields': [],
            'has_sy_uname': False,
            'details': None
        }
        
        if enhanced_patterns:
            pattern = enhanced_patterns[0]
            result['found'] = True
            result['table'] = pattern.table
            result['fields'] = list(pattern.fields.keys())
            
            # Check for sy-uname
            for field, value in pattern.fields.items():
                if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                    result['has_sy_uname'] = True
                    break
            
            result['details'] = f"Pattern type: {pattern.pattern_type}, Confidence: {pattern.confidence}"
        else:
            # Fallback to multiline handler
            sy_uname_line = None
            for i in range(len(code_snippet)):
                if 'sy-uname' in code_snippet[i].lower():
                    sy_uname_line = i
                    break
            
            if sy_uname_line is None:
                sy_uname_line = 0
            
            multiline = self.multiline_handler.analyze_multiline_context(code_snippet, sy_uname_line)
            
            if multiline and multiline.statement_type == 'UPDATE':
                result['found'] = True
                result['table'] = multiline.table
                result['fields'] = list(multiline.fields.keys())
                
                # Check for sy-uname in fields
                for field, value in multiline.fields.items():
                    if 'SY-UNAME' in value.upper():
                        result['has_sy_uname'] = True
                        break
                
                result['details'] = f"Lines {multiline.start_line}-{multiline.end_line}"
        
        return result
    
    def test_modify_operation(self, lines: List[str], test_case: TestCase) -> Dict:
        """Test MODIFY operation parsing"""
        start_idx = test_case.line_range[0] - 1
        end_idx = test_case.line_range[1]
        code_snippet = lines[start_idx:end_idx]
        
        # Try enhanced handler first
        enhanced_patterns = self.enhanced_modify_handler.analyze_modify(code_snippet, start_idx)
        
        result = {
            'found': False,
            'table': None,
            'fields': [],
            'has_sy_uname': False,
            'details': None
        }
        
        if enhanced_patterns:
            pattern = enhanced_patterns[0]
            result['found'] = True
            result['table'] = pattern.table
            result['fields'] = list(pattern.fields.keys())
            
            # Check for sy-uname
            for field, value in pattern.fields.items():
                if 'SY-UNAME' in value.upper() or 'SY_UNAME' in value.upper():
                    result['has_sy_uname'] = True
                    break
            
            result['details'] = f"Pattern type: {pattern.pattern_type}, Confidence: {pattern.confidence}"
        else:
            # Fallback to multiline handler
            sy_uname_line = None
            for i in range(len(code_snippet)):
                if 'sy-uname' in code_snippet[i].lower():
                    sy_uname_line = i
                    break
            
            if sy_uname_line is None:
                sy_uname_line = 0
            
            multiline = self.multiline_handler.analyze_multiline_context(code_snippet, sy_uname_line)
            
            if multiline and multiline.statement_type == 'MODIFY':
                result['found'] = True
                result['table'] = multiline.table
                result['fields'] = list(multiline.fields.keys())
                
                # Check for sy-uname in fields
                for field, value in multiline.fields.items():
                    if 'SY-UNAME' in value.upper():
                        result['has_sy_uname'] = True
                        break
                
                result['details'] = f"Lines {multiline.start_line}-{multiline.end_line}"
            
            # Also check for MODIFY patterns with simpler pattern
            if not result['found']:
                for line in code_snippet:
                    if re.search(r'\bMODIFY\b.*\bsy-uname\b', line, re.IGNORECASE):
                        result['found'] = True
                        result['has_sy_uname'] = True
                        # Try to extract table name
                        table_match = re.search(r'MODIFY\s+(\w+)', line, re.IGNORECASE)
                        if table_match:
                            result['table'] = table_match.group(1).upper()
                        break
        
        return result
    
    def run_test(self, lines: List[str], test_case: TestCase) -> Dict:
        """Run a single test case"""
        # First try comprehensive handler
        start_idx = test_case.line_range[0] - 1
        end_idx = test_case.line_range[1]
        
        ops = self.comprehensive_handler.analyze(lines, start_idx, end_idx)
        
        for op in ops:
            if op.operation.value == test_case.operation:
                return {
                    'found': True,
                    'table': op.table,
                    'fields': list(op.fields.keys()),
                    'has_sy_uname': any('sy-uname' in str(v).lower() for v in op.fields.values()),
                    'details': f"Pattern type: {op.pattern}, Confidence: {op.confidence}"
                }
        
        # Fallback to specific handlers
        if test_case.operation == 'INSERT':
            return self.test_insert_operation(lines, test_case)
        elif test_case.operation == 'UPDATE':
            return self.test_update_operation(lines, test_case)
        elif test_case.operation == 'MODIFY':
            return self.test_modify_operation(lines, test_case)
        else:
            return {'found': False, 'error': 'Unknown operation'}
    
    def print_results(self, results: List[Tuple[TestCase, Dict]]):
        """Print test results with color coding"""
        print("\n" + "="*80)
        print("TEST RESULTS - Database Operations Parser")
        print("="*80 + "\n")
        
        passed = 0
        failed = 0
        partial = 0
        
        # Group by operation type
        for op_type in ['INSERT', 'UPDATE', 'MODIFY']:
            print(f"\n{Fore.CYAN}--- {op_type} Operations ---{Style.RESET_ALL}")
            
            for test_case, result in results:
                if test_case.operation != op_type:
                    continue
                
                status = ""
                if result['found'] and result['has_sy_uname']:
                    status = f"{Fore.GREEN}✓ PASS{Style.RESET_ALL}"
                    passed += 1
                elif result['found']:
                    status = f"{Fore.YELLOW}⚠ PARTIAL{Style.RESET_ALL}"
                    partial += 1
                else:
                    status = f"{Fore.RED}✗ FAIL{Style.RESET_ALL}"
                    failed += 1
                
                print(f"{status} {test_case.test_id}: {test_case.description}")
                
                if result['found']:
                    table_info = f"Table: {result['table'] or 'NOT FOUND'}"
                    field_info = f"Fields: {', '.join(result['fields'][:3]) if result['fields'] else 'NONE'}"
                    sy_uname_info = f"SY-UNAME: {'YES' if result['has_sy_uname'] else 'NO'}"
                    print(f"    {table_info} | {field_info} | {sy_uname_info}")
                    
                    if result.get('details'):
                        print(f"    Details: {result['details']}")
                else:
                    print(f"    {Fore.RED}Operation not detected{Style.RESET_ALL}")
                    if result.get('error'):
                        print(f"    Error: {result['error']}")
        
        # Summary
        print("\n" + "="*80)
        print("SUMMARY")
        print("="*80)
        total = passed + failed + partial
        pass_rate = (passed / total * 100) if total > 0 else 0
        
        print(f"{Fore.GREEN}Passed:{Style.RESET_ALL} {passed}/{total} ({pass_rate:.1f}%)")
        print(f"{Fore.YELLOW}Partial:{Style.RESET_ALL} {partial}/{total} ({partial/total*100:.1f}%)")
        print(f"{Fore.RED}Failed:{Style.RESET_ALL} {failed}/{total} ({failed/total*100:.1f}%)")
        
        # Identify patterns that need improvement
        if failed > 0 or partial > 0:
            print(f"\n{Fore.YELLOW}Areas for Improvement:{Style.RESET_ALL}")
            failed_patterns = set()
            for test_case, result in results:
                if not result['found'] or not result['has_sy_uname']:
                    failed_patterns.add(test_case.pattern_type)
            
            for pattern in failed_patterns:
                print(f"  - {pattern}")
        
        return passed, partial, failed
    
    def run_all_tests(self, test_file: str):
        """Run all tests"""
        print(f"Loading test file: {test_file}")
        lines = self.load_test_file(test_file)
        
        print(f"Loaded {len(lines)} lines")
        
        # Get test cases
        test_cases = self.define_test_cases()
        print(f"Running {len(test_cases)} test cases...")
        
        results = []
        for test_case in test_cases:
            result = self.run_test(lines, test_case)
            results.append((test_case, result))
        
        # Print results
        passed, partial, failed = self.print_results(results)
        
        # Return success if all tests passed
        return failed == 0 and partial == 0


def main():
    """Main test runner"""
    tester = DatabaseOperationsTest()
    
    # Test file path
    test_file = "input/all_abap_patterns.abap"
    
    if not os.path.exists(test_file):
        print(f"{Fore.RED}Error: Test file not found: {test_file}{Style.RESET_ALL}")
        sys.exit(1)
    
    # Run tests
    success = tester.run_all_tests(test_file)
    
    if success:
        print(f"\n{Fore.GREEN}All tests passed successfully!{Style.RESET_ALL}")
        sys.exit(0)
    else:
        print(f"\n{Fore.YELLOW}Some tests failed or partially passed. Improvements needed.{Style.RESET_ALL}")
        sys.exit(1)


if __name__ == "__main__":
    main()
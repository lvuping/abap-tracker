#!/usr/bin/env python3
"""
Fixed Enhanced ABAP Analyzer with proper sliding window implementation
"""

import re
import time
import io
import csv
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from pathlib import Path

# Add parent directory to path
import sys
import os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from patterns.advanced_patterns import AdvancedPatternDetector, PatternCache
from utils.context_aware_taint import ContextAwareTaintTracker
from core.final_handler import FinalDBHandler
from utils.encoding_utils import safe_file_read
from core.enhanced_analyzer import AnalysisResult, SlidingWindowAnalyzer, EnhancedABAPAnalyzer


class FixedEnhancedABAPAnalyzer(EnhancedABAPAnalyzer):
    """Fixed Enhanced ABAP Analyzer with proper window analysis"""

    def analyze_file(self, file_path: str, target_lines: List[int] = None) -> List[AnalysisResult]:
        """
        Analyze ABAP file with proper sliding window context analysis

        This version properly analyzes the entire window around each target line,
        not just the target line itself.
        """
        start_time = time.time()
        results = []

        # Read file with encoding detection
        lines, encoding = safe_file_read(file_path)

        # First pass: Context-aware taint tracking on entire file
        taint_start = time.time()
        taint_analysis = self.taint_tracker.analyze_code_with_context(lines)
        self.metrics['taint_time'] += time.time() - taint_start

        # If no target lines specified, analyze entire file
        if not target_lines:
            target_lines = range(len(lines))

        # Track which lines we've already processed to avoid duplicates
        processed_operations = set()

        # Analyze each target line with its surrounding context
        for line_num in target_lines:
            if line_num >= len(lines):
                continue

            # Get the line content
            target_line_content = lines[line_num].strip().upper()

            # Skip if this line doesn't contain sy-uname
            if 'SY-UNAME' not in target_line_content:
                # But still check the window for operations
                pass

            # Define window boundaries (default 50 lines before and after)
            window_size = self.sliding_window.window_size if self.sliding_window else 50
            start_idx = max(0, line_num - window_size)
            end_idx = min(len(lines), line_num + window_size + 1)

            # Extract window
            window = lines[start_idx:end_idx]

            # Step 1: Find sy-uname assignments in the window
            tainted_vars = self._find_tainted_variables_in_window(window, line_num - start_idx)

            # Step 2: Find database operations in the window
            for i, line in enumerate(window):
                actual_line_num = start_idx + i

                # Skip if we've already processed this operation
                line_key = f"{actual_line_num}:{line.strip()[:50]}"
                if line_key in processed_operations:
                    continue

                # Check for database operations
                operation_result = self._check_for_db_operation(
                    window, i, tainted_vars, actual_line_num, file_path, taint_analysis
                )

                if operation_result:
                    processed_operations.add(line_key)
                    results.append(operation_result)

        self.metrics['total_time'] += time.time() - start_time
        return results

    def _find_tainted_variables_in_window(self, window: List[str], target_idx: int) -> set:
        """
        Find all variables that are tainted with sy-uname in the window
        """
        tainted_vars = {'SY-UNAME', 'SYUNAME', 'SY-UNAME'}

        # Patterns for sy-uname assignment
        patterns = [
            r'(\w+)\s*=\s*sy-uname',  # lv_user = sy-uname
            r'DATA:\s*(\w+)\s+TYPE\s+sy-uname',  # DATA: lv_user TYPE sy-uname
            r'(\w+)\s+TYPE\s+sy-uname',  # lv_user TYPE sy-uname
            r'MOVE\s+sy-uname\s+TO\s+(\w+)',  # MOVE sy-uname TO lv_user
            r'(\w+)-(\w+)\s*=\s*sy-uname',  # ls_struct-field = sy-uname
        ]

        for i, line in enumerate(window):
            line_upper = line.upper().strip()

            # Check for sy-uname assignments
            for pattern in patterns:
                matches = re.finditer(pattern, line, re.IGNORECASE)
                for match in matches:
                    var_name = match.group(1)
                    tainted_vars.add(var_name.upper())
                    # Also add structure field notation
                    if len(match.groups()) > 1:
                        field_name = match.group(2)
                        tainted_vars.add(f"{var_name.upper()}-{field_name.upper()}")

            # Check for variable-to-variable assignments
            for tainted_var in list(tainted_vars):
                # Pattern: lv_other = lv_tainted
                pattern = rf'(\w+)\s*=\s*{re.escape(tainted_var)}'
                matches = re.finditer(pattern, line, re.IGNORECASE)
                for match in matches:
                    new_var = match.group(1)
                    tainted_vars.add(new_var.upper())

        return tainted_vars

    def _check_for_db_operation(self, window: List[str], line_idx: int,
                                tainted_vars: set, actual_line_num: int,
                                file_path: str, taint_analysis: Dict) -> Optional[AnalysisResult]:
        """
        Check if a line contains a database operation with tainted variables
        """
        if line_idx >= len(window):
            return None

        line = window[line_idx].strip()
        line_upper = line.upper()

        # Skip comments
        if line.startswith('*') or line.startswith('"'):
            return None

        # Database operation patterns
        db_operations = ['INSERT', 'UPDATE', 'MODIFY', 'DELETE']

        # Check if line contains a database operation
        operation_found = None
        for op in db_operations:
            if op in line_upper:
                operation_found = op
                break

        if not operation_found:
            return None

        # Build complete statement (handle multi-line)
        statement = self._build_complete_statement(window, line_idx)
        statement_upper = statement.upper()

        # Check if any tainted variable is used in this operation
        tainted_found = []
        for var in tainted_vars:
            if var in statement_upper:
                tainted_found.append(var)

        if not tainted_found:
            return None

        # Extract table name
        table_name = self._extract_table_name(statement, operation_found)

        # Create result
        return AnalysisResult(
            file_path=file_path,
            line_number=actual_line_num + 1,  # Convert to 1-based
            status='Complete',
            table=table_name or "Unknown",
            fields={},  # Add fields dict
            operation=operation_found,
            confidence=0.9 if table_name else 0.7,
            tainted_variables=list(tainted_found)[:5],  # Limit to 5
            advanced_patterns=[],
            context_info={'scope': 'window_analysis'},
            performance_metrics={}  # Add performance metrics
        )

    def _build_complete_statement(self, lines: List[str], start_idx: int) -> str:
        """
        Build a complete ABAP statement, handling multi-line statements
        """
        if start_idx >= len(lines):
            return ""

        statement = lines[start_idx].strip()

        # Continue reading lines until we find a period (statement end)
        i = start_idx
        while i < len(lines) - 1 and not statement.rstrip().endswith('.'):
            i += 1
            if i < len(lines):
                next_line = lines[i].strip()
                # Skip comments
                if not next_line.startswith('*') and not next_line.startswith('"'):
                    statement += " " + next_line

        return statement

    def _extract_table_name(self, statement: str, operation: str) -> Optional[str]:
        """
        Extract table name from a database operation statement
        """
        statement_upper = statement.upper()

        # Patterns for different operations
        patterns = {
            'INSERT': [
                r'INSERT\s+INTO\s+(\w+)',
                r'INSERT\s+(\w+)',
                r'INSERT\s+INTO\s+TABLE\s+(\w+)',
            ],
            'UPDATE': [
                r'UPDATE\s+(\w+)',
                r'UPDATE\s+TABLE\s+(\w+)',
            ],
            'MODIFY': [
                r'MODIFY\s+(\w+)',
                r'MODIFY\s+TABLE\s+(\w+)',
            ],
            'DELETE': [
                r'DELETE\s+FROM\s+(\w+)',
                r'DELETE\s+(\w+)',
                r'DELETE\s+FROM\s+TABLE\s+(\w+)',
            ]
        }

        if operation in patterns:
            for pattern in patterns[operation]:
                match = re.search(pattern, statement_upper)
                if match:
                    return match.group(1)

        return None


def analyze_csv_with_fixed_analyzer(csv_path: str, output_path: str = None, verbose: bool = False) -> Tuple[List, Dict]:
    """
    Analyze CSV with the fixed enhanced analyzer
    """
    # Create fixed analyzer
    analyzer = FixedEnhancedABAPAnalyzer(
        enable_caching=True,
        enable_sliding_window=True,
        window_size=50,
        verbose=verbose
    )

    if verbose:
        print(f"\n{'='*80}")
        print(f"FIXED ENHANCED ANALYZER - ANALYZING WITH PROPER WINDOW CONTEXT")
        print(f"{'='*80}")

    # Read CSV
    from utils.encoding_utils import safe_file_read
    csv_lines, encoding = safe_file_read(csv_path)

    if verbose:
        print(f"\n📂 Loading CSV: {csv_path}")
        print(f"   ✓ CSV encoding: {encoding}")

    # Parse CSV
    csv_content = io.StringIO(''.join(csv_lines))
    reader = csv.DictReader(csv_content)
    rows = list(reader)

    if verbose:
        print(f"   ✓ Total rows: {len(rows)}")

    # Group by file
    files_to_analyze = {}
    for row in rows:
        file_path = row.get('file_path', '').strip()
        line_number_str = row.get('line_number', '').strip()

        if not file_path or not line_number_str:
            continue

        try:
            line_num = int(line_number_str) - 1  # Convert to 0-indexed
        except ValueError:
            continue

        if file_path not in files_to_analyze:
            files_to_analyze[file_path] = []
        files_to_analyze[file_path].append(line_num)

    if verbose:
        print(f"   ✓ Files to analyze: {len(files_to_analyze)}")

    # Analyze each file
    results = []
    files_processed = 0

    for file_path, line_nums in files_to_analyze.items():
        # Try file path as is, or relative to input directory
        file_to_check = Path(file_path)
        if not file_to_check.exists():
            file_to_check = Path('input') / file_path

        if file_to_check.exists():
            files_processed += 1
            if verbose and files_processed <= 5:
                print(f"\n[{files_processed}/{len(files_to_analyze)}] Processing: {file_path}")
                print(f"   Lines to check: {len(line_nums)}")

            # Analyze file with fixed analyzer
            file_results = analyzer.analyze_file(str(file_to_check), line_nums)

            if verbose and files_processed <= 5:
                print(f"   ✓ Found {len(file_results)} operations")
                if file_results:
                    # Show first result
                    first = file_results[0]
                    print(f"   Sample: {first.operation} on {first.table} at line {first.line_number}")

            results.extend(file_results)

    if verbose:
        print(f"\n{'='*80}")
        print(f"📊 FIXED ANALYZER RESULTS")
        print(f"{'='*80}")
        print(f"   ✓ Files processed: {files_processed}/{len(files_to_analyze)}")
        print(f"   ✓ Total operations found: {len(results)}")

        if results:
            # Group by operation
            ops = {}
            for r in results:
                ops[r.operation] = ops.get(r.operation, 0) + 1
            print(f"\n   Operations breakdown:")
            for op, count in sorted(ops.items()):
                print(f"     • {op}: {count}")

    # Save results if output path provided
    if output_path:
        import json
        with open(output_path, 'w', encoding='utf-8') as f:
            json_data = [asdict(r) if hasattr(r, '__dict__') else r for r in results]
            json.dump(json_data, f, indent=2, ensure_ascii=False)
        if verbose:
            print(f"\n💾 Results saved to: {output_path}")

    return results, analyzer.get_performance_metrics()
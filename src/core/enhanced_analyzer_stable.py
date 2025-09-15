#!/usr/bin/env python3
"""
Stable Enhanced ABAP Analyzer with Windows compatibility and error handling
"""

import re
import time
import io
import csv
import sys
import os
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from utils.encoding_utils import safe_file_read


@dataclass
class AnalysisResult:
    """Analysis result data class"""
    file_path: str
    line_number: int
    status: str
    table: Optional[str]
    fields: Dict[str, str]
    operation: Optional[str]
    confidence: float
    tainted_variables: List[str]
    advanced_patterns: List[Dict]
    context_info: Dict
    performance_metrics: Dict


class StableEnhancedAnalyzer:
    """Stable Enhanced ABAP Analyzer with proper error handling"""

    def __init__(self, window_size: int = 50, verbose: bool = False):
        self.window_size = window_size
        self.verbose = verbose
        self.metrics = {
            'total_time': 0,
            'files_processed': 0,
            'errors': 0
        }

    def analyze_csv(self, csv_path: str, output_path: str = None) -> Tuple[List[AnalysisResult], Dict]:
        """
        Analyze CSV file with robust error handling
        """
        results = []
        start_time = time.time()

        try:
            print(f"\n{'='*80}")
            print(f"ENHANCED ANALYZER - STARTING ANALYSIS")
            print(f"{'='*80}")
            print(f"📂 Loading CSV: {csv_path}")

            # Read CSV with encoding detection
            csv_lines, encoding = safe_file_read(csv_path)
            print(f"✓ CSV encoding: {encoding}")
            print(f"✓ CSV lines: {len(csv_lines)}")

            # Parse CSV
            csv_content = io.StringIO(''.join(csv_lines))
            reader = csv.DictReader(csv_content)
            rows = list(reader)
            print(f"✓ Total rows: {len(rows)}")

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

            print(f"✓ Files to analyze: {len(files_to_analyze)}")
            print(f"\n🔍 Starting analysis with window size: {self.window_size}")
            print("Processing files...")

            # Process each file
            files_processed = 0
            total_files = len(files_to_analyze)
            last_progress_time = time.time()

            for file_idx, (file_path, line_nums) in enumerate(files_to_analyze.items(), 1):
                # Show progress
                current_time = time.time()
                if file_idx % 5 == 0 or file_idx == 1 or file_idx == total_files or (current_time - last_progress_time) > 2:
                    print(f"  Progress: {file_idx}/{total_files} files ({file_idx/total_files*100:.1f}%)")
                    last_progress_time = current_time
                    # Flush output for Windows compatibility
                    sys.stdout.flush()

                # Try to find the file
                file_to_check = Path(file_path)
                if not file_to_check.exists():
                    # Try relative to input directory
                    file_to_check = Path('input') / file_path

                if file_to_check.exists():
                    try:
                        # Analyze file with timeout check
                        file_start = time.time()
                        file_results = self.analyze_file(str(file_to_check), line_nums)
                        file_time = time.time() - file_start

                        # Warn if file takes too long
                        if file_time > 5:
                            print(f"  ⚠️ Slow processing for {file_path}: {file_time:.1f}s")

                        results.extend(file_results)
                        files_processed += 1
                        self.metrics['files_processed'] = files_processed

                    except Exception as e:
                        self.metrics['errors'] += 1
                        if self.verbose:
                            print(f"  ⚠️ Error in {file_path}: {str(e)[:100]}")
                        continue

            # Final summary
            self.metrics['total_time'] = time.time() - start_time

            print(f"\n{'='*80}")
            print(f"📊 ANALYSIS COMPLETE")
            print(f"{'='*80}")
            print(f"✓ Files processed: {files_processed}/{total_files}")
            print(f"✓ Operations found: {len(results)}")
            print(f"✓ Time taken: {self.metrics['total_time']:.2f} seconds")

            if results:
                # Group by operation
                ops = {}
                for r in results:
                    ops[r.operation] = ops.get(r.operation, 0) + 1
                print(f"\nOperations breakdown:")
                for op, count in sorted(ops.items()):
                    print(f"  • {op}: {count}")

            # Save results if output path provided
            if output_path and results:
                self.save_results(results, output_path)
                print(f"\n💾 Results saved to: {output_path}")

        except Exception as e:
            print(f"\n❌ CRITICAL ERROR: {e}")
            import traceback
            if self.verbose:
                traceback.print_exc()

        return results, self.metrics

    def analyze_file(self, file_path: str, target_lines: List[int]) -> List[AnalysisResult]:
        """
        Analyze ABAP file with sliding window
        """
        results = []

        try:
            # Read file with encoding detection
            lines, encoding = safe_file_read(file_path)

            # Track processed operations to avoid duplicates
            processed_operations = set()

            # Analyze each target line with window
            for line_num in target_lines:
                if line_num >= len(lines):
                    continue

                # Define window boundaries
                start_idx = max(0, line_num - self.window_size)
                end_idx = min(len(lines), line_num + self.window_size + 1)

                # Extract window
                window = lines[start_idx:end_idx]

                # Find tainted variables in window
                tainted_vars = self.find_tainted_variables(window)

                # Find database operations in window
                for i, line in enumerate(window):
                    actual_line_num = start_idx + i

                    # Create unique key for this operation
                    line_key = f"{actual_line_num}:{line.strip()[:50]}"
                    if line_key in processed_operations:
                        continue

                    # Check for database operation
                    result = self.check_db_operation(
                        window, i, tainted_vars, actual_line_num, file_path
                    )

                    if result:
                        processed_operations.add(line_key)
                        results.append(result)

        except Exception as e:
            if self.verbose:
                print(f"    Error analyzing {file_path}: {e}")

        return results

    def find_tainted_variables(self, window: List[str]) -> set:
        """
        Find variables tainted with sy-uname
        """
        tainted = {'SY-UNAME', 'SYUNAME'}

        patterns = [
            r'(\w+)\s*=\s*sy-uname',
            r'DATA:\s*(\w+)\s+TYPE\s+sy-uname',
            r'(\w+)\s+TYPE\s+sy-uname',
            r'MOVE\s+sy-uname\s+TO\s+(\w+)',
            r'(\w+)-(\w+)\s*=\s*sy-uname',
        ]

        for line in window:
            for pattern in patterns:
                matches = re.finditer(pattern, line, re.IGNORECASE)
                for match in matches:
                    tainted.add(match.group(1).upper())

        return tainted

    def check_db_operation(self, window: List[str], line_idx: int,
                           tainted_vars: set, actual_line_num: int,
                           file_path: str) -> Optional[AnalysisResult]:
        """
        Check if line contains database operation
        """
        if line_idx >= len(window):
            return None

        line = window[line_idx].strip()
        line_upper = line.upper()

        # Skip comments
        if line.startswith('*') or line.startswith('"'):
            return None

        # Check for database operations
        db_ops = ['INSERT', 'UPDATE', 'MODIFY', 'DELETE']
        operation = None

        for op in db_ops:
            if op in line_upper:
                operation = op
                break

        if not operation:
            return None

        # Build complete statement
        statement = self.build_statement(window, line_idx)
        statement_upper = statement.upper()

        # Check if tainted variables are used
        tainted_found = []
        for var in tainted_vars:
            if var in statement_upper:
                tainted_found.append(var)

        if not tainted_found:
            return None

        # Extract table name
        table = self.extract_table(statement, operation)

        return AnalysisResult(
            file_path=file_path,
            line_number=actual_line_num + 1,
            status='Complete',
            table=table or 'Unknown',
            fields={},
            operation=operation,
            confidence=0.9 if table else 0.7,
            tainted_variables=list(tainted_found)[:5],
            advanced_patterns=[],
            context_info={'window_size': self.window_size},
            performance_metrics={}
        )

    def build_statement(self, lines: List[str], start_idx: int) -> str:
        """
        Build complete ABAP statement
        """
        if start_idx >= len(lines):
            return ""

        statement = lines[start_idx].strip()
        i = start_idx

        # Continue until period (statement end)
        while i < len(lines) - 1 and not statement.rstrip().endswith('.'):
            i += 1
            if i < len(lines):
                next_line = lines[i].strip()
                if not next_line.startswith('*') and not next_line.startswith('"'):
                    statement += " " + next_line

        return statement

    def extract_table(self, statement: str, operation: str) -> Optional[str]:
        """
        Extract table name from statement
        """
        patterns = {
            'INSERT': [r'INSERT\s+INTO\s+(\w+)', r'INSERT\s+(\w+)'],
            'UPDATE': [r'UPDATE\s+(\w+)'],
            'MODIFY': [r'MODIFY\s+(\w+)'],
            'DELETE': [r'DELETE\s+FROM\s+(\w+)', r'DELETE\s+(\w+)']
        }

        if operation in patterns:
            for pattern in patterns[operation]:
                match = re.search(pattern, statement.upper())
                if match:
                    return match.group(1)

        return None

    def save_results(self, results: List[AnalysisResult], output_path: str):
        """
        Save results to file
        """
        try:
            import json
            with open(output_path, 'w', encoding='utf-8') as f:
                json_data = [asdict(r) for r in results]
                json.dump(json_data, f, indent=2, ensure_ascii=False)
        except Exception as e:
            print(f"  ⚠️ Error saving results: {e}")


def run_stable_analysis(csv_path: str, verbose: bool = False) -> Tuple[List, Dict]:
    """
    Run analysis with stable analyzer
    """
    analyzer = StableEnhancedAnalyzer(window_size=50, verbose=verbose)
    return analyzer.analyze_csv(csv_path)
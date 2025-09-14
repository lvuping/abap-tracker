#!/usr/bin/env python3
"""
Enhanced ABAP Analyzer with Performance Optimizations
Integrates advanced patterns, context-aware taint tracking, and sliding window analysis
"""

import re
import time
from typing import List, Dict, Optional, Tuple, Any
from dataclasses import dataclass, asdict
from pathlib import Path

# Import enhanced modules
from patterns.advanced_patterns import AdvancedPatternDetector, PatternCache
from utils.context_aware_taint import ContextAwareTaintTracker
from core.final_handler import FinalDBHandler


@dataclass
class AnalysisResult:
    """Enhanced analysis result"""
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


class SlidingWindowAnalyzer:
    """Sliding window analysis for performance optimization"""

    def __init__(self, window_size: int = 50):
        self.window_size = window_size

    def analyze_with_window(self, lines: List[str], target_line: int,
                           analyzer_func: callable) -> Any:
        """
        Analyze code with sliding window around target line

        Args:
            lines: All lines of code
            target_line: Target line number (0-indexed)
            analyzer_func: Function to analyze the window

        Returns:
            Analysis result from analyzer_func
        """
        # Calculate window boundaries
        start = max(0, target_line - self.window_size)
        end = min(len(lines), target_line + self.window_size + 1)

        # Extract window
        window = lines[start:end]

        # Adjust target line for window
        adjusted_target = target_line - start

        # Analyze window
        return analyzer_func(window, adjusted_target, start)


class EnhancedABAPAnalyzer:
    """Enhanced ABAP analyzer with all improvements"""

    def __init__(self, enable_caching: bool = True,
                 enable_sliding_window: bool = True,
                 window_size: int = 50):
        # Initialize components
        self.db_handler = FinalDBHandler()
        self.advanced_detector = AdvancedPatternDetector()
        self.taint_tracker = ContextAwareTaintTracker()
        self.pattern_cache = PatternCache() if enable_caching else None
        self.sliding_window = SlidingWindowAnalyzer(window_size) if enable_sliding_window else None

        # Performance metrics
        self.metrics = {
            'total_time': 0,
            'pattern_time': 0,
            'taint_time': 0,
            'cache_hits': 0,
            'cache_misses': 0
        }

        # Enhanced multi-line handling
        self.continuation_chars = {'.', ',', ':'}

    def analyze_file(self, file_path: str, target_lines: List[int] = None) -> List[AnalysisResult]:
        """
        Analyze ABAP file with enhanced capabilities

        Args:
            file_path: Path to ABAP file
            target_lines: Optional list of specific line numbers to analyze

        Returns:
            List of analysis results
        """
        start_time = time.time()
        results = []

        # Read file
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()

        # First pass: Context-aware taint tracking
        taint_start = time.time()
        taint_analysis = self.taint_tracker.analyze_code_with_context(lines)
        self.metrics['taint_time'] += time.time() - taint_start

        # Determine lines to analyze
        if target_lines:
            lines_to_analyze = target_lines
        else:
            lines_to_analyze = range(len(lines))

        # Analyze each target line
        for line_num in lines_to_analyze:
            if self.sliding_window:
                # Use sliding window
                result = self.sliding_window.analyze_with_window(
                    lines, line_num,
                    lambda w, t, o: self._analyze_window(w, t, o, file_path, taint_analysis)
                )
            else:
                # Analyze full file
                result = self._analyze_line(lines, line_num, file_path, taint_analysis)

            if result:
                results.append(result)

        self.metrics['total_time'] += time.time() - start_time
        return results

    def _analyze_window(self, window: List[str], target_idx: int,
                       window_offset: int, file_path: str,
                       taint_analysis: Dict) -> Optional[AnalysisResult]:
        """Analyze a window of code"""
        # Build statement starting from target line
        stmt = self._build_enhanced_statement(window, target_idx)
        if not stmt:
            return None

        # Perform analysis
        return self._perform_analysis(
            stmt, window_offset + target_idx, file_path,
            window, taint_analysis
        )

    def _analyze_line(self, lines: List[str], line_num: int,
                     file_path: str, taint_analysis: Dict) -> Optional[AnalysisResult]:
        """Analyze a specific line with context"""
        # Build statement
        stmt = self._build_enhanced_statement(lines, line_num)
        if not stmt:
            return None

        # Perform analysis
        return self._perform_analysis(
            stmt, line_num, file_path,
            lines, taint_analysis
        )

    def _build_enhanced_statement(self, lines: List[str], start_idx: int) -> Optional[str]:
        """
        Build complete statement with enhanced multi-line handling

        Handles:
        - Period (.) continuation
        - Comma (,) continuation in chain statements
        - Colon (:) chain statements
        - Parentheses balancing
        """
        if start_idx >= len(lines):
            return None

        stmt = lines[start_idx].strip()

        # Skip comments
        if stmt.startswith('*') or stmt.startswith('"'):
            return None

        # Handle multi-line statements
        i = start_idx
        open_parens = stmt.count('(') - stmt.count(')')
        in_string = False

        while i < len(lines) - 1:
            # Check for statement end
            if not in_string:
                # Check for continuation
                if stmt.rstrip().endswith('.') and open_parens == 0:
                    break

                # Check for chain statement markers
                if stmt.rstrip().endswith(','):
                    # Comma continuation
                    pass
                elif stmt.rstrip().endswith(':'):
                    # Colon chain
                    pass
                elif open_parens > 0:
                    # Unbalanced parentheses
                    pass
                else:
                    # Check if next line is a continuation
                    next_line = lines[i + 1].strip() if i + 1 < len(lines) else ""
                    if not next_line or next_line.startswith('*'):
                        break
                    # Common continuation patterns
                    if not any(next_line.upper().startswith(kw) for kw in
                              ['DATA', 'SELECT', 'INSERT', 'UPDATE', 'MODIFY',
                               'DELETE', 'CALL', 'PERFORM', 'IF', 'LOOP', 'FORM']):
                        # Likely a continuation
                        pass
                    else:
                        break

            # Add next line
            i += 1
            next_line = lines[i].strip()

            # Skip comment lines in multi-line statements
            if next_line.startswith('*'):
                continue

            stmt += ' ' + next_line

            # Update parentheses count
            open_parens = stmt.count('(') - stmt.count(')')

            # Check for string literals (simplified)
            in_string = stmt.count("'") % 2 != 0

            # Stop if statement is complete
            if stmt.rstrip().endswith('.') and open_parens == 0 and not in_string:
                break

        return stmt if stmt else None

    def _perform_analysis(self, stmt: str, line_num: int, file_path: str,
                         lines: List[str], taint_analysis: Dict) -> Optional[AnalysisResult]:
        """Perform comprehensive analysis on statement"""
        pattern_start = time.time()

        # Initialize result
        result = AnalysisResult(
            file_path=file_path,
            line_number=line_num + 1,  # Convert to 1-indexed
            status="No Match",
            table=None,
            fields={},
            operation=None,
            confidence=0.0,
            tainted_variables=[],
            advanced_patterns=[],
            context_info={},
            performance_metrics={}
        )

        # Check for standard database operations
        db_ops = self.db_handler.analyze([stmt], 0, 1)
        if db_ops:
            op = db_ops[0]
            result.status = "Match"
            result.table = op.table
            result.fields = op.fields
            result.operation = op.operation.value
            result.confidence = op.confidence

            # Check if operation involves tainted variables
            if op.has_sy_uname:
                result.status = "Match with SY-UNAME"
                result.confidence = min(1.0, result.confidence * 1.1)

        # Check for advanced patterns
        advanced_matches = self.advanced_detector.detect_patterns(
            stmt, taint_analysis.get('tainted_variables', {})
        )
        if advanced_matches:
            result.advanced_patterns = advanced_matches
            if not result.operation:
                # Use advanced pattern as primary if no standard match
                best_match = max(advanced_matches, key=lambda x: x['confidence'])
                result.operation = best_match['operation_type']
                result.confidence = best_match['confidence']
                result.status = "Advanced Pattern Match"

        # Check taint analysis
        tainted_in_stmt = self._find_tainted_variables(
            stmt, taint_analysis.get('tainted_variables', {})
        )
        if tainted_in_stmt:
            result.tainted_variables = tainted_in_stmt
            result.confidence = min(1.0, result.confidence * 1.05)

        # Add context information
        result.context_info = self._get_context_info(line_num, taint_analysis)

        # Performance metrics
        self.metrics['pattern_time'] += time.time() - pattern_start
        result.performance_metrics = {
            'analysis_time': time.time() - pattern_start,
            'cache_used': self.pattern_cache is not None
        }

        # Update cache stats if using cache
        if self.pattern_cache:
            cache_stats = self.pattern_cache.get_stats()
            self.metrics['cache_hits'] = cache_stats['total_hits']
            self.metrics['cache_misses'] = cache_stats['total_misses']

        return result if result.status != "No Match" else None

    def _find_tainted_variables(self, stmt: str, tainted_vars: Dict) -> List[str]:
        """Find tainted variables in statement"""
        found = []
        stmt_upper = stmt.upper()

        for var_name in tainted_vars.keys():
            # Check for variable in statement
            # Use word boundaries to avoid partial matches
            pattern = r'\b' + re.escape(var_name) + r'\b'
            if re.search(pattern, stmt_upper):
                found.append(var_name)

        return found

    def _get_context_info(self, line_num: int, taint_analysis: Dict) -> Dict:
        """Get context information for line"""
        context = {
            'scope': 'UNKNOWN',
            'inter_procedural_flows': []
        }

        # Find scope for this line
        for scope_info in taint_analysis.get('scope_analysis', []):
            if 'lines' in scope_info:
                line_range = scope_info['lines'].split('-')
                if len(line_range) == 2:
                    start = int(line_range[0])
                    end = int(line_range[1]) if line_range[1] != '1' else 999999
                    if start <= line_num <= end:
                        context['scope'] = f"{scope_info['type']}:{scope_info['name']}"
                        break

        # Check for inter-procedural flows
        context['inter_procedural_flows'] = list(
            taint_analysis.get('inter_procedural_flows', {}).keys()
        )

        return context

    def get_performance_metrics(self) -> Dict:
        """Get performance metrics"""
        metrics = self.metrics.copy()

        # Add cache statistics if available
        if self.pattern_cache:
            cache_stats = self.pattern_cache.get_stats()
            metrics['cache_hit_rate'] = cache_stats['hit_rate']
            metrics['cache_size'] = cache_stats['cache_size']

        # Calculate percentages
        if metrics['total_time'] > 0:
            metrics['pattern_time_pct'] = (metrics['pattern_time'] / metrics['total_time']) * 100
            metrics['taint_time_pct'] = (metrics['taint_time'] / metrics['total_time']) * 100

        return metrics

    def analyze_csv(self, csv_path: str, output_path: str = None) -> Tuple[List[AnalysisResult], Dict]:
        """
        Analyze CSV file with enhanced analyzer

        Args:
            csv_path: Path to CSV file with columns: id, file_path, line_number
            output_path: Optional output path for results

        Returns:
            Tuple of (results, performance_metrics)
        """
        import csv
        results = []

        # Read CSV
        with open(csv_path, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            rows = list(reader)

        # Group by file for efficiency
        files_to_analyze = {}
        for row in rows:
            file_path = row['file_path']
            line_num = int(row['line_number']) - 1  # Convert to 0-indexed

            if file_path not in files_to_analyze:
                files_to_analyze[file_path] = []
            files_to_analyze[file_path].append(line_num)

        # Analyze each file
        for file_path, line_nums in files_to_analyze.items():
            if Path(file_path).exists():
                file_results = self.analyze_file(file_path, line_nums)
                results.extend(file_results)

        # Save results if output path provided
        if output_path:
            self._save_results(results, output_path)

        return results, self.get_performance_metrics()

    def _save_results(self, results: List[AnalysisResult], output_path: str):
        """Save analysis results to file"""
        import json

        # Convert to JSON-serializable format
        json_results = []
        for result in results:
            result_dict = asdict(result)
            json_results.append(result_dict)

        # Save to file
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(json_results, f, indent=2)

        print(f"Results saved to: {output_path}")


# Example usage
if __name__ == "__main__":
    # Create enhanced analyzer
    analyzer = EnhancedABAPAnalyzer(
        enable_caching=True,
        enable_sliding_window=True,
        window_size=50
    )

    # Example: Analyze a single file
    test_file = "input/test.abap"
    if Path(test_file).exists():
        results = analyzer.analyze_file(test_file)
        print(f"Found {len(results)} matches")

        # Print performance metrics
        metrics = analyzer.get_performance_metrics()
        print("\nPerformance Metrics:")
        for key, value in metrics.items():
            print(f"  {key}: {value}")
    else:
        print(f"Test file {test_file} not found")
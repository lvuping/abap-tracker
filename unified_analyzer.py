"""
통합 ABAP 분석 엔진
패턴 매칭과 오염 추적을 결합한 종합 분석 시스템
"""

import re
import json
from typing import List, Dict, Tuple, Optional, Set
from dataclasses import dataclass, asdict
from datetime import datetime
from improved_patterns import ImprovedPatternMatcher, OperationType


@dataclass
class AnalysisContext:
    """분석 컨텍스트 정보"""
    current_form: Optional[str] = None
    current_loop_depth: int = 0
    current_rfc: Optional[str] = None
    scope_stack: List[str] = None
    
    def __post_init__(self):
        if self.scope_stack is None:
            self.scope_stack = []


class UnifiedAnalyzer:
    """통합 ABAP 분석기"""
    
    def __init__(self, seed_variables: List[str] = None):
        self.seed_variables = seed_variables or ['SY-UNAME']
        self.pattern_matcher = ImprovedPatternMatcher()
        self.context = AnalysisContext()
        
        # 추적 상태
        self.propagation_graph: Dict[str, Set[str]] = {}
        self.variable_scopes: Dict[str, str] = {}
        self.trace_steps: List[str] = []
        
        # 분석 결과
        self.analysis_result = {
            'source': ', '.join(self.seed_variables),
            'tainted_variables': [],
            'data_flows': [],
            'database_sinks': [],
            'propagation_paths': [],
            'trace_steps': [],
            'statistics': {}
        }
    
    def analyze(self, code_snippet: List[str], start_line: int = 0) -> Dict:
        """
        코드 스니펫 분석
        
        Args:
            code_snippet: 분석할 코드 라인 리스트
            start_line: 시작 라인 번호 (0-based)
        
        Returns:
            분석 결과 딕셔너리
        """
        start_time = datetime.now()
        
        # 초기화
        self._reset_state()
        
        # SY-UNAME 위치 확인
        if not self._verify_seed_location(code_snippet, start_line):
            return self._create_error_result("SY-UNAME not found at specified line")
        
        # Phase 1: 구조 분석
        self._analyze_structure(code_snippet)
        
        # Phase 2: 패턴 기반 분석
        self._perform_pattern_analysis(code_snippet, start_line)
        
        # Phase 3: 전파 경로 재구성
        self._reconstruct_propagation_paths()
        
        # Phase 4: 신뢰도 평가
        self._apply_confidence_scoring()
        
        # 통계 정보 추가
        execution_time = (datetime.now() - start_time).total_seconds()
        self._add_statistics(len(code_snippet), execution_time)
        
        return self.analysis_result
    
    def _reset_state(self):
        """상태 초기화"""
        self.pattern_matcher = ImprovedPatternMatcher()
        self.context = AnalysisContext()
        self.propagation_graph.clear()
        self.variable_scopes.clear()
        self.trace_steps.clear()
    
    def _verify_seed_location(self, code_snippet: List[str], start_line: int) -> bool:
        """시드 변수 위치 확인"""
        if start_line >= len(code_snippet):
            self.trace_steps.append(f"❌ Line {start_line + 1} out of range")
            return False
        
        target_line = code_snippet[start_line].strip()
        
        # 주석이나 빈 줄 확인
        if not target_line or target_line.startswith('*'):
            self.trace_steps.append(f"❌ Line {start_line + 1} is empty or comment")
            return False
        
        # SY-UNAME 존재 확인
        for seed in self.seed_variables:
            if seed.upper() in target_line.upper():
                self.trace_steps.append(f"✅ {seed} found at line {start_line + 1}")
                # 시드 변수를 오염된 것으로 초기 표시
                self.pattern_matcher._mark_tainted(seed, start_line + 1, "Seed variable")
                return True
        
        self.trace_steps.append(f"❌ No seed variables found at line {start_line + 1}")
        return False
    
    def _analyze_structure(self, code_snippet: List[str]):
        """코드 구조 분석"""
        for i, line in enumerate(code_snippet):
            line = line.strip()
            if not line or line.startswith('*'):
                continue
            
            # FORM 정의 추적
            form_match = re.match(r'^\s*FORM\s+(\w+)', line, re.IGNORECASE)
            if form_match:
                form_name = form_match.group(1).upper()
                self.context.current_form = form_name
                self.context.scope_stack.append(f"FORM:{form_name}")
                self.trace_steps.append(f"📍 Entering FORM {form_name} at line {i + 1}")
            
            # ENDFORM 추적
            if re.match(r'^\s*ENDFORM', line, re.IGNORECASE):
                if self.context.scope_stack and self.context.scope_stack[-1].startswith('FORM:'):
                    leaving_form = self.context.scope_stack.pop()
                    self.trace_steps.append(f"📍 Leaving {leaving_form} at line {i + 1}")
                self.context.current_form = None
            
            # LOOP 추적
            if re.match(r'^\s*LOOP', line, re.IGNORECASE):
                self.context.current_loop_depth += 1
                self.context.scope_stack.append(f"LOOP:{i + 1}")
                self.trace_steps.append(f"🔄 Entering LOOP at line {i + 1}")
            
            # ENDLOOP 추적
            if re.match(r'^\s*ENDLOOP', line, re.IGNORECASE):
                if self.context.current_loop_depth > 0:
                    self.context.current_loop_depth -= 1
                    if self.context.scope_stack and self.context.scope_stack[-1].startswith('LOOP:'):
                        leaving_loop = self.context.scope_stack.pop()
                        self.trace_steps.append(f"🔄 Leaving {leaving_loop} at line {i + 1}")
            
            # RFC 호출 추적
            rfc_match = re.match(r"^\s*CALL\s+FUNCTION\s+'([^']+)'", line, re.IGNORECASE)
            if rfc_match:
                self.context.current_rfc = rfc_match.group(1)
                self.trace_steps.append(f"📞 RFC call to {self.context.current_rfc} at line {i + 1}")
    
    def _perform_pattern_analysis(self, code_snippet: List[str], start_line: int):
        """패턴 기반 분석 수행"""
        # 시작 라인부터 분석
        for i in range(start_line, len(code_snippet)):
            line = code_snippet[i].strip()
            if not line or line.startswith('*'):
                continue
            
            # 패턴 매처로 라인 분석
            self.pattern_matcher.analyze_line(line, i + 1)
            
            # 현재 스코프 저장
            current_scope = self._get_current_scope()
            for var_name in self.pattern_matcher.tainted_variables:
                if var_name not in self.variable_scopes:
                    self.variable_scopes[var_name] = current_scope
            
            # 중요한 작업 로깅
            self._log_important_operations(line, i + 1)
    
    def _log_important_operations(self, line: str, line_number: int):
        """중요한 작업 로깅"""
        line_upper = line.upper()
        
        # 데이터베이스 작업
        if any(op in line_upper for op in ['INSERT', 'UPDATE', 'MODIFY', 'DELETE']):
            for sink in self.pattern_matcher.database_sinks:
                if sink.line == line_number:
                    fields_str = ', '.join(sink.fields) if sink.fields else 'ALL'
                    self.trace_steps.append(
                        f"💾 {sink.operation} {sink.table} at line {line_number} (Fields: {fields_str})"
                    )
        
        # 할당 작업
        for flow in self.pattern_matcher.data_flows:
            if flow.line == line_number and flow.operation == OperationType.ASSIGNMENT:
                self.trace_steps.append(
                    f"➡️ Assignment: {flow.source} → {flow.target} at line {line_number}"
                )
    
    def _reconstruct_propagation_paths(self):
        """전파 경로 재구성"""
        # 전파 그래프 구축
        for flow in self.pattern_matcher.data_flows:
            if flow.source not in self.propagation_graph:
                self.propagation_graph[flow.source] = set()
            self.propagation_graph[flow.source].add(flow.target)
        
        # 시드 변수부터 sink까지 경로 추적
        paths = []
        for seed in self.seed_variables:
            seed_upper = seed.upper()
            visited = set()
            current_path = []
            self._trace_path(seed_upper, visited, current_path, paths)
        
        self.analysis_result['propagation_paths'] = paths
    
    def _trace_path(self, current: str, visited: Set[str], current_path: List[str], all_paths: List):
        """DFS를 사용한 경로 추적"""
        if current in visited:
            return
        
        visited.add(current)
        current_path.append(current)
        
        # 현재 변수가 sink에 도달하는지 확인
        for sink in self.pattern_matcher.database_sinks:
            if sink.source_variable == current or current in [f"{sink.table}-{f}" for f in sink.fields]:
                all_paths.append({
                    'variable': current,
                    'path': ' → '.join(current_path),
                    'sink': f"{sink.operation} {sink.table}"
                })
        
        # 다음 노드 탐색
        if current in self.propagation_graph:
            for next_var in self.propagation_graph[current]:
                self._trace_path(next_var, visited, current_path, all_paths)
        
        current_path.pop()
        visited.remove(current)
    
    def _apply_confidence_scoring(self):
        """신뢰도 점수 적용"""
        # 데이터 흐름 신뢰도
        for flow in self.pattern_matcher.data_flows:
            confidence = 0.5  # 기본 신뢰도
            
            # 작업 유형별 신뢰도
            if flow.operation in [OperationType.ASSIGNMENT, OperationType.STRUCTURE_FIELD]:
                confidence = 0.9
            elif flow.operation in [OperationType.DATABASE_INSERT, OperationType.DATABASE_UPDATE]:
                confidence = 0.95
            elif flow.operation in [OperationType.RFC_CALL, OperationType.PERFORM_CALL]:
                confidence = 0.6
            
            # 스코프 기반 조정
            if flow.target in self.variable_scopes:
                scope = self.variable_scopes[flow.target]
                if scope.startswith('LOOP:'):
                    confidence *= 0.9  # 루프 내부는 약간 낮은 신뢰도
            
            flow.confidence = confidence
        
        # Sink 신뢰도
        for sink in self.pattern_matcher.database_sinks:
            confidence = 0.7  # 기본 신뢰도
            
            # 직접 DB 작업은 높은 신뢰도
            if not sink.operation.startswith('INDIRECT_'):
                confidence = 0.9
            
            # 필드가 명시된 경우 더 높은 신뢰도
            if sink.fields:
                confidence = min(0.95, confidence + 0.1 * len(sink.fields))
            
            # Z/Y 테이블은 최고 신뢰도
            if sink.table.startswith(('Z', 'Y')):
                confidence = min(0.99, confidence + 0.1)
            
            sink.confidence = confidence
    
    def _get_current_scope(self) -> str:
        """현재 스코프 반환"""
        if not self.context.scope_stack:
            return 'GLOBAL'
        return self.context.scope_stack[-1]
    
    def _add_statistics(self, total_lines: int, execution_time: float):
        """통계 정보 추가"""
        pattern_result = self.pattern_matcher.get_analysis_result()
        
        self.analysis_result.update({
            'tainted_variables': pattern_result['tainted_variables'],
            'data_flows': pattern_result['data_flows'],
            'database_sinks': pattern_result['database_sinks'],
            'trace_steps': self.trace_steps[:10],  # 처음 10개만
            'statistics': {
                'total_lines': total_lines,
                'tainted_count': len(pattern_result['tainted_variables']),
                'flow_count': len(pattern_result['data_flows']),
                'sink_count': len(pattern_result['database_sinks']),
                'execution_time': f"{execution_time:.3f}s"
            }
        })
    
    def _create_error_result(self, reason: str) -> Dict:
        """오류 결과 생성"""
        return {
            'status': 'Not Found',
            'reason': reason,
            'trace_steps': self.trace_steps,
            'statistics': {
                'total_lines': 0,
                'tainted_count': 0,
                'flow_count': 0,
                'sink_count': 0
            }
        }
    
    def export_report(self, output_file: str = None) -> str:
        """분석 보고서 생성"""
        report = []
        report.append("=" * 80)
        report.append("ABAP TAINT ANALYSIS REPORT")
        report.append("=" * 80)
        report.append(f"Timestamp: {datetime.now().isoformat()}")
        report.append(f"Source Variables: {self.analysis_result['source']}")
        report.append("")
        
        # 요약
        stats = self.analysis_result.get('statistics', {})
        report.append("SUMMARY")
        report.append("-" * 40)
        report.append(f"Tainted Variables: {stats.get('tainted_count', 0)}")
        report.append(f"Data Flows: {stats.get('flow_count', 0)}")
        report.append(f"Database Sinks: {stats.get('sink_count', 0)}")
        report.append(f"Execution Time: {stats.get('execution_time', 'N/A')}")
        report.append("")
        
        # 오염된 변수
        if self.analysis_result['tainted_variables']:
            report.append("TAINTED VARIABLES")
            report.append("-" * 40)
            for var in self.analysis_result['tainted_variables'][:20]:
                report.append(f"  • {var}")
            if len(self.analysis_result['tainted_variables']) > 20:
                report.append(f"  ... and {len(self.analysis_result['tainted_variables']) - 20} more")
            report.append("")
        
        # 데이터베이스 작업
        if self.analysis_result['database_sinks']:
            report.append("DATABASE OPERATIONS")
            report.append("-" * 40)
            for sink in self.analysis_result['database_sinks'][:10]:
                fields = ', '.join(sink['fields']) if sink['fields'] else 'ALL'
                report.append(f"  • {sink['operation']} {sink['table']} (Line {sink['line']})")
                report.append(f"    Fields: {fields}")
                report.append(f"    Source: {sink['source']}")
            report.append("")
        
        # 전파 경로
        if self.analysis_result['propagation_paths']:
            report.append("PROPAGATION PATHS")
            report.append("-" * 40)
            for path in self.analysis_result['propagation_paths'][:5]:
                report.append(f"  • {path['variable']}")
                report.append(f"    Path: {path['path']}")
                report.append(f"    Sink: {path['sink']}")
            report.append("")
        
        # 추적 단계
        if self.analysis_result['trace_steps']:
            report.append("TRACE STEPS")
            report.append("-" * 40)
            for step in self.analysis_result['trace_steps']:
                report.append(f"  {step}")
            report.append("")
        
        report_text = '\n'.join(report)
        
        if output_file:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(report_text)
        
        return report_text
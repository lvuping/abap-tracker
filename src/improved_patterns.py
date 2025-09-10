"""
향상된 ABAP 패턴 매칭 시스템
복잡한 ABAP 구문을 정확하게 인식하고 추적
"""

import re
from typing import List, Dict, Tuple, Optional, Set
from dataclasses import dataclass
from enum import Enum


class OperationType(Enum):
    """작업 유형 분류"""
    ASSIGNMENT = "ASSIGNMENT"
    DATABASE_INSERT = "DATABASE_INSERT"
    DATABASE_UPDATE = "DATABASE_UPDATE"
    DATABASE_MODIFY = "DATABASE_MODIFY"
    DATABASE_DELETE = "DATABASE_DELETE"
    RFC_CALL = "RFC_CALL"
    PERFORM_CALL = "PERFORM_CALL"
    STRUCTURE_FIELD = "STRUCTURE_FIELD"
    INTERNAL_TABLE = "INTERNAL_TABLE"
    CLEAR_OPERATION = "CLEAR_OPERATION"
    VALUE_CONSTRUCTOR = "VALUE_CONSTRUCTOR"


@dataclass
class TaintedVariable:
    """오염된 변수 정보"""
    name: str
    line: int
    reason: str
    scope: str = "GLOBAL"
    fields: List[str] = None
    confidence: float = 1.0
    source: str = None  # 오염의 원인이 된 변수 (예: SY-UNAME)


@dataclass
class DataFlow:
    """데이터 흐름 정보"""
    source: str
    target: str
    line: int
    operation: OperationType
    confidence: float = 1.0


@dataclass
class DatabaseSink:
    """데이터베이스 작업 정보"""
    table: str
    operation: str
    line: int
    fields: List[str]
    source_variable: str
    confidence: float = 1.0


class ImprovedPatternMatcher:
    """향상된 ABAP 패턴 매처"""
    
    def __init__(self):
        self.tainted_variables: Dict[str, TaintedVariable] = {}
        self.data_flows: List[DataFlow] = []
        self.database_sinks: List[DatabaseSink] = []
        self.internal_table_mappings: Dict[str, str] = {}
        self.work_area_mappings: Dict[str, str] = {}
        
        # Early termination flag - stop when first Z/Y table operation is found
        self.found_target_operation = False
        self.target_table = None
        self.target_fields = []
        
        # 향상된 패턴 정의
        self._init_patterns()
    
    def _init_patterns(self):
        """패턴 초기화"""
        
        # 직접 할당 패턴
        self.assignment_patterns = [
            # 기본 할당: var = value
            (r'^\s*(\w+(?:-\w+)?)\s*=\s*(.+?)(?:\.|$)', 'direct'),
            # MOVE 문: MOVE value TO var
            (r'^\s*MOVE\s+(.+?)\s+TO\s+(\w+(?:-\w+)?)(?:\s|\.|$)', 'move'),
            # VALUE 생성자: var = VALUE #( field = value )
            (r'^\s*(\w+)\s*=\s*VALUE\s+#?\s*\(\s*(.+?)\s*\)', 'value'),
            # 구조체 필드 할당: wa-field = value
            (r'^\s*(\w+)-(\w+)\s*=\s*(.+?)(?:\s|\.|$)', 'structure'),
        ]
        
        # 데이터베이스 작업 패턴
        self.database_patterns = [
            # INSERT 작업
            (r'^\s*INSERT\s+(\w+)(?:\s+FROM\s+(\w+))?', OperationType.DATABASE_INSERT),
            (r'^\s*INSERT\s+INTO\s+(\w+)\s+VALUES\s+(\w+)', OperationType.DATABASE_INSERT),
            # UPDATE 작업
            (r'^\s*UPDATE\s+(\w+)\s+(?:FROM\s+(\w+)|SET\s+(.+?))', OperationType.DATABASE_UPDATE),
            # MODIFY 작업
            (r'^\s*MODIFY\s+(\w+)(?:\s+FROM\s+(\w+))?', OperationType.DATABASE_MODIFY),
            # DELETE 작업
            (r'^\s*DELETE\s+(?:FROM\s+)?(\w+)(?:\s+WHERE\s+(.+?))?', OperationType.DATABASE_DELETE),
        ]
        
        # Internal Table 작업 패턴
        self.internal_table_patterns = [
            # APPEND: work area를 internal table에 추가
            (r'^\s*APPEND\s+(\w+)\s+TO\s+(\w+)', 'append'),
            # INSERT: work area를 table에 삽입
            (r'^\s*INSERT\s+(\w+)\s+INTO\s+(?:TABLE\s+)?(\w+)', 'insert'),
            # MODIFY: internal table 수정
            (r'^\s*MODIFY\s+(?:TABLE\s+)?(\w+)\s+FROM\s+(\w+)', 'modify'),
            # SELECT INTO TABLE: DB에서 internal table로
            (r'SELECT\s+.*?\s+FROM\s+(\w+).*?INTO\s+(?:CORRESPONDING\s+FIELDS\s+OF\s+)?TABLE\s+(\w+)', 'select'),
        ]
        
        # 함수/서브루틴 호출 패턴
        self.call_patterns = [
            # RFC 호출
            (r"^\s*CALL\s+FUNCTION\s+'([^']+)'", OperationType.RFC_CALL),
            # PERFORM 호출
            (r'^\s*PERFORM\s+(\w+)(?:\s+USING\s+(.+?))?', OperationType.PERFORM_CALL),
            # SUBMIT 프로그램
            (r'^\s*SUBMIT\s+(\w+)(?:\s+WITH\s+(.+?))?', 'submit'),
        ]
        
        # CLEAR/REFRESH/FREE 패턴
        self.clear_patterns = [
            (r'^\s*(CLEAR|REFRESH|FREE)(?::\s*|\s+)(\w+(?:,\s*\w+)*)', 'clear'),
        ]
        
        # 체인 문장 패턴 (콜론 표기법)
        self.chain_pattern = r'^\s*(\w+):\s*(.+?)(?:\.|$)'
        
        # VALUE 생성자 내부 필드 패턴
        self.value_field_pattern = r'(\w+)\s*=\s*([^,\)]+)'
        
        # MOVE-CORRESPONDING 패턴
        self.move_corresponding_pattern = r'^\s*MOVE-CORRESPONDING\s+(\w+)\s+TO\s+(\w+)'
    
    def analyze_line(self, line: str, line_number: int) -> None:
        """한 줄 분석"""
        # Early termination if target operation already found
        if self.found_target_operation:
            return
            
        line = line.strip()
        if not line or line.startswith('*'):
            return
        
        # 주석 제거
        line = line.split('"')[0].strip()
        
        # 체인 문장 처리
        if self._is_chain_statement(line):
            self._process_chain_statement(line, line_number)
            return
        
        # 각 패턴 유형별 분석
        self._check_assignments(line, line_number)
        self._check_database_operations(line, line_number)
        self._check_internal_tables(line, line_number)
        self._check_function_calls(line, line_number)
        self._check_clear_operations(line, line_number)
        self._check_move_corresponding(line, line_number)
    
    def _check_assignments(self, line: str, line_number: int) -> None:
        """할당문 검사"""
        for pattern, assign_type in self.assignment_patterns:
            match = re.match(pattern, line, re.IGNORECASE)
            if match:
                if assign_type == 'structure':
                    # 구조체 필드 할당: wa-field = value
                    structure = match.group(1).upper()
                    field = match.group(2).upper()
                    value = match.group(3).strip()
                    
                    if self._is_system_variable(value) or self._is_tainted(value):
                        full_path = f"{structure}-{field}"
                        # source 정보 전달 - SY-UNAME인지 확인
                        source = value.upper() if 'SY-UNAME' in value.upper() else self._get_taint_source(value)
                        self._mark_tainted(full_path, line_number, f"Assignment from {value}", source)
                        self._mark_tainted(structure, line_number, f"Contains tainted field {field}", source)
                        self._add_flow(value, full_path, line_number, OperationType.STRUCTURE_FIELD)
                        
                elif assign_type == 'value':
                    # VALUE 생성자
                    target = match.group(1).upper()
                    fields_content = match.group(2)
                    
                    if self._process_value_constructor(target, fields_content, line_number):
                        self._mark_tainted(target, line_number, "VALUE constructor with tainted data")
                        
                elif assign_type == 'move':
                    # MOVE 문
                    source = match.group(1).strip()
                    target = match.group(2).upper()
                    
                    if self._is_system_variable(source) or self._is_tainted(source):
                        # source 정보 전달
                        taint_source = source.upper() if 'SY-UNAME' in source.upper() else self._get_taint_source(source)
                        self._mark_tainted(target, line_number, f"MOVE from {source}", taint_source)
                        self._add_flow(source, target, line_number, OperationType.ASSIGNMENT)
                        
                else:  # direct
                    # 직접 할당
                    target = match.group(1).upper()
                    source = match.group(2).strip()
                    
                    if self._is_system_variable(source) or self._is_tainted(source):
                        # source 정보 전달
                        taint_source = source.upper() if 'SY-UNAME' in source.upper() else self._get_taint_source(source)
                        self._mark_tainted(target, line_number, f"Assignment from {source}", taint_source)
                        self._add_flow(source, target, line_number, OperationType.ASSIGNMENT)
    
    def _check_database_operations(self, line: str, line_number: int) -> None:
        """데이터베이스 작업 검사"""
        for pattern, op_type in self.database_patterns:
            match = re.match(pattern, line, re.IGNORECASE)
            if match:
                table = match.group(1).upper()
                
                # Z/Y 테이블만 추적
                if not (table.startswith('Z') or table.startswith('Y')):
                    continue
                
                source = None
                fields = []
                
                if len(match.groups()) >= 2 and match.group(2):
                    source = match.group(2).upper()
                    
                    if self._is_tainted(source):
                        # SY-UNAME에서 오염된 필드만 추출
                        fields = self._get_tainted_fields_from_source(source, 'SY-UNAME')
                        if fields:  # Only if we have SY-UNAME affected fields
                            self._add_sink(table, op_type.value, line_number, fields, source)
                            # Mark that we found the target operation
                            self.found_target_operation = True
                            self.target_table = table
                            self.target_fields = fields
                            return  # Stop processing further
                        
                elif op_type == OperationType.DATABASE_UPDATE and len(match.groups()) >= 3:
                    # UPDATE SET 절 처리
                    set_clause = match.group(3)
                    if set_clause:
                        fields = self._parse_set_clause(set_clause, line_number)
                        if fields:
                            self._add_sink(table, op_type.value, line_number, fields, "SET_CLAUSE")
                            # Mark that we found the target operation
                            self.found_target_operation = True
                            self.target_table = table
                            self.target_fields = fields
                            return  # Stop processing further
    
    def _check_internal_tables(self, line: str, line_number: int) -> None:
        """Internal Table 작업 검사"""
        for pattern, op_type in self.internal_table_patterns:
            match = re.search(pattern, line, re.IGNORECASE)
            if match:
                if op_type == 'select':
                    # SELECT INTO TABLE: DB 테이블과 internal table 매핑
                    db_table = match.group(1).upper()
                    int_table = match.group(2).upper()
                    
                    if db_table.startswith(('Z', 'Y')):
                        self.internal_table_mappings[int_table] = db_table
                        
                elif op_type in ['append', 'insert']:
                    # Work area를 internal table에 추가
                    work_area = match.group(1).upper()
                    int_table = match.group(2).upper()
                    
                    if self._is_tainted(work_area):
                        self._mark_tainted(int_table, line_number, f"{op_type.upper()} tainted work area")
                        self._add_flow(work_area, int_table, line_number, OperationType.INTERNAL_TABLE)
                        
                        # Internal table이 DB 테이블과 연결되어 있으면 sink 추가
                        if int_table in self.internal_table_mappings:
                            db_table = self.internal_table_mappings[int_table]
                            # SY-UNAME에서 오염된 필드만 추출
                            fields = self._get_tainted_fields_from_source(work_area, 'SY-UNAME')
                            if fields and (db_table.startswith('Z') or db_table.startswith('Y')):
                                self._add_sink(db_table, f"INDIRECT_{op_type.upper()}", line_number, fields, work_area)
                                # Mark that we found the target operation
                                self.found_target_operation = True
                                self.target_table = db_table
                                self.target_fields = fields
                                return  # Stop processing further
                            
                elif op_type == 'modify':
                    # MODIFY internal table
                    int_table = match.group(1).upper()
                    work_area = match.group(2).upper()
                    
                    if self._is_tainted(work_area):
                        self._mark_tainted(int_table, line_number, "MODIFY from tainted work area")
                        self._add_flow(work_area, int_table, line_number, OperationType.INTERNAL_TABLE)
    
    def _check_function_calls(self, line: str, line_number: int) -> None:
        """함수/서브루틴 호출 검사"""
        for pattern, op_type in self.call_patterns:
            match = re.match(pattern, line, re.IGNORECASE)
            if match:
                if op_type == OperationType.RFC_CALL:
                    func_name = match.group(1)
                    # Check if it's a Z/Y function and has tainted data
                    if (func_name.startswith('Z') or func_name.startswith('Y')):
                        # Check if SY-UNAME data is being passed
                        if any(self._is_tainted(var) for var in self.tainted_variables):
                            self._add_flow("SY-UNAME", func_name, line_number, op_type)
                            # Mark as found for RFC calls too
                            self.found_target_operation = True
                            self.target_table = func_name  # Store RFC name as target
                            self.target_fields = ["RFC_PARAMETER"]  # Generic RFC parameter
                            return
                    
                elif op_type == OperationType.PERFORM_CALL:
                    routine = match.group(1)
                    params = match.group(2) if len(match.groups()) > 1 else None
                    
                    if params:
                        # USING 파라미터 분석
                        param_list = params.split()
                        for param in param_list:
                            param = param.strip().upper()
                            if self._is_tainted(param):
                                self._add_flow(param, routine, line_number, op_type)
    
    def _check_clear_operations(self, line: str, line_number: int) -> None:
        """CLEAR/REFRESH/FREE 작업 검사"""
        for pattern, _ in self.clear_patterns:
            match = re.match(pattern, line, re.IGNORECASE)
            if match:
                operation = match.group(1).upper()
                variables = match.group(2).split(',')
                
                for var in variables:
                    var = var.strip().upper()
                    if var in self.tainted_variables:
                        del self.tainted_variables[var]
                        # 구조체 필드도 제거
                        fields_to_remove = [k for k in self.tainted_variables if k.startswith(f"{var}-")]
                        for field in fields_to_remove:
                            del self.tainted_variables[field]
    
    def _check_move_corresponding(self, line: str, line_number: int) -> None:
        """MOVE-CORRESPONDING 검사"""
        match = re.match(self.move_corresponding_pattern, line, re.IGNORECASE)
        if match:
            source = match.group(1).upper()
            target = match.group(2).upper()
            
            if self._is_tainted(source):
                self._mark_tainted(target, line_number, f"MOVE-CORRESPONDING from {source}")
                self._add_flow(source, target, line_number, OperationType.ASSIGNMENT)
                
                # 필드 단위 전파
                source_fields = self._get_tainted_fields(source)
                for field in source_fields:
                    self._mark_tainted(f"{target}-{field}", line_number, f"MOVE-CORRESPONDING field {field}")
    
    def _is_chain_statement(self, line: str) -> bool:
        """체인 문장인지 확인"""
        return bool(re.match(self.chain_pattern, line, re.IGNORECASE))
    
    def _process_chain_statement(self, line: str, line_number: int) -> None:
        """체인 문장 처리"""
        match = re.match(self.chain_pattern, line, re.IGNORECASE)
        if match:
            prefix = match.group(1)
            statements = match.group(2).split(',')
            
            for stmt in statements:
                full_statement = f"{prefix} {stmt.strip()}."
                self.analyze_line(full_statement, line_number)
    
    def _process_value_constructor(self, target: str, fields_content: str, line_number: int) -> bool:
        """VALUE 생성자 처리"""
        has_tainted = False
        
        # 필드 할당 파싱
        field_matches = re.findall(self.value_field_pattern, fields_content)
        for field_name, field_value in field_matches:
            field_name = field_name.upper()
            field_value = field_value.strip()
            
            if self._is_system_variable(field_value) or self._is_tainted(field_value):
                full_path = f"{target}-{field_name}"
                self._mark_tainted(full_path, line_number, f"VALUE field from {field_value}")
                has_tainted = True
        
        return has_tainted
    
    def _parse_set_clause(self, set_clause: str, line_number: int) -> List[str]:
        """UPDATE SET 절 파싱"""
        tainted_fields = []
        
        # field = value 패턴 찾기
        assignments = set_clause.split(',')
        for assignment in assignments:
            match = re.match(r'(\w+)\s*=\s*(.+)', assignment.strip())
            if match:
                field = match.group(1).upper()
                value = match.group(2).strip()
                
                if self._is_system_variable(value) or self._is_tainted(value):
                    tainted_fields.append(field)
        
        return tainted_fields
    
    def _is_system_variable(self, value: str) -> bool:
        """시스템 변수인지 확인"""
        system_vars = ['SY-UNAME', 'SY-DATUM', 'SY-UZEIT', 'SY-MANDT', 'SY-LANGU']
        value_upper = value.upper()
        return any(sv in value_upper for sv in system_vars)
    
    def _is_tainted(self, variable: str) -> bool:
        """변수가 오염되었는지 확인"""
        variable = variable.upper()
        
        # 직접 오염된 변수
        if variable in self.tainted_variables:
            return True
        
        # 구조체 필드 확인
        for tainted_var in self.tainted_variables:
            if tainted_var.startswith(f"{variable}-") or variable.startswith(f"{tainted_var}-"):
                return True
        
        return False
    
    def _get_tainted_fields(self, structure: str) -> List[str]:
        """오염된 구조체 필드 목록 반환"""
        structure = structure.upper()
        fields = []
        
        for var_name in self.tainted_variables:
            if var_name.startswith(f"{structure}-"):
                field = var_name[len(structure) + 1:]
                if field:
                    fields.append(field)
        
        return fields
    
    def _get_tainted_fields_from_source(self, structure: str, source: str = 'SY-UNAME') -> List[str]:
        """특정 source(예: SY-UNAME)에서 오염된 구조체 필드만 반환"""
        structure = structure.upper()
        source = source.upper()
        fields = []
        
        for var_name, tainted_var in self.tainted_variables.items():
            if var_name.startswith(f"{structure}-"):
                # source가 일치하거나, chain으로 연결된 경우
                if tainted_var.source == source or (tainted_var.source and source in tainted_var.source):
                    field = var_name[len(structure) + 1:]
                    if field:
                        fields.append(field)
        
        return fields
    
    def _get_taint_source(self, variable: str) -> str:
        """변수의 오염 source를 반환"""
        variable = variable.upper()
        if variable in self.tainted_variables:
            return self.tainted_variables[variable].source or variable
        return variable
    
    def _mark_tainted(self, variable: str, line: int, reason: str, source: str = None) -> None:
        """변수를 오염된 것으로 표시"""
        variable = variable.upper()
        if variable not in self.tainted_variables:
            self.tainted_variables[variable] = TaintedVariable(
                name=variable,
                line=line,
                reason=reason,
                source=source
            )
    
    def _add_flow(self, source: str, target: str, line: int, operation: OperationType) -> None:
        """데이터 흐름 추가"""
        self.data_flows.append(DataFlow(
            source=source.upper() if isinstance(source, str) else source,
            target=target.upper(),
            line=line,
            operation=operation
        ))
    
    def _add_sink(self, table: str, operation: str, line: int, fields: List[str], source: str) -> None:
        """데이터베이스 sink 추가"""
        self.database_sinks.append(DatabaseSink(
            table=table,
            operation=operation,
            line=line,
            fields=fields,
            source_variable=source
        ))
    
    def get_analysis_result(self) -> Dict:
        """분석 결과 반환"""
        return {
            'tainted_variables': list(self.tainted_variables.keys()),
            'data_flows': [
                {
                    'source': flow.source,
                    'target': flow.target,
                    'line': flow.line,
                    'operation': flow.operation.value
                }
                for flow in self.data_flows
            ],
            'database_sinks': [
                {
                    'table': sink.table,
                    'operation': sink.operation,
                    'line': sink.line,
                    'fields': sink.fields,
                    'source': sink.source_variable
                }
                for sink in self.database_sinks
            ]
        }
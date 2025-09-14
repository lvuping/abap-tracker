#!/usr/bin/env python3
"""
Context-Aware Taint Tracking Module
Enhanced taint propagation with scope and context awareness
"""

import re
from typing import Dict, List, Set, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum


class ScopeType(Enum):
    """Types of scopes in ABAP code"""
    GLOBAL = "GLOBAL"
    FORM = "FORM"
    METHOD = "METHOD"
    LOOP = "LOOP"
    IF_BLOCK = "IF_BLOCK"
    CASE_BLOCK = "CASE_BLOCK"
    DO_WHILE = "DO_WHILE"
    FUNCTION = "FUNCTION"
    CLASS = "CLASS"


class TaintType(Enum):
    """Types of taint sources"""
    SYSTEM_VARIABLE = "SYSTEM_VARIABLE"
    USER_INPUT = "USER_INPUT"
    DATABASE_READ = "DATABASE_READ"
    PARAMETER = "PARAMETER"
    RETURN_VALUE = "RETURN_VALUE"
    FIELD_SYMBOL = "FIELD_SYMBOL"
    REFERENCE = "REFERENCE"


@dataclass
class TaintContext:
    """Context information for tainted variable"""
    scope: ScopeType
    scope_name: str
    line_start: int
    line_end: int
    parent_scope: Optional['TaintContext'] = None
    local_variables: Set[str] = field(default_factory=set)
    parameters: Dict[str, str] = field(default_factory=dict)  # param_name: param_type


@dataclass
class EnhancedTaintedVariable:
    """Enhanced tainted variable with full context"""
    name: str
    taint_type: TaintType
    source_line: int
    confidence: float
    context: TaintContext
    propagation_path: List[str] = field(default_factory=list)
    affected_fields: Set[str] = field(default_factory=set)
    aliases: Set[str] = field(default_factory=set)
    data_type: Optional[str] = None
    is_global: bool = False
    is_field_symbol: bool = False
    is_reference: bool = False


class ContextAwareTaintTracker:
    """Advanced taint tracking with context awareness"""

    def __init__(self):
        self.tainted_vars: Dict[str, EnhancedTaintedVariable] = {}
        self.scope_stack: List[TaintContext] = []
        self.current_scope: Optional[TaintContext] = None
        self.global_scope = TaintContext(
            scope=ScopeType.GLOBAL,
            scope_name="GLOBAL",
            line_start=0,
            line_end=999999
        )
        self.scope_stack.append(self.global_scope)
        self.current_scope = self.global_scope

        # System variable patterns
        self.system_vars = {
            'SY-UNAME', 'SY_UNAME', 'SYUNAME',
            'SY-DATUM', 'SY-UZEIT', 'SY-MANDT',
            'SY-LANGU', 'SY-SYSID', 'SY-CLIENT'
        }

        # Track inter-procedural flows
        self.procedure_flows: Dict[str, List[str]] = {}  # procedure_name: [tainted_params]

    def enter_scope(self, scope_type: ScopeType, scope_name: str, line_start: int):
        """Enter a new scope"""
        new_context = TaintContext(
            scope=scope_type,
            scope_name=scope_name,
            line_start=line_start,
            line_end=-1,  # Will be set when exiting scope
            parent_scope=self.current_scope
        )
        self.scope_stack.append(new_context)
        self.current_scope = new_context

    def exit_scope(self, line_end: int):
        """Exit current scope"""
        if len(self.scope_stack) > 1:  # Keep global scope
            self.current_scope.line_end = line_end
            self.scope_stack.pop()
            self.current_scope = self.scope_stack[-1]

    def analyze_code_with_context(self, lines: List[str]) -> Dict:
        """Analyze code with full context tracking"""
        results = {
            'tainted_variables': {},
            'scope_analysis': [],
            'inter_procedural_flows': {}
        }

        for i, line in enumerate(lines):
            line_stripped = line.strip()

            # Skip comments
            if line_stripped.startswith('*') or line_stripped.startswith('"'):
                continue

            # Detect scope changes
            self._detect_scope_change(line_stripped, i)

            # Track variable declarations
            self._track_declarations(line_stripped, i)

            # Track assignments and propagation
            self._track_assignments(line_stripped, i)

            # Track procedure calls
            self._track_procedure_calls(line_stripped, i)

            # Track field symbols and references
            self._track_field_symbols(line_stripped, i)

        # Finalize analysis
        results['tainted_variables'] = self._get_tainted_summary()
        results['scope_analysis'] = self._get_scope_analysis()
        results['inter_procedural_flows'] = self.procedure_flows

        return results

    def _detect_scope_change(self, line: str, line_num: int):
        """Detect and track scope changes"""
        line_upper = line.upper()

        # FORM/ENDFORM
        if line_upper.startswith('FORM '):
            match = re.match(r'FORM\s+(\w+)', line, re.IGNORECASE)
            if match:
                self.enter_scope(ScopeType.FORM, match.group(1), line_num)
                self._parse_form_parameters(line)
        elif line_upper.startswith('ENDFORM'):
            self.exit_scope(line_num)

        # METHOD/ENDMETHOD
        elif line_upper.startswith('METHOD '):
            match = re.match(r'METHOD\s+(\w+)', line, re.IGNORECASE)
            if match:
                self.enter_scope(ScopeType.METHOD, match.group(1), line_num)
        elif line_upper.startswith('ENDMETHOD'):
            self.exit_scope(line_num)

        # LOOP/ENDLOOP
        elif line_upper.startswith('LOOP '):
            self.enter_scope(ScopeType.LOOP, f"LOOP_{line_num}", line_num)
        elif line_upper.startswith('ENDLOOP'):
            self.exit_scope(line_num)

        # IF/ENDIF
        elif line_upper.startswith('IF '):
            self.enter_scope(ScopeType.IF_BLOCK, f"IF_{line_num}", line_num)
        elif line_upper.startswith('ENDIF'):
            self.exit_scope(line_num)

        # DO/ENDDO
        elif line_upper.startswith('DO '):
            self.enter_scope(ScopeType.DO_WHILE, f"DO_{line_num}", line_num)
        elif line_upper.startswith('ENDDO'):
            self.exit_scope(line_num)

        # FUNCTION/ENDFUNCTION
        elif line_upper.startswith('FUNCTION '):
            match = re.match(r'FUNCTION\s+(\w+)', line, re.IGNORECASE)
            if match:
                self.enter_scope(ScopeType.FUNCTION, match.group(1), line_num)
        elif line_upper.startswith('ENDFUNCTION'):
            self.exit_scope(line_num)

    def _parse_form_parameters(self, line: str):
        """Parse FORM parameters for taint tracking"""
        # Parse USING and CHANGING parameters
        using_match = re.search(r'USING\s+(.*?)(?:CHANGING|\.|\s*$)', line, re.IGNORECASE)
        changing_match = re.search(r'CHANGING\s+(.*?)(?:\.|\s*$)', line, re.IGNORECASE)

        if using_match:
            params = re.findall(r'(\w+)(?:\s+TYPE\s+(\w+))?', using_match.group(1))
            for param_name, param_type in params:
                self.current_scope.parameters[param_name.upper()] = param_type or 'ANY'

        if changing_match:
            params = re.findall(r'(\w+)(?:\s+TYPE\s+(\w+))?', changing_match.group(1))
            for param_name, param_type in params:
                self.current_scope.parameters[param_name.upper()] = param_type or 'ANY'

    def _track_declarations(self, line: str, line_num: int):
        """Track variable declarations"""
        # DATA declarations
        data_match = re.match(r'DATA:\s*(\w+)', line, re.IGNORECASE)
        if data_match:
            var_name = data_match.group(1).upper()
            self.current_scope.local_variables.add(var_name)

            # Check for initial value with SY-UNAME
            if re.search(r'VALUE\s+sy-uname', line, re.IGNORECASE):
                self._mark_tainted(var_name, TaintType.SYSTEM_VARIABLE, line_num)

        # Field symbol declarations
        fs_match = re.match(r'FIELD-SYMBOLS:\s*<(\w+)>', line, re.IGNORECASE)
        if fs_match:
            fs_name = f"<{fs_match.group(1).upper()}>"
            self.current_scope.local_variables.add(fs_name)

    def _track_assignments(self, line: str, line_num: int):
        """Track variable assignments and taint propagation"""
        # Direct assignment: var = sy-uname
        assign_match = re.match(r'(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
        if assign_match:
            target = assign_match.group(1).upper()
            source = assign_match.group(2).strip()

            # Check if source is system variable
            if self._is_system_variable(source):
                self._mark_tainted(target, TaintType.SYSTEM_VARIABLE, line_num)

            # Check if source is tainted variable
            elif self._is_tainted(source.upper()):
                self._propagate_taint(source.upper(), target, line_num)

        # Structure field assignment: struct-field = sy-uname
        struct_match = re.match(r'(\w+)-(\w+)\s*=\s*(.*?)(?:\.|$)', line, re.IGNORECASE)
        if struct_match:
            struct_name = struct_match.group(1).upper()
            field_name = struct_match.group(2).upper()
            source = struct_match.group(3).strip()

            if self._is_system_variable(source) or self._is_tainted(source.upper()):
                full_name = f"{struct_name}-{field_name}"
                self._mark_tainted(full_name, TaintType.SYSTEM_VARIABLE, line_num)

                # Also mark the structure as containing tainted data
                if struct_name in self.tainted_vars:
                    self.tainted_vars[struct_name].affected_fields.add(field_name)
                else:
                    self._mark_tainted(struct_name, TaintType.SYSTEM_VARIABLE, line_num)
                    self.tainted_vars[struct_name].affected_fields.add(field_name)

        # MOVE statement
        move_match = re.match(r'MOVE\s+(.*?)\s+TO\s+(.*?)(?:\.|$)', line, re.IGNORECASE)
        if move_match:
            source = move_match.group(1).strip().upper()
            target = move_match.group(2).strip().upper()

            if self._is_system_variable(source) or self._is_tainted(source):
                self._propagate_taint(source, target, line_num)

        # MOVE-CORRESPONDING
        move_corr_match = re.match(r'MOVE-CORRESPONDING\s+(\w+)\s+TO\s+(\w+)', line, re.IGNORECASE)
        if move_corr_match:
            source = move_corr_match.group(1).upper()
            target = move_corr_match.group(2).upper()

            if self._is_tainted(source):
                self._propagate_taint(source, target, line_num)
                # Mark all fields as potentially tainted
                if source in self.tainted_vars:
                    for field in self.tainted_vars[source].affected_fields:
                        self._mark_tainted(f"{target}-{field}", TaintType.SYSTEM_VARIABLE, line_num)

    def _track_procedure_calls(self, line: str, line_num: int):
        """Track procedure calls for inter-procedural analysis"""
        # PERFORM statement
        perform_match = re.match(r'PERFORM\s+(\w+)(?:\s+USING\s+(.*?))?(?:\s+CHANGING\s+(.*?))?(?:\.|$)',
                                line, re.IGNORECASE)
        if perform_match:
            proc_name = perform_match.group(1).upper()
            using_params = perform_match.group(2) or ""
            changing_params = perform_match.group(3) or ""

            tainted_params = []
            # Check USING parameters
            for param in re.findall(r'\w+', using_params):
                if self._is_tainted(param.upper()):
                    tainted_params.append(param.upper())

            # Check CHANGING parameters
            for param in re.findall(r'\w+', changing_params):
                if self._is_tainted(param.upper()):
                    tainted_params.append(param.upper())

            if tainted_params:
                self.procedure_flows[proc_name] = tainted_params

        # CALL FUNCTION
        call_match = re.match(r'CALL\s+FUNCTION\s+[\'"](\w+)[\'"]', line, re.IGNORECASE)
        if call_match:
            func_name = call_match.group(1).upper()
            # Mark function calls that might propagate taint
            if 'USER' in func_name or 'CREATE' in func_name or 'UPDATE' in func_name:
                self.procedure_flows[func_name] = ['POTENTIAL_TAINT']

    def _track_field_symbols(self, line: str, line_num: int):
        """Track field symbols and references"""
        # ASSIGN statement
        assign_match = re.match(r'ASSIGN\s+(.*?)\s+TO\s+<(\w+)>', line, re.IGNORECASE)
        if assign_match:
            source = assign_match.group(1).strip().upper()
            fs_name = f"<{assign_match.group(2).upper()}>"

            if self._is_tainted(source):
                self._mark_tainted(fs_name, TaintType.FIELD_SYMBOL, line_num)
                self.tainted_vars[fs_name].is_field_symbol = True

        # GET REFERENCE
        ref_match = re.match(r'GET\s+REFERENCE\s+OF\s+(.*?)\s+INTO\s+(\w+)', line, re.IGNORECASE)
        if ref_match:
            source = ref_match.group(1).strip().upper()
            target = ref_match.group(2).upper()

            if self._is_tainted(source):
                self._mark_tainted(target, TaintType.REFERENCE, line_num)
                self.tainted_vars[target].is_reference = True

    def _is_system_variable(self, text: str) -> bool:
        """Check if text contains system variable"""
        if not text:
            return False
        text_upper = text.upper().replace('_', '-')
        return any(var in text_upper for var in self.system_vars)

    def _is_tainted(self, var_name: str) -> bool:
        """Check if variable is tainted in current or parent scopes"""
        var_upper = var_name.upper()

        # Check current scope and parent scopes
        scope = self.current_scope
        while scope:
            # Check if variable is tainted in this scope
            for tainted_var in self.tainted_vars.values():
                if tainted_var.name == var_upper:
                    # Check if variable is accessible in current scope
                    if (tainted_var.is_global or
                        tainted_var.context == scope or
                        self._is_parent_scope(tainted_var.context, scope)):
                        return True

            scope = scope.parent_scope

        return False

    def _is_parent_scope(self, parent: TaintContext, child: TaintContext) -> bool:
        """Check if parent is an ancestor scope of child"""
        current = child.parent_scope
        while current:
            if current == parent:
                return True
            current = current.parent_scope
        return False

    def _mark_tainted(self, var_name: str, taint_type: TaintType, line_num: int):
        """Mark variable as tainted with context"""
        var_upper = var_name.upper()

        # Check if it's a global variable
        is_global = (self.current_scope == self.global_scope or
                    var_upper not in self.current_scope.local_variables)

        tainted_var = EnhancedTaintedVariable(
            name=var_upper,
            taint_type=taint_type,
            source_line=line_num,
            confidence=1.0 if taint_type == TaintType.SYSTEM_VARIABLE else 0.9,
            context=self.current_scope,
            is_global=is_global
        )

        self.tainted_vars[var_upper] = tainted_var

    def _propagate_taint(self, source: str, target: str, line_num: int):
        """Propagate taint from source to target"""
        source_upper = source.upper()
        target_upper = target.upper()

        if source_upper in self.tainted_vars:
            source_taint = self.tainted_vars[source_upper]

            # Create new tainted variable
            target_taint = EnhancedTaintedVariable(
                name=target_upper,
                taint_type=source_taint.taint_type,
                source_line=line_num,
                confidence=source_taint.confidence * 0.95,  # Slight confidence decay
                context=self.current_scope,
                propagation_path=source_taint.propagation_path + [source_upper],
                is_global=(self.current_scope == self.global_scope)
            )

            # Copy aliases and affected fields
            target_taint.aliases = source_taint.aliases.copy()
            target_taint.affected_fields = source_taint.affected_fields.copy()

            self.tainted_vars[target_upper] = target_taint

    def _get_tainted_summary(self) -> Dict:
        """Get summary of tainted variables"""
        summary = {}
        for var_name, var_info in self.tainted_vars.items():
            summary[var_name] = {
                'type': var_info.taint_type.value,
                'confidence': var_info.confidence,
                'scope': var_info.context.scope.value,
                'scope_name': var_info.context.scope_name,
                'line': var_info.source_line,
                'is_global': var_info.is_global,
                'propagation_path': var_info.propagation_path,
                'affected_fields': list(var_info.affected_fields)
            }
        return summary

    def _get_scope_analysis(self) -> List[Dict]:
        """Get scope analysis results"""
        analysis = []
        for scope in self.scope_stack:
            scope_info = {
                'type': scope.scope.value,
                'name': scope.scope_name,
                'lines': f"{scope.line_start}-{scope.line_end}",
                'local_vars': list(scope.local_variables),
                'parameters': scope.parameters,
                'tainted_vars': []
            }

            # Find tainted variables in this scope
            for var_name, var_info in self.tainted_vars.items():
                if var_info.context == scope:
                    scope_info['tainted_vars'].append(var_name)

            analysis.append(scope_info)

        return analysis

    def get_tainted_variables_by_scope(self, scope_type: ScopeType = None) -> List[str]:
        """Get tainted variables filtered by scope type"""
        if scope_type:
            return [
                var.name for var in self.tainted_vars.values()
                if var.context.scope == scope_type
            ]
        return list(self.tainted_vars.keys())

    def get_inter_procedural_flows(self) -> Dict[str, List[str]]:
        """Get inter-procedural taint flows"""
        return self.procedure_flows.copy()

    def analyze_data_flow(self, var_name: str) -> Dict:
        """Analyze complete data flow for a variable"""
        var_upper = var_name.upper()
        if var_upper not in self.tainted_vars:
            return {'found': False}

        var_info = self.tainted_vars[var_upper]
        return {
            'found': True,
            'variable': var_upper,
            'source': var_info.taint_type.value,
            'confidence': var_info.confidence,
            'propagation_path': var_info.propagation_path,
            'scope_chain': self._get_scope_chain(var_info.context),
            'affected_fields': list(var_info.affected_fields),
            'aliases': list(var_info.aliases)
        }

    def _get_scope_chain(self, context: TaintContext) -> List[str]:
        """Get the scope chain for a context"""
        chain = []
        current = context
        while current:
            chain.append(f"{current.scope.value}:{current.scope_name}")
            current = current.parent_scope
        return chain
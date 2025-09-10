"""
Enhanced Taint Propagation Optimizer for ABAP Code Analysis
Reduces redundancy and improves accuracy of taint tracking
"""

from typing import Dict, List, Set, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
import re


@dataclass
class TaintSource:
    """Represents the original source of taint"""
    name: str
    line: int
    type: str  # 'system_variable', 'parameter', 'field', 'derived'


@dataclass
class TaintedVariable:
    """Enhanced tainted variable with propagation tracking"""
    name: str
    line: int
    reason: str
    source: TaintSource
    confidence: float
    propagation_depth: int = 0
    children: List[str] = field(default_factory=list)
    is_structure: bool = False
    affected_fields: List[str] = field(default_factory=list)


class PropagationType(Enum):
    """Types of taint propagation"""
    DIRECT_ASSIGNMENT = "direct"
    STRUCTURE_FIELD = "field"
    INTERNAL_TABLE = "table"
    MOVE_CORRESPONDING = "move_corr"
    VALUE_CONSTRUCTOR = "value"
    PARAMETER_PASSING = "param"
    RFC_CALL = "rfc"
    PERFORM_CALL = "perform"


class TaintPropagationOptimizer:
    """Optimized taint propagation handler"""
    
    def __init__(self):
        # Track tainted variables with enhanced metadata
        self.tainted_variables: Dict[str, TaintedVariable] = {}
        
        # Track propagation chains to avoid redundancy
        self.propagation_chains: Dict[str, List[str]] = {}
        
        # Track which variables are aliases of others
        self.aliases: Dict[str, str] = {}
        
        # System variables that are taint sources
        self.system_sources = {
            'SY-UNAME': 'user_id',
            'SY-DATUM': 'date',
            'SY-UZEIT': 'time',
            'SY-MANDT': 'client'
        }
        
        # Track confidence decay per propagation level
        self.confidence_decay = {
            PropagationType.DIRECT_ASSIGNMENT: 0.98,
            PropagationType.STRUCTURE_FIELD: 0.95,
            PropagationType.INTERNAL_TABLE: 0.92,
            PropagationType.MOVE_CORRESPONDING: 0.90,
            PropagationType.VALUE_CONSTRUCTOR: 0.93,
            PropagationType.PARAMETER_PASSING: 0.88,
            PropagationType.RFC_CALL: 0.85,
            PropagationType.PERFORM_CALL: 0.87
        }
        
        # Track already processed combinations to avoid redundancy
        self.processed_propagations: Set[Tuple[str, str, int]] = set()
    
    def mark_tainted(self, 
                     variable: str, 
                     line: int, 
                     reason: str,
                     source_variable: Optional[str] = None,
                     propagation_type: PropagationType = PropagationType.DIRECT_ASSIGNMENT) -> bool:
        """
        Mark a variable as tainted with optimized tracking
        
        Returns:
            True if this is a new taint, False if redundant
        """
        variable = variable.upper()
        
        # Check if this is a redundant propagation
        propagation_key = (source_variable or 'SYSTEM', variable, line)
        if propagation_key in self.processed_propagations:
            return False
        
        # Check if variable is already tainted from the same source
        if variable in self.tainted_variables:
            existing = self.tainted_variables[variable]
            # If same source and later line, this is redundant
            if existing.source.name == (source_variable or 'SY-UNAME'):
                if line > existing.line:
                    return False
        
        # Determine the taint source
        if source_variable:
            # Propagated taint
            if source_variable in self.tainted_variables:
                parent_taint = self.tainted_variables[source_variable]
                source = parent_taint.source
                depth = parent_taint.propagation_depth + 1
                confidence = parent_taint.confidence * self.confidence_decay[propagation_type]
            else:
                # Source is not tracked, create new source
                source = TaintSource(
                    name=source_variable,
                    line=line,
                    type='derived'
                )
                depth = 1
                confidence = 0.80
        else:
            # Direct system variable taint
            source = TaintSource(
                name=variable if variable in self.system_sources else 'SY-UNAME',
                line=line,
                type='system_variable'
            )
            depth = 0
            confidence = 1.0
        
        # Check for structure fields
        is_structure = '-' in variable
        affected_fields = []
        
        if is_structure:
            # Extract structure and field
            parts = variable.split('-')
            if len(parts) == 2:
                struct_name, field_name = parts
                affected_fields = [field_name]
                
                # Also mark the base structure as tainted
                if struct_name not in self.tainted_variables:
                    self.tainted_variables[struct_name] = TaintedVariable(
                        name=struct_name,
                        line=line,
                        reason=f"Structure containing tainted field {field_name}",
                        source=source,
                        confidence=confidence * 0.95,
                        propagation_depth=depth,
                        is_structure=True,
                        affected_fields=[field_name]
                    )
                else:
                    # Add field to existing structure
                    if field_name not in self.tainted_variables[struct_name].affected_fields:
                        self.tainted_variables[struct_name].affected_fields.append(field_name)
        
        # Create or update tainted variable
        self.tainted_variables[variable] = TaintedVariable(
            name=variable,
            line=line,
            reason=reason,
            source=source,
            confidence=confidence,
            propagation_depth=depth,
            is_structure=is_structure,
            affected_fields=affected_fields
        )
        
        # Track propagation chain
        if source_variable and source_variable in self.propagation_chains:
            self.propagation_chains[variable] = self.propagation_chains[source_variable] + [variable]
        else:
            self.propagation_chains[variable] = [variable]
        
        # Mark as processed
        self.processed_propagations.add(propagation_key)
        
        # Update parent's children if exists
        if source_variable and source_variable in self.tainted_variables:
            if variable not in self.tainted_variables[source_variable].children:
                self.tainted_variables[source_variable].children.append(variable)
        
        return True
    
    def is_tainted(self, variable: str) -> bool:
        """Check if a variable is tainted"""
        variable = variable.upper()
        
        # Check direct match
        if variable in self.tainted_variables:
            return True
        
        # Check aliases
        if variable in self.aliases:
            return self.aliases[variable] in self.tainted_variables
        
        # Check if it's a field of a tainted structure
        if '-' in variable:
            struct_name = variable.split('-')[0]
            if struct_name in self.tainted_variables:
                return self.tainted_variables[struct_name].is_structure
        
        # Check for system variables
        return variable in self.system_sources
    
    def get_taint_source(self, variable: str) -> Optional[TaintSource]:
        """Get the original source of taint for a variable"""
        variable = variable.upper()
        
        if variable in self.tainted_variables:
            return self.tainted_variables[variable].source
        
        if variable in self.aliases:
            alias = self.aliases[variable]
            if alias in self.tainted_variables:
                return self.tainted_variables[alias].source
        
        return None
    
    def add_alias(self, alias: str, original: str) -> None:
        """Add an alias relationship between variables"""
        alias = alias.upper()
        original = original.upper()
        
        # Follow alias chains
        while original in self.aliases:
            original = self.aliases[original]
        
        self.aliases[alias] = original
    
    def get_tainted_fields(self, structure: str) -> List[str]:
        """Get list of tainted fields in a structure"""
        structure = structure.upper()
        
        if structure in self.tainted_variables:
            var = self.tainted_variables[structure]
            if var.is_structure:
                return var.affected_fields.copy()
        
        # Also check for individual field entries
        fields = []
        prefix = f"{structure}-"
        for var_name in self.tainted_variables:
            if var_name.startswith(prefix):
                field = var_name[len(prefix):]
                if field not in fields:
                    fields.append(field)
        
        return fields
    
    def clean_taint(self, variable: str) -> None:
        """Remove taint from a variable (e.g., after CLEAR statement)"""
        variable = variable.upper()
        
        # Remove the variable itself
        if variable in self.tainted_variables:
            # Remove from parent's children
            tainted_var = self.tainted_variables[variable]
            for other_var in self.tainted_variables.values():
                if variable in other_var.children:
                    other_var.children.remove(variable)
            
            del self.tainted_variables[variable]
        
        # Remove aliases
        aliases_to_remove = [alias for alias, orig in self.aliases.items() if orig == variable]
        for alias in aliases_to_remove:
            del self.aliases[alias]
        
        # Remove from propagation chains
        if variable in self.propagation_chains:
            del self.propagation_chains[variable]
        
        # Remove structure fields if it's a structure
        fields_to_remove = [var for var in self.tainted_variables if var.startswith(f"{variable}-")]
        for field in fields_to_remove:
            del self.tainted_variables[field]
    
    def get_propagation_summary(self) -> Dict:
        """Get a summary of taint propagation"""
        summary = {
            'total_tainted': len(self.tainted_variables),
            'system_sources': sum(1 for v in self.tainted_variables.values() 
                                if v.source.type == 'system_variable'),
            'max_depth': max((v.propagation_depth for v in self.tainted_variables.values()), default=0),
            'unique_sources': len(set(v.source.name for v in self.tainted_variables.values())),
            'structures': sum(1 for v in self.tainted_variables.values() if v.is_structure),
            'high_confidence': sum(1 for v in self.tainted_variables.values() if v.confidence > 0.90),
            'medium_confidence': sum(1 for v in self.tainted_variables.values() if 0.70 <= v.confidence <= 0.90),
            'low_confidence': sum(1 for v in self.tainted_variables.values() if v.confidence < 0.70)
        }
        
        return summary
    
    def get_tainted_variables_list(self, min_confidence: float = 0.0) -> List[str]:
        """Get list of tainted variables above confidence threshold"""
        return [
            var.name for var in self.tainted_variables.values()
            if var.confidence >= min_confidence
        ]
    
    def optimize_propagation_chains(self) -> None:
        """Optimize propagation chains by removing redundant entries"""
        # Remove redundant chains where a variable appears multiple times
        for var, chain in self.propagation_chains.items():
            # Remove duplicates while preserving order
            seen = set()
            optimized = []
            for item in chain:
                if item not in seen:
                    seen.add(item)
                    optimized.append(item)
            self.propagation_chains[var] = optimized
        
        # Remove variables that are just pass-through (no operations on them)
        pass_through_vars = []
        for var_name, var in self.tainted_variables.items():
            # If variable has exactly one child and no other operations
            if len(var.children) == 1 and var.propagation_depth > 0:
                # Check if it's just a temporary assignment
                if 'temporary' in var.reason.lower() or 'assignment' in var.reason.lower():
                    pass_through_vars.append(var_name)
        
        # Merge pass-through variables
        for var in pass_through_vars:
            if var in self.tainted_variables and self.tainted_variables[var].children:
                child = self.tainted_variables[var].children[0]
                if child in self.tainted_variables:
                    # Update child to point to parent's source
                    self.tainted_variables[child].propagation_depth -= 1
                    self.tainted_variables[child].confidence *= 1.05  # Boost confidence slightly


# Example usage
if __name__ == "__main__":
    optimizer = TaintPropagationOptimizer()
    
    # Test taint propagation
    optimizer.mark_tainted('LV_USER', 10, 'Direct assignment from SY-UNAME')
    optimizer.mark_tainted('WA_TABLE-CREATED_BY', 11, 'Field assignment', 'LV_USER', PropagationType.STRUCTURE_FIELD)
    optimizer.mark_tainted('LT_TABLE', 12, 'Append work area', 'WA_TABLE', PropagationType.INTERNAL_TABLE)
    
    # Test redundancy prevention
    optimizer.mark_tainted('WA_TABLE-CREATED_BY', 15, 'Redundant assignment', 'LV_USER', PropagationType.STRUCTURE_FIELD)
    
    print("Tainted Variables:")
    for var_name, var in optimizer.tainted_variables.items():
        print(f"  {var_name}: confidence={var.confidence:.2f}, depth={var.propagation_depth}")
    
    print("\nPropagation Summary:")
    print(optimizer.get_propagation_summary())
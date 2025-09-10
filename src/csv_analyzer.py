"""
Enhanced CSV Analyzer for SY-UNAME Tracking
Extracts comprehensive information including keywords, tables, fields, and RFC details
"""

import re
from typing import List, Dict, Optional, Set, Tuple
from dataclasses import dataclass, asdict
from unified_analyzer import UnifiedAnalyzer
from insert_values_handler import InsertValuesHandler
from multiline_statement_handler import MultilineStatementHandler, enhance_partial_cases
from complete_analyzer import CompleteAnalyzer, enhance_to_complete


@dataclass
class ComprehensiveAnalysis:
    """Comprehensive analysis result with all extracted information"""
    id: str
    source_file: str
    line_number: int
    status: str
    keywords: List[str]
    tables: List[str]
    fields: List[str]
    rfc_functions: List[str]
    rfc_parameters: List[str]
    operations: List[str]
    perform_calls: List[str]  # Added PERFORM calls
    tainted_variables: List[str]
    confidence: float
    description: str
    trace_path: str
    
    def to_csv_row(self) -> Dict:
        """Convert to CSV row format - prioritizing direct impacts"""
        return {
            "ID": self.id,
            "Source_File": self.source_file,
            "Line_Number": self.line_number,
            "Status": self.status,
            "Final_Table": ", ".join(self.tables[:3]) if self.tables else "",  # Primary tables
            "Final_Fields": ", ".join(self.fields[:5]) if self.fields else "",  # Key fields only
            "DB_Operations": ", ".join([k for k in self.keywords if k in ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MODIFY']][:3]),
            "RFC_Functions": ", ".join(self.rfc_functions[:3]) if self.rfc_functions else "",
            "PERFORM_Calls": ", ".join(self.perform_calls[:3]) if self.perform_calls else "",
            "Confidence": f"{self.confidence:.2f}",
            "Tainted_Variables": ", ".join(self.tainted_variables[:5]),  # Top 5 only
            "Description": self.description[:150]  # Shorter description
        }


class EnhancedCSVAnalyzer:
    """Enhanced analyzer for CSV processing with comprehensive data extraction"""
    
    # ABAP Keywords to detect - ONLY DIRECT DATABASE/FUNCTION IMPACTS
    ABAP_KEYWORDS = {
        'DATA_OPERATIONS': ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MODIFY'],
        'FUNCTIONS': ['CALL FUNCTION', 'PERFORM', 'SUBMIT'],
        'TABLE_OPERATIONS': ['APPEND', 'INSERT INTO TABLE', 'MODIFY TABLE', 'DELETE FROM TABLE']
    }
    
    # Common SAP Tables (for detection)
    COMMON_TABLES = {
        'USER': ['USR01', 'USR02', 'USR03', 'USR04', 'USR05', 'UST04', 'UST12'],
        'AUTH': ['AGR_USERS', 'AGR_DEFINE', 'AGR_PROF', 'AGR_1251', 'AGR_TCODES'],
        'AUDIT': ['CDHDR', 'CDPOS', 'DBTABLOG'],
        'SYSTEM': ['T000', 'T001', 'T002', 'T003', 'T004', 'T005'],
        'CUSTOM': []  # Will detect Z* and Y* tables
    }
    
    def __init__(self):
        self.analyzer = UnifiedAnalyzer(seed_variables=['SY-UNAME'])
        self.insert_values_handler = InsertValuesHandler()
        self.multiline_handler = MultilineStatementHandler()
        self.complete_analyzer = CompleteAnalyzer()
    
    def analyze_location(self, 
                         id_value: str,
                         file_path: str, 
                         line_number: int,
                         code_snippet: List[str],
                         actual_line_number: int = None) -> ComprehensiveAnalysis:
        """
        Analyze a single SY-UNAME location and extract comprehensive information
        
        Args:
            id_value: Unique identifier for this location
            file_path: Path to the ABAP file
            line_number: Line number where SY-UNAME appears (0-based relative to snippet)
            code_snippet: Code snippet to analyze
            actual_line_number: Actual line number in the original file (1-based)
        
        Returns:
            ComprehensiveAnalysis object with all extracted information
        """
        # Use actual_line_number if provided, otherwise use the relative line_number
        display_line_number = actual_line_number if actual_line_number is not None else line_number
        
        # Run unified analyzer (line_number is already 0-based relative to snippet)
        analysis_result = self.analyzer.analyze(code_snippet, line_number)
        
        # Extract keywords from surrounding code
        keywords = self._extract_keywords(code_snippet, line_number - 1)
        
        # Extract tables and fields
        tables, fields = self._extract_database_info(analysis_result, code_snippet)
        
        # Check if we found the first database operation with Z/Y table
        # If yes, don't search for RFC and PERFORM calls - they come after
        found_primary_operation = False
        if tables and len(tables) > 0:
            # Check if it's a Z/Y table (custom table)
            first_table = tables[0]
            if first_table and (first_table.startswith('Z') or first_table.startswith('Y')):
                found_primary_operation = True
        
        # Only extract RFC and PERFORM if no primary database operation was found
        if not found_primary_operation:
            # Extract RFC information
            rfc_functions, rfc_parameters = self._extract_rfc_info(analysis_result, code_snippet)
            
            # Extract PERFORM calls
            perform_calls = self._extract_perform_calls(analysis_result, code_snippet)
        else:
            # We found the primary operation, stop searching for additional operations
            rfc_functions = []
            rfc_parameters = []
            perform_calls = []
        
        # Extract operations
        operations = self._extract_operations(analysis_result)
        
        # Extract tainted variables
        tainted_variables = self._extract_tainted_variables(analysis_result)
        
        # Calculate confidence score
        confidence = self._calculate_confidence(analysis_result)
        
        # Generate description
        description = self._generate_description(
            operations, tables, fields, rfc_functions, tainted_variables
        )
        
        # Extract trace path
        trace_path = self._extract_trace_path(analysis_result)
        
        # Add extracted data to analysis_result for status determination
        analysis_result['final_table'] = tables[0] if tables else None
        analysis_result['final_fields'] = fields[0] if fields else None
        analysis_result['database_operations'] = operations[0] if operations else None
        analysis_result['rfc_functions'] = rfc_functions
        analysis_result['perform_calls'] = perform_calls
        
        # Enhance partial cases with multiline statement analysis
        if not analysis_result['final_table'] or not analysis_result['final_fields']:
            # Try to get missing information from multiline context
            multiline_context = self.multiline_handler.get_complete_context(
                code_snippet, 
                line_number  # This is already 0-based relative to snippet
            )
            
            # Update missing table
            if multiline_context['table'] and not analysis_result['final_table']:
                analysis_result['final_table'] = multiline_context['table']
                if not tables:
                    tables = [multiline_context['table']]
            
            # Update missing fields
            if multiline_context['fields'] and not analysis_result['final_fields']:
                analysis_result['final_fields'] = multiline_context['fields'][0] if multiline_context['fields'] else None
                if not fields and multiline_context['fields']:
                    fields = multiline_context['fields'][:5]
            
            # Update missing operation
            if multiline_context['operation'] and not analysis_result['database_operations']:
                analysis_result['database_operations'] = multiline_context['operation']
                if not operations:
                    operations = [multiline_context['operation']]
            
            # Update confidence if improved
            if multiline_context['confidence'] > confidence:
                confidence = multiline_context['confidence']
        
        # Final enhancement to ensure Complete status
        # Use the complete analyzer to fill any remaining gaps
        if analysis_result.get('status') != 'Complete' or not all([
            analysis_result.get('final_table'),
            analysis_result.get('final_fields'),
            analysis_result.get('database_operations')
        ]):
            complete_result = self.complete_analyzer.analyze_complete(
                code_snippet,
                line_number,
                analysis_result.get('status', 'Partial'),
                analysis_result.get('final_table'),
                analysis_result.get('final_fields'),
                analysis_result.get('database_operations')
            )
            
            # Update with complete analysis
            if complete_result['table']:
                analysis_result['final_table'] = complete_result['table']
                if not tables:
                    tables = [complete_result['table']]
            
            if complete_result['field']:
                analysis_result['final_fields'] = complete_result['field']
                if not fields:
                    fields = [complete_result['field']]
            
            if complete_result['operation']:
                analysis_result['database_operations'] = complete_result['operation']
                if not operations:
                    operations = [complete_result['operation']]
            
            # Update confidence
            if complete_result['confidence'] > 0:
                confidence = complete_result['confidence']
        
        # Determine status
        status = self._determine_status(analysis_result)
        
        return ComprehensiveAnalysis(
            id=id_value,
            source_file=file_path,
            line_number=display_line_number,
            status=status,
            keywords=keywords,
            tables=tables,
            fields=fields,
            rfc_functions=rfc_functions,
            rfc_parameters=rfc_parameters,
            operations=operations,
            perform_calls=perform_calls,
            tainted_variables=tainted_variables,
            confidence=confidence,
            description=description,
            trace_path=trace_path
        )
    
    def _extract_keywords(self, code_snippet: List[str], target_line: int) -> List[str]:
        """Extract ABAP keywords from surrounding code"""
        keywords = []
        context_range = 20  # Look 20 lines before and after
        
        start = max(0, target_line - context_range)
        end = min(len(code_snippet), target_line + context_range + 1)
        
        for i in range(start, end):
            line = code_snippet[i].upper().strip()
            
            # Skip comments and empty lines
            if not line or line.startswith('*'):
                continue
            
            # Check for each keyword category
            for category, keyword_list in self.ABAP_KEYWORDS.items():
                for keyword in keyword_list:
                    if keyword in line:
                        # Add keyword with category for better context
                        keyword_with_context = f"{keyword}"
                        if keyword_with_context not in keywords:
                            keywords.append(keyword_with_context)
        
        return keywords[:20]  # Return top 20 keywords
    
    def _extract_database_info(self, 
                               analysis_result: Dict,
                               code_snippet: List[str]) -> Tuple[List[str], List[str]]:
        """Extract database tables and fields - focusing on direct impacts"""
        tables = []
        fields = []
        
        # Priority 1: From database sinks (most important) - ONLY FIRST ONE
        database_sinks = analysis_result.get('database_sinks', [])
        if database_sinks:
            # Take only the FIRST sink (the first Z/Y table operation)
            first_sink = database_sinks[0]
            table = first_sink.get('table', '')
            if table:
                tables.append(table)
            
            # Only fields from the FIRST sink
            sink_fields = first_sink.get('fields', [])
            for field in sink_fields:
                if field and field not in fields:
                    # Only include fields ending with _BY (user tracking fields)
                    field_upper = field.upper()
                    if field_upper.endswith('_BY') or field_upper == 'SY-UNAME' or field_upper == 'RFC_PARAMETER':
                        fields.append(field)
        
        # Priority 2: Check for INSERT VALUES patterns using the enhanced handler
        if not tables or not fields:
            # Try to detect INSERT VALUES patterns
            insert_patterns = self.insert_values_handler.analyze_insert_values(code_snippet)
            
            # Get tainted variables from analysis
            tainted_vars = analysis_result.get('tainted_variables', [])
            
            # Extract variable names properly
            tainted_var_names = []
            if isinstance(tainted_vars, list):
                for var in tainted_vars:
                    if isinstance(var, dict):
                        name = var.get('name', '')
                        if name:
                            tainted_var_names.append(name)
                    elif isinstance(var, str):
                        tainted_var_names.append(var)
            
            for pattern in insert_patterns:
                # Check if this pattern has SY-UNAME or tainted variables
                tainted_fields = self.insert_values_handler.find_tainted_fields(
                    pattern, 
                    tainted_var_names
                )
                
                if tainted_fields:
                    # Add table if not already present
                    if pattern.table not in tables and pattern.table.startswith(('Z', 'Y')):
                        tables.append(pattern.table)
                    
                    # Add tainted fields
                    for field in tainted_fields:
                        if field not in fields:
                            fields.append(field)
                    
                    # Only process first matching pattern for consistency
                    if tables and fields:
                        break
        
        # Return only the first table and its fields (already filtered above)
        return tables[:1], fields[:5]  # Only first table and up to 5 fields
    
    def _extract_rfc_info(self, 
                         analysis_result: Dict,
                         code_snippet: List[str]) -> Tuple[List[str], List[str]]:
        """Extract RFC function names and parameters"""
        rfc_functions = []
        rfc_parameters = []
        
        # From analysis result
        for flow in analysis_result.get('data_flows', []):
            if 'RFC' in str(flow.get('operation', '')):
                target = flow.get('target', '')
                if target and target not in rfc_functions:
                    rfc_functions.append(target)
        
        # Additional detection from code
        for i, line in enumerate(code_snippet):
            # Detect CALL FUNCTION statements
            rfc_match = re.match(r".*CALL\s+FUNCTION\s+'([^']+)'", line, re.IGNORECASE)
            if rfc_match:
                func_name = rfc_match.group(1)
                if func_name not in rfc_functions:
                    rfc_functions.append(func_name)
                
                # Extract parameters from following lines
                j = i + 1
                while j < len(code_snippet) and j < i + 20:  # Look ahead max 20 lines
                    param_line = code_snippet[j]
                    
                    # Look for EXPORTING/IMPORTING/TABLES/CHANGING
                    if re.search(r'(EXPORTING|IMPORTING|TABLES|CHANGING)', param_line, re.IGNORECASE):
                        # Extract parameter names
                        param_matches = re.findall(r'(\w+)\s*=', param_line)
                        for param in param_matches:
                            if param not in rfc_parameters:
                                rfc_parameters.append(param)
                    
                    # Stop at end of CALL FUNCTION
                    if '.' in param_line and not param_line.strip().endswith(','):
                        break
                    
                    j += 1
        
        return rfc_functions[:5], rfc_parameters[:10]  # Limit results
    
    def _extract_perform_calls(self, 
                              analysis_result: Dict,
                              code_snippet: List[str]) -> List[str]:
        """Extract PERFORM subroutine calls"""
        perform_calls = []
        
        # From analysis result
        for flow in analysis_result.get('data_flows', []):
            if 'PERFORM' in str(flow.get('operation', '')):
                target = flow.get('target', '')
                if target and target not in perform_calls:
                    perform_calls.append(target)
        
        # Additional detection from code
        for line in code_snippet[:200]:  # Check first 200 lines
            # Detect PERFORM statements
            perform_match = re.match(r".*PERFORM\s+(\w+)", line, re.IGNORECASE)
            if perform_match:
                routine_name = perform_match.group(1)
                if routine_name not in perform_calls:
                    perform_calls.append(routine_name)
        
        return perform_calls[:5]  # Limit to 5 PERFORM calls
    
    def _extract_operations(self, analysis_result: Dict) -> List[str]:
        """Extract operation types"""
        operations = []
        
        # From data flows
        for flow in analysis_result.get('data_flows', []):
            op = str(flow.get('operation', ''))
            if op and op not in operations:
                # Clean operation name
                op = op.replace('OperationType.', '').replace('_', ' ').title()
                operations.append(op)
        
        # From database sinks
        for sink in analysis_result.get('database_sinks', []):
            op = sink.get('operation', '')
            if op and op not in operations:
                operations.append(op)
        
        return operations[:10]  # Limit results
    
    def _extract_tainted_variables(self, analysis_result: Dict) -> List[str]:
        """Extract tainted variables"""
        tainted = []
        
        for var in analysis_result.get('tainted_variables', []):
            if isinstance(var, dict):
                var_name = var.get('name', '')
            else:
                var_name = str(var)
            
            if var_name and var_name not in tainted:
                tainted.append(var_name)
        
        return tainted[:20]  # Limit results
    
    def _calculate_confidence(self, analysis_result: Dict) -> float:
        """Calculate confidence score based on analysis quality and complexity
        
        Confidence ranges:
        - Direct assignment: 0.95-1.00
        - One-level indirection: 0.85-0.94
        - Multi-level flow: 0.70-0.84
        - Complex/uncertain: 0.60-0.69
        """
        # Start with base confidence
        confidence = 0.60  # Minimum confidence for any detection
        
        stats = analysis_result.get('statistics', {})
        tainted_count = stats.get('tainted_count', 0)
        flow_count = stats.get('flow_count', 0)
        sink_count = stats.get('sink_count', 0)
        
        # Check if we have complete information
        has_table = bool(analysis_result.get('final_table'))
        has_field = bool(analysis_result.get('final_fields'))
        has_operation = bool(analysis_result.get('database_operations'))
        
        # Direct assignment pattern (highest confidence)
        if has_table and has_field and has_operation:
            if flow_count <= 2:  # Direct assignment
                confidence = 0.95 + (sink_count * 0.01)  # 0.95-1.00
            elif flow_count <= 4:  # One-level indirection
                confidence = 0.85 + (sink_count * 0.02)  # 0.85-0.94
            else:  # Multi-level flow
                confidence = 0.70 + min(0.14, flow_count * 0.02)  # 0.70-0.84
        # Partial information
        elif has_table or has_field or has_operation:
            if flow_count <= 2:
                confidence = 0.75 + (tainted_count * 0.02)
            else:
                confidence = 0.65 + min(0.15, flow_count * 0.01)
        # Only taint tracking available
        elif tainted_count > 0:
            confidence = 0.60 + min(0.09, tainted_count * 0.01)
        
        # Bonus for direct database operations
        for sink in analysis_result.get('database_sinks', []):
            if not sink.get('operation', '').startswith('INDIRECT'):
                confidence = min(1.0, confidence + 0.05)
                break
        
        # Bonus for RFC or PERFORM tracking
        if analysis_result.get('rfc_functions'):
            confidence = min(1.0, confidence + 0.02)
        if analysis_result.get('perform_calls'):
            confidence = min(1.0, confidence + 0.02)
        
        return round(min(1.0, max(0.60, confidence)), 2)
    
    def _generate_description(self, 
                             operations: List[str],
                             tables: List[str],
                             fields: List[str],
                             rfc_functions: List[str],
                             tainted_variables: List[str]) -> str:
        """Generate human-readable description"""
        parts = []
        
        if operations:
            parts.append(f"Operations: {', '.join(operations[:3])}")
        
        if tables:
            parts.append(f"Tables: {', '.join(tables[:3])}")
        
        if fields:
            parts.append(f"Fields: {len(fields)} affected")
        
        if rfc_functions:
            parts.append(f"RFC: {', '.join(rfc_functions[:2])}")
        
        if tainted_variables:
            parts.append(f"Variables: {len(tainted_variables)} tainted")
        
        if not parts:
            return "SY-UNAME usage detected"
        
        return " | ".join(parts)
    
    def _extract_trace_path(self, analysis_result: Dict) -> str:
        """Extract propagation trace path"""
        paths = analysis_result.get('propagation_paths', [])
        
        if paths:
            # Get the first/most significant path
            first_path = paths[0]
            return first_path.get('path', '')
        
        # Alternative: build from trace steps
        trace_steps = analysis_result.get('trace_steps', [])
        if trace_steps:
            # Filter for significant steps
            significant_steps = [
                step for step in trace_steps 
                if any(marker in step for marker in ['→', '💾', '📞', '✅'])
            ]
            return ' | '.join(significant_steps[:5])
        
        return ""
    
    def _determine_status(self, analysis_result: Dict) -> str:
        """Determine analysis status based on completeness of tracking"""
        status = analysis_result.get('status', None)
        
        # Check for explicit error statuses
        if status == 'Not Found':
            return 'Not Found'
        elif status and 'error' in str(status).lower():
            return 'Error'
        
        # For successful analysis (no status field or status is None/Unknown)
        # Check if we have complete tracking information
        has_table = bool(analysis_result.get('final_table'))
        has_field = bool(analysis_result.get('final_fields'))
        has_operation = bool(analysis_result.get('database_operations'))
        
        # Special case: VARIABLE is not a real table, it's just a variable assignment
        if has_table and analysis_result.get('final_table') == 'VARIABLE':
            # This is just a variable assignment without DB operation
            return 'Variable Only'
        
        # Complete: Real Z/Y Table + Field + Operation all identified
        if has_table and has_field and has_operation:
            table = analysis_result.get('final_table', '')
            # Only mark as Complete if it's a real custom table (Z* or Y*)
            if table and (table.startswith('Z') or table.startswith('Y')):
                return 'Complete'
            else:
                return 'Partial'  # Standard SAP table or unknown
        # Partial: Some elements identified but not all
        elif has_table or has_field or has_operation or \
             analysis_result.get('tainted_variables') or \
             analysis_result.get('database_sinks') or \
             analysis_result.get('data_flows'):
            return 'Partial'
        else:
            return 'No Findings'
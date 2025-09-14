# ABAP SY-UNAME Taint Tracker

A comprehensive static analysis tool for tracking how the `SY-UNAME` system variable flows through ABAP code and reaches database operations. The tracker performs taint analysis to identify where user identity data ends up in database tables.

## Features

- **Taint Propagation**: Tracks `SY-UNAME` through variable assignments, concatenations, and transformations
- **Inter-procedural Analysis**: Follows taint through FORM routines, FUNCTION modules, and METHOD calls
- **Database Sink Detection**: Identifies INSERT, UPDATE, MODIFY, DELETE operations on real DB tables
- **Configurable Table Filtering**: Focus on custom tables (Z*, Y* by default)
- **Confidence Scoring**: Rates traces as high/medium/low confidence based on directness
- **Detailed Trace Steps**: Records complete propagation path from source to sink

## Installation

Requires Python 3.10+ with only standard library dependencies:

```bash
# Clone the repository
git clone <repository>
cd abap-tracker

# No external dependencies needed - uses only Python stdlib
```

## Usage

### Basic Command Line Usage

```bash
# Analyze with default settings
python abap_sy_uname_tracker.py \
  --root /path/to/abap/codebase \
  --input seeds.csv \
  --output results.json

# With custom table prefixes
python abap_sy_uname_tracker.py \
  --root /path/to/abap/codebase \
  --input seeds.csv \
  --output results.csv \
  --table-prefixes Z,Y,CUSTOM
```

### Input CSV Format

The input CSV must have the following columns:

```csv
file_path,line_number,code_line
program1.abap,45,"lv_user = sy-uname."
program2.abap,123,"MOVE sy-uname TO gv_username."
```

- `file_path`: Path to ABAP file (relative to root or absolute)
- `line_number`: Line number where SY-UNAME is referenced
- `code_line`: Optional - the actual code line (will be read from file if not provided)

### Output Formats

#### JSON Output

```json
{
  "timestamp": "2024-01-01T10:00:00",
  "root_path": "/path/to/codebase",
  "table_prefixes": ["Z", "Y"],
  "traces": [
    {
      "trace_id": "program1_45",
      "start_file": "program1.abap",
      "start_line": 45,
      "trace_steps": [
        {
          "file": "program1.abap",
          "stmt_line_no": 45,
          "stmt_text": "lv_user = sy-uname.",
          "action": "assign"
        },
        {
          "file": "program1.abap",
          "stmt_line_no": 52,
          "stmt_text": "INSERT ztable FROM ls_data.",
          "action": "db"
        }
      ],
      "final_sinks": [
        {
          "type": "INSERT",
          "table_name": "ZTABLE",
          "sink_statement": "INSERT ztable FROM ls_data.",
          "file": "program1.abap",
          "line": 52
        }
      ],
      "confidence": "high",
      "tainted_variables": ["SY-UNAME", "LV_USER", "LS_DATA-USER"]
    }
  ]
}
```

#### CSV Output

| trace_id | start_file | start_line | sink_count | first_sink_type | first_sink_table | confidence | tainted_var_count | trace_step_count |
|----------|------------|------------|------------|-----------------|------------------|------------|-------------------|------------------|
| prog1_45 | prog1.abap | 45 | 1 | INSERT | ZTABLE | high | 3 | 2 |

## How It Works

### 1. Preprocessing
- Reads all ABAP files under the specified root directory
- Removes comments (lines starting with `*` and content after `"`)
- Normalizes whitespace and joins multi-line statements (until `.`)

### 2. Indexing
The tracker builds indices for efficient lookup:
- **Assignment statements**: `=`, `MOVE`, `CONCATENATE`, etc.
- **Call sites**: `CALL FUNCTION`, `CALL METHOD`, `PERFORM`
- **Form/Function/Method definitions**: For inter-procedural analysis
- **Database operations**: `INSERT`, `UPDATE`, `MODIFY`, `DELETE`

### 3. Taint Seeding
From each CSV row, identifies variables assigned from `SY-UNAME`:
- Direct assignment: `lv_user = sy-uname`
- MOVE statement: `MOVE sy-uname TO lv_user`
- Structure field: `ls_data-created_by = sy-uname`

### 4. Propagation Rules
Iteratively propagates taint through:
- **Assignments**: If RHS contains tainted variable, LHS becomes tainted
- **Function calls**: Maps actual to formal parameters
- **Method calls**: Tracks parameters passed to methods
- **FORM routines**: Follows USING/CHANGING parameters

### 5. Sink Detection
Identifies final database operations:
- Direct DB operations on real tables (Z*, Y*, or configured prefixes)
- Excludes internal tables (LT_*, GT_*, IT_*, etc.)
- Handles both static and dynamic table names

### 6. Confidence Scoring
- **High**: Direct use of tainted variable in DB operation (≤3 steps)
- **Medium**: Tainted variable passed through function/method calls
- **Low**: Many propagation steps (>10) or dynamic table names

## Configuration

### Table Prefixes
By default, the tracker considers tables starting with `Z` or `Y` as real database tables. You can configure additional prefixes:

```bash
--table-prefixes Z,Y,CUSTOM,APP
```

### Internal Table Exclusion
The following patterns are automatically excluded as internal tables:
- `LT_*` - Local tables
- `GT_*` - Global tables
- `IT_*` - Internal tables
- `CT_*` - Changing tables
- `TT_*` - Table types
- `LS_*` - Local structures
- `GS_*` - Global structures

## Testing

Run the test suite to validate the tracker:

```bash
# Run all tests
python run_tests.py

# Debug a specific trace
python run_tests.py debug test1_direct_insert_6
```

### Test Coverage
The test suite includes:
1. **Direct insertion**: Immediate use of SY-UNAME in DB operations
2. **FORM propagation**: Taint flow through FORM routines
3. **Function calls**: Tracking through CALL FUNCTION
4. **Complex flows**: Multiple propagation steps and transformations
5. **Dynamic operations**: Handling of dynamic table names

## Iterative Debugging

When a trace fails to find expected sinks, the tracker provides debug information:

1. **Seed statement and tainted variables found**
2. **Regex patterns that didn't match**
3. **Suggestions for pattern improvements**

Use the debug mode to investigate specific traces:

```bash
python run_tests.py debug <trace_id>
```

## Limitations

1. **Static Analysis Only**: Does not execute code or handle runtime behavior
2. **Pattern-Based**: Relies on regex patterns which may miss complex constructs
3. **Local Scope**: Inter-file analysis limited to explicit includes/calls
4. **Dynamic Tables**: Limited support for dynamically determined table names

## Advanced Usage

### Custom Pattern Extension

To add custom patterns, modify the indexer methods:

```python
# Add custom assignment pattern
def _is_assignment(self, stmt):
    patterns = [
        r'\w+\s*=\s*',
        r'MOVE\s+.+\s+TO\s+',
        r'YOUR_CUSTOM_PATTERN',  # Add here
    ]
    return any(re.search(p, stmt.normalized) for p in patterns)
```

### Debugging Tips

1. **Increase verbosity**: Add print statements in propagation methods
2. **Check indices**: Verify statements are properly categorized
3. **Trace steps**: Review the complete propagation path
4. **Pattern testing**: Test regex patterns on isolated statements

## Performance Considerations

- **Codebase Size**: Indexing time scales with number of files
- **Iteration Limit**: Maximum 10 propagation iterations (configurable)
- **File Types**: Processes .abap, .src, .txt, .prog files

## Contributing

To improve the tracker:

1. Add test cases for uncovered patterns
2. Enhance regex patterns for better matching
3. Improve inter-procedural analysis
4. Add support for additional ABAP constructs

## License

[Your License Here]

## Support

For issues or questions:
- Create an issue in the repository
- Check existing test cases for examples
- Review debug output for pattern matching issues
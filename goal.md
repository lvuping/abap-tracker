You are a senior Python engineer experienced with static analysis of ABAP sources.
Task: build a *sy-uname taint-tracker* that, starting from an initial CSV (columns: file_path, code_line — containing or referencing SY-UNAME usage), traces how the value originating from SY-UNAME flows through variables, parameters, function/module calls, methods, FORM routines and finally reaches DB operations (INSERT / UPDATE / MODIFY / DELETE / CALL FUNCTION (RFC) / CALL METHOD that write to DB). The tracker should *only* report final DB writes into real database tables (Z*, Y* or user-configured prefixes). It should ignore ordinary internal tables (lt_*, gt_*, it_*, ct_* etc) unless the internal table ultimately is used to INSERT into a real DB table later in the call chain.

INPUT:
- CSV with at least these columns: file_path, code_line (a snippet or the exact line that references SY-UNAME).
- Access to a codebase folder (all .abap/.src/.txt/.prog files). The script should accept a root path.

OUTPUT:
- CSV/JSON that, for each input row, returns:
  - start_file, start_line
  - trace_id
  - trace_steps: ordered list of {file, stmt_line_no, stmt_text, action} showing how taint propagated
  - final_sinks: list of {type: INSERT|UPDATE|MODIFY|RFC|METHOD|PERFORM, table_name (if known), sink_statement, file, line}
  - confidence: high|medium|low (based on direct use vs indirect/dynamic)

REQUIREMENTS & IMPLEMENTATION DETAILS (step-by-step):
1. Preprocessing:
   - Read all ABAP files under root. Remove commented lines (lines starting with `*` or content after `"`).
   - Normalize whitespace and join continued logical statements: join lines until `.` (ABAP statement terminator). Work at statement-level.

2. Indexing:
   - Build an index of statements per file with line numbers.
   - Build quick indices:
     - assign_statements (MOVE, `=`, CONCATENATE, SHIFT, SPLIT)
     - call_sites (CALL FUNCTION, CALL METHOD, obj->method(...), method(...))
     - form/function/module definitions (FORM .. ENDFORM, FUNCTION .. ENDFUNCTION, CLASS..METHODS)
     - db_ops: statements containing INSERT/UPDATE/MODIFY/DELETE and patterns `INSERT INTO`, `INSERT ztable`, `MODIFY ztable`, `UPDATE ztable` and dynamic forms like `INSERT (lv_tabname)`.

3. Seed taint:
   - From each CSV row, identify the exact variable(s) assigned from SY-UNAME (e.g., `lv_user = sy-uname.`, `MOVE sy-uname TO <lv>`, direct `sy-uname` used in expressions).
   - Seed set: { variable_name } plus sy-uname itself.

4. Propagation rules (iterative):
   - For each statement S in file order (and across files for includes/contains):
     - If RHS contains any tainted symbol, mark LHS symbols tainted.
       - patterns: `LHS = RHS`, `MOVE RHS TO LHS`, `CONCATENATE ... INTO LHS`, `SPLIT ... INTO LHS`, `FIELD-SYMBOLS <fs> ASSIGN ...`
     - If tainted var appears in a CALL FUNCTION / CALL METHOD / method call parameters:
       - Map actual -> formal param names by locating the target definition (function/method/proc). If definition exists in codebase, seed the corresponding formal parameter(s) inside that routine and continue propagation inside the callee.
       - If definition not found (external RFC / standard FM / external class), record a call-site sink with the parameter name and evidence — treat as possible sink (medium confidence) if target FM name starts with `Z`/`Y` or if param is used in TABLES/EXPORTING that may write to DB.

5. Detecting final DB sinks:
   - A statement is a sink if:
     - direct DB operation on a DB table: `INSERT|MODIFY|UPDATE|DELETE` where table name matches configured patterns (`^Z`, `^Y`, or explicit list)
       - capture table name from forms:
         - `INSERT ztable FROM ...`
         - `INSERT INTO ztable ...`
         - `INSERT VALUE #( ... ) INTO ztable.`
         - `MODIFY ztable FROM ...`
         - `UPDATE ztable SET ... WHERE ...`
         - `DELETE FROM ztable WHERE ...`
       - dynamic forms like `INSERT (lv_tabname)` => attempt to resolve lv_tabname if it is assigned with a literal earlier; otherwise mark table as unknown/dynamic (low confidence).
     - CALL FUNCTION 'Z_...' or 'Y_...' (RFC-style) where an argument or table parameter receives a tainted var that is likely used server-side.
     - CALL METHOD or SYSTEM class call that performs DB write — if target method definition is present in codebase, analyze inside it; otherwise mark as possible sink.

6. Postprocessing:
   - Collapse trace into minimal, readable trace_steps (file, line_no, short_text, type=assign/call/db).
   - Compute confidence:
     - high: direct tainted var used in DB-op WHERE or in the payload inserted (clear RHS->insert mapping).
     - medium: tainted var passed to function/method whose name starts with Z/Y or whose body is not available.
     - low: dynamic table names unresolved or ambiguous uses.

7. Filtering DB table names:
   - Only consider tables that match user-configured real-db-table regexes (default: `^[ZY].*` but allow users to pass additional prefixes or explicit table names).
   - Exclude internal table variable patterns: `^(?:LT_|GT_|IT_|CT_|TT_|LS_|GS_).+` (case-insensitive) unless they later flow into an INSERT INTO Z*/Y* table.

8. CLI/Outputs:
   - Provide a CLI: `python abap_sy_uname_tracker.py --root /path/to/src --input seeds.csv --out results.json --table-prefixes Z,Y`
   - Provide a test runner: `pytest` or a small `run_tests.py` that loads the sample ABAP snippets and asserts expected final_sinks.

DELIVERABLES:
- abap_sy_uname_tracker.py (python 3.10+, uses only stdlib: re, csv, json, pathlib, argparse, typing)
- tests/ (several ABAP files and expected JSON)
- README.md describing usage and how to add/adjust regexes and table-prefixes.

ITERATIVE DEBUG INSTRUCTION:
- If a seed trace fails (no sink found but you expect one), produce a focused diff:
  1) show the seed statement and the chain of used variables found
  2) list regex patterns that did not match (assignments / db ops)
  3) suggest minimal regex to add
- Keep changes minimal and re-run tests until each failing test becomes green.

Now implement the script and tests. Provide clear logs when running so user can see which rule matched each step.

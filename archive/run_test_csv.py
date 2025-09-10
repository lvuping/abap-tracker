#!/usr/bin/env python3
"""
Test CSV 기반 SY-UNAME 추적 실행 스크립트

기능:
- input/test.csv 파일을 읽어서 SY-UNAME 분석 실행
- CSV 형식: id, file_path, line_number
- 결과를 output 폴더에 CSV 및 JSON으로 저장

사용법:
    python run_test_csv.py                    # 기본 실행 (JSON + CSV)
    python run_test_csv.py --json-only       # JSON만 출력
    python run_test_csv.py --csv-only        # CSV만 출력
    python run_test_csv.py --verbose         # 상세 출력
"""

import csv
import json
import sys
import os
from datetime import datetime
from analyzer import trace_sy_uname_in_snippet
from encoding_utils import safe_file_read


def process_test_csv(input_file="input/test.csv", output_format="both", verbose=False):
    """
    test.csv 파일을 처리하고 결과를 생성
    
    Args:
        input_file: 입력 CSV 파일 경로
        output_format: "json", "csv", "both" 중 하나
        verbose: 상세 출력 여부
    
    Returns:
        bool: 성공 여부
    """
    all_results = []
    
    # 입력 파일 확인
    if not os.path.exists(input_file):
        print(f"❌ 오류: {input_file} 파일을 찾을 수 없습니다.")
        print(f"   입력 파일을 생성하려면 다음 형식으로 CSV 파일을 만드세요:")
        print(f"   id,file_path,line_number")
        print(f"   1,test_basic,10")
        print(f"   2,user_example,5")
        return False
    
    print(f"🔍 Test CSV 기반 SY-UNAME 추적 분석 시작")
    print(f"📁 입력 파일: {input_file}")
    print("=" * 80)
    
    # CSV 파일 읽기 (자동 인코딩 감지)
    lines, encoding_used = safe_file_read(input_file)
    
    if verbose:
        print(f"📝 파일 인코딩: {encoding_used}")
    
    # CSV 데이터 처리
    import io
    csv_content = io.StringIO(''.join(lines))
    reader = csv.DictReader(csv_content)
    
    # 헤더 확인
    if not reader.fieldnames or not all(field in reader.fieldnames for field in ['id', 'file_path', 'line_number']):
        print(f"❌ 오류: CSV 파일은 'id', 'file_path', 'line_number' 컬럼을 포함해야 합니다.")
        print(f"   현재 컬럼: {reader.fieldnames}")
        return False
    
    # 총 항목 수 계산
    rows = list(reader)
    total_items = len(rows)
    
    if total_items == 0:
        print(f"⚠️ 경고: 처리할 데이터가 없습니다.")
        return False
    
    print(f"📊 총 {total_items}개 항목 발견\n")
    
    # 각 항목 처리
    for idx, row in enumerate(rows, 1):
        try:
            test_id = row['id'].strip()
            file_path = row['file_path'].strip()
            line_number = int(row['line_number'].strip())
            
            # .abap 확장자 자동 추가
            if not file_path.endswith('.abap'):
                file_path = file_path + '.abap'
            
            # input/ 디렉토리 경로 자동 추가
            if not file_path.startswith('input/'):
                file_path = 'input/' + file_path
            
            print(f"📍 [{idx}/{total_items}] ID={test_id}: {os.path.basename(file_path)} (라인 {line_number})")
            
            # ABAP 파일 읽기
            if not os.path.exists(file_path):
                print(f"   ❌ 파일 없음: {file_path}")
                all_results.append({
                    'id': test_id,
                    'source_file': file_path,
                    'source_line': line_number,
                    'result': {
                        'status': 'File Not Found',
                        'error': f'File not found: {file_path}'
                    }
                })
                continue
            
            # 파일 내용 읽기
            all_lines, abap_encoding = safe_file_read(file_path)
            
            if verbose:
                print(f"   📄 ABAP 파일 인코딩: {abap_encoding}")
                print(f"   📝 파일 총 라인 수: {len(all_lines)}")
            
            # 분석할 코드 범위 추출 (앞 200줄, 뒤 1000줄)
            start = max(0, line_number - 201)
            end = min(len(all_lines), line_number + 1000)
            snippet = all_lines[start:end]
            
            # 상대적 라인 번호 계산
            relative_start_line = line_number - start - 1
            
            # SY-UNAME 추적 분석 실행
            result = trace_sy_uname_in_snippet(snippet, relative_start_line)
            
            # 결과 출력
            if result['status'] == 'Found':
                if result['type'] == 'RFC':
                    print(f"   ✅ RFC 호출 발견: {result.get('name', 'Unknown')}")
                elif result['type'] == 'AUDIT_FIELD':
                    print(f"   ✅ 감사 필드: {result.get('structure')}-{result.get('field')}")
                elif result['type'] in ['DATABASE_UPDATE_FIELD', 'DATABASE_INSERT_FIELD', 
                                       'DATABASE_MODIFY_FIELD', 'DATABASE_SELECT_WHERE']:
                    fields = ', '.join(result.get('fields', []))
                    print(f"   🎯 데이터베이스: {result.get('table')}.{fields} ({result.get('operation', 'UNKNOWN')})")
                elif result['type'].startswith('DATABASE_'):
                    print(f"   ✅ 데이터베이스: {result.get('operation', 'UNKNOWN')} {result.get('table')}")
                else:
                    print(f"   ✅ Sink 발견: {result['type']}")
                    
                if verbose and result.get('trace_path'):
                    print(f"   📝 추적 경로 ({len(result['trace_path'])}단계):")
                    for step in result['trace_path'][:5]:  # 처음 5단계만 표시
                        print(f"      • {step}")
                        
            elif result['status'] == 'Scope Boundary Reached':
                boundary_type = result.get('type', 'Unknown')
                print(f"   ⛔ 스코프 경계 도달: {boundary_type}")
                if verbose:
                    print(f"      라인: {result.get('boundary_line', 'Unknown')}")
                    
            elif result.get('error_type') == 'SYUNAME_NOT_AT_SPECIFIED_LINE':
                print(f"   ❌ 지정된 라인에 SY-UNAME 없음")
                if verbose:
                    print(f"      실제 내용: '{result.get('actual_content', 'N/A')}'")
                    
            elif result.get('error_type') == 'NO_SINK_FOUND_AFTER_TRACING':
                print(f"   ⚠️ SY-UNAME 추적됨 but Sink 미발견")
                if verbose:
                    analysis = result.get('analysis_summary', {})
                    print(f"      분석된 문장: {analysis.get('total_statements_analyzed', 0)}개")
                    print(f"      추적 단계: {analysis.get('trace_steps', 0)}단계")
            else:
                print(f"   ⚠️ 분석 실패: {result.get('reason', 'Unknown')}")
            
            # 결과 저장
            all_results.append({
                'id': test_id,
                'source_file': file_path,
                'source_line': line_number,
                'result': result
            })
            
        except Exception as e:
            print(f"   ❌ 처리 오류: {str(e)}")
            all_results.append({
                'id': row.get('id', 'Unknown'),
                'source_file': row.get('file_path', 'Unknown'),
                'source_line': row.get('line_number', 0),
                'result': {
                    'status': 'Error',
                    'error': str(e)
                }
            })
    
    # 결과 저장
    print("\n" + "=" * 80)
    print("💾 결과 저장 중...")
    
    output_dir = 'output'
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    
    # JSON 출력
    if output_format in ['json', 'both']:
        json_file = f"{output_dir}/test_results_{timestamp}.json"
        with open(json_file, 'w', encoding='utf-8') as f:
            json.dump(all_results, f, indent=2, ensure_ascii=False)
        print(f"📄 JSON 결과 저장: {json_file}")
    
    # CSV 출력
    if output_format in ['csv', 'both']:
        csv_file = f"{output_dir}/test_results_{timestamp}.csv"
        export_to_csv(all_results, csv_file)
        print(f"📊 CSV 결과 저장: {csv_file}")
    
    # 결과 요약
    print("\n" + "=" * 80)
    print("🎉 분석 완료!")
    print(f"📊 분석 요약:")
    
    found_count = sum(1 for r in all_results if r['result'].get('status') == 'Found')
    boundary_count = sum(1 for r in all_results if r['result'].get('status') == 'Scope Boundary Reached')
    not_found_count = sum(1 for r in all_results if r['result'].get('error_type') == 'SYUNAME_NOT_AT_SPECIFIED_LINE')
    no_sink_count = sum(1 for r in all_results if r['result'].get('error_type') == 'NO_SINK_FOUND_AFTER_TRACING')
    error_count = sum(1 for r in all_results if r['result'].get('status') in ['Error', 'File Not Found'])
    
    print(f"  • 총 분석: {len(all_results)}개")
    print(f"  • ✅ Sink 발견: {found_count}개")
    print(f"  • ⛔ 스코프 경계: {boundary_count}개")
    print(f"  • ❌ SY-UNAME 없음: {not_found_count}개")
    print(f"  • ⚠️ Sink 미발견: {no_sink_count}개")
    print(f"  • ❌ 오류: {error_count}개")
    
    return True


def export_to_csv(results, output_file):
    """
    분석 결과를 CSV 파일로 저장
    """
    if not results:
        return
    
    csv_rows = []
    max_trace_steps = 0
    
    for item in results:
        result = item['result']
        
        row = {
            'ID': item['id'],
            'Source_File': item['source_file'],
            'Line_Number': item['source_line'],
            'Status': result.get('status', 'Unknown'),
            'Type': result.get('type', ''),
            'Table': result.get('table', ''),
            'Fields': ', '.join(result.get('fields', [])) if result.get('fields') else '',
            'RFC_Name': result.get('name', '') if result.get('type') == 'RFC' else '',
            'Operation': result.get('operation', ''),
            'Description': result.get('description', ''),
            'Error': result.get('error', ''),
            'Trace_Steps': len(result.get('trace_path', result.get('path', [])))
        }
        
        # 추적 경로 추가 (최대 5단계)
        trace_path = result.get('trace_path', result.get('path', []))
        for i, step in enumerate(trace_path[:5], 1):
            row[f'Trace_Step_{i}'] = step
            max_trace_steps = max(max_trace_steps, i)
        
        csv_rows.append(row)
    
    # CSV 파일 작성
    if csv_rows:
        # 기본 필드 이름 정의
        fieldnames = [
            'ID', 'Source_File', 'Line_Number', 'Status', 'Type', 
            'Table', 'Fields', 'RFC_Name', 'Operation', 
            'Description', 'Error', 'Trace_Steps'
        ]
        
        # 추적 단계 필드 추가
        for i in range(1, max_trace_steps + 1):
            fieldnames.append(f'Trace_Step_{i}')
        
        with open(output_file, 'w', newline='', encoding='utf-8-sig') as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            # 모든 필드가 있도록 보장
            for row in csv_rows:
                complete_row = {field: row.get(field, '') for field in fieldnames}
                writer.writerow(complete_row)


def create_sample_test_csv():
    """
    샘플 test.csv 파일 생성
    """
    sample_file = 'input/test.csv'
    
    print(f"📝 샘플 test.csv 파일 생성 중...")
    
    sample_data = [
        ['id', 'file_path', 'line_number'],
        ['1', 'test_basic', '10'],
        ['2', 'user_example', '5'],
        ['3', '01_database_insert', '50'],
        ['4', '02_database_update', '45'],
        ['5', 'test_complex_flow', '25']
    ]
    
    with open(sample_file, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerows(sample_data)
    
    print(f"✅ 샘플 파일 생성 완료: {sample_file}")
    print(f"   이 파일을 수정하여 테스트할 위치를 지정하세요.")
    return sample_file


def main():
    """
    메인 함수
    """
    print("🚀 Test CSV 기반 SY-UNAME 추적기")
    print(f"📅 실행 시간: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # 명령행 인자 처리
    output_format = 'both'  # 기본값
    verbose = False
    create_sample = False
    
    if '--json-only' in sys.argv:
        output_format = 'json'
    elif '--csv-only' in sys.argv:
        output_format = 'csv'
    
    if '--verbose' in sys.argv or '-v' in sys.argv:
        verbose = True
    
    if '--create-sample' in sys.argv:
        create_sample = True
    
    # 샘플 파일 생성 옵션
    if create_sample:
        create_sample_test_csv()
        return 0
    
    # test.csv 파일 확인
    test_file = 'input/test.csv'
    if not os.path.exists(test_file):
        print(f"⚠️ {test_file} 파일이 없습니다.")
        print(f"   샘플 파일을 생성하려면 다음 명령을 실행하세요:")
        print(f"   python {sys.argv[0]} --create-sample")
        return 1
    
    # 분석 실행
    success = process_test_csv(test_file, output_format, verbose)
    
    if not success:
        return 1
    
    return 0


if __name__ == '__main__':
    exit(main())
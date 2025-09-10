#!/usr/bin/env python3
"""
ABAP Tracker - 간소화된 메인 실행 파일
"""

import os
import sys
import json
import csv
import argparse
from datetime import datetime
# from test_runner import TestRunner  # TODO: Create this module if needed
from unified_analyzer import UnifiedAnalyzer
from encoding_utils import safe_file_read


def run_test():
    """테스트 모드 실행"""
    print("\n" + "="*80)
    print("🚀 ABAP Tracker 테스트 실행")
    print("="*80)
    
    # TODO: Implement TestRunner module
    print("❌ TestRunner 모듈이 구현되지 않았습니다.")
    print("대신 --batch 옵션을 사용하세요:")
    print("  python abap_tracker.py --batch input/sy_uname_locations.csv")
    return
    
    # runner = TestRunner(verbose=False)
    # test_cases = runner.load_test_cases('input/sy_uname_locations.csv')
    # 
    # if not test_cases:
    #     print("❌ 테스트 케이스를 로드할 수 없습니다.")
    #     return
    # 
    # runner.run_all_tests(test_cases)
    # 
    # # 결과 저장
    # timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    # csv_file = f"output/test_results_{timestamp}.csv"
    # json_file = f"output/test_report_{timestamp}.json"
    # 
    # runner.export_csv(csv_file)
    # runner.export_json(json_file)
    # 
    # print(f"\n📊 결과 파일:")
    # print(f"  • CSV: {csv_file}")
    # print(f"  • JSON: {json_file}")


def run_analysis(file_path, line_number, verbose=False):
    """단일 파일 분석"""
    if not os.path.exists(file_path):
        print(f"❌ 파일을 찾을 수 없습니다: {file_path}")
        return None
    
    print(f"\n📍 분석 중: {file_path}:{line_number}")
    
    # 자동 인코딩 감지로 파일 읽기
    lines, encoding_used = safe_file_read(file_path)
    if verbose:
        print(f"  📄 파일 인코딩: {encoding_used}")
    
    if line_number > len(lines):
        print(f"❌ 라인 {line_number}이 파일 범위를 벗어났습니다.")
        return None
    
    analyzer = UnifiedAnalyzer(seed_variables=['SY-UNAME'])
    result = analyzer.analyze(lines, line_number - 1)  # 0-based index
    
    if verbose:
        print("\n📋 분석 결과:")
        print(f"  • 오염된 변수: {len(result.get('tainted_variables', []))}")
        print(f"  • 데이터 흐름: {len(result.get('data_flows', []))}")
        print(f"  • DB 작업: {len(result.get('database_sinks', []))}")
        
        if result.get('database_sinks'):
            print("\n💾 데이터베이스 작업:")
            for sink in result['database_sinks'][:5]:
                fields = ', '.join(sink['fields']) if sink['fields'] else 'ALL'
                print(f"  • {sink['operation']} {sink['table']} (Line {sink['line']}, Fields: {fields})")
    
    return result


def batch_analysis(csv_file, output_format='json', verbose=False):
    """CSV 파일 기반 일괄 분석"""
    if not os.path.exists(csv_file):
        print(f"❌ CSV 파일을 찾을 수 없습니다: {csv_file}")
        return
    
    print(f"\n📂 일괄 분석 시작: {csv_file}")
    
    results = []
    # CSV 파일 자동 인코딩 감지
    lines, csv_encoding = safe_file_read(csv_file)
    if verbose:
        print(f"  📄 CSV 파일 인코딩: {csv_encoding}")
    
    import io
    csv_content = io.StringIO(''.join(lines))
    reader = csv.DictReader(csv_content)
    total = len(lines) - 1  # 헤더 제외
    
    for i, row in enumerate(reader, 1):
        file_path = row.get('file_path', '').strip()
        if not file_path.endswith('.abap'):
            file_path += '.abap'
        if not file_path.startswith('input/'):
            file_path = 'input/' + file_path
        
        line_number = int(row.get('line_number', 0))
        
        if not verbose:
            print(f"  [{i}/{total}] {os.path.basename(file_path)}:{line_number}", end='\r')
        
        result = run_analysis(file_path, line_number, verbose=False)
        if result:
            results.append({
                'id': row.get('id', i),
                'file': file_path,
                'line': line_number,
                'result': result
            })
    
    print(f"\n✅ 분석 완료: {len(results)}개 항목")
    
    # 결과 저장
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    if output_format in ['json', 'both']:
        json_file = f"output/analysis_{timestamp}.json"
        with open(json_file, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        print(f"  • JSON 저장: {json_file}")
    
    if output_format in ['csv', 'both']:
        csv_file = f"output/analysis_{timestamp}.csv"
        with open(csv_file, 'w', newline='', encoding='utf-8') as f:
            fieldnames = ['id', 'file', 'line', 'status', 'type', 'table', 'fields']
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            
            for item in results:
                res = item['result']
                row = {
                    'id': item['id'],
                    'file': item['file'],
                    'line': item['line'],
                    'status': res.get('status', 'Unknown'),
                    'type': res.get('type', ''),
                    'table': '',
                    'fields': ''
                }
                
                if res.get('database_sinks'):
                    sink = res['database_sinks'][0]
                    row['table'] = sink['table']
                    row['fields'] = ';'.join(sink['fields'])
                
                writer.writerow(row)
        print(f"  • CSV 저장: {csv_file}")


def main():
    parser = argparse.ArgumentParser(
        description='ABAP Tracker - SY-UNAME 추적 및 DB 작업 분석',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''
사용 예시:
  python abap_tracker.py --test                    # 테스트 실행
  python abap_tracker.py --file test.abap --line 10  # 단일 파일 분석
  python abap_tracker.py --batch input/locations.csv # 일괄 분석
  python abap_tracker.py                           # 기본 테스트 실행
        '''
    )
    
    parser.add_argument('--test', action='store_true', 
                       help='전체 테스트 실행')
    parser.add_argument('--file', type=str,
                       help='분석할 ABAP 파일')
    parser.add_argument('--line', type=int,
                       help='SY-UNAME이 있는 라인 번호')
    parser.add_argument('--batch', type=str,
                       help='일괄 분석할 CSV 파일')
    parser.add_argument('--csv', action='store_true',
                       help='CSV 형식으로 출력')
    parser.add_argument('--json', action='store_true',
                       help='JSON 형식으로 출력 (기본값)')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='상세 출력')
    
    args = parser.parse_args()
    
    # 출력 형식 결정
    output_format = 'json'  # 기본값
    if args.csv and args.json:
        output_format = 'both'
    elif args.csv:
        output_format = 'csv'
    
    # 실행 모드 결정
    if args.test or (not args.file and not args.batch):
        # 테스트 모드 (기본)
        run_test()
    elif args.file and args.line:
        # 단일 파일 분석
        result = run_analysis(args.file, args.line, args.verbose)
        if result and output_format != 'none':
            print(json.dumps(result, indent=2, ensure_ascii=False))
    elif args.batch:
        # 일괄 분석
        batch_analysis(args.batch, output_format, args.verbose)
    else:
        # 인자가 부족한 경우
        if args.file and not args.line:
            print("❌ --file 옵션을 사용할 때는 --line도 필요합니다.")
        else:
            parser.print_help()


if __name__ == '__main__':
    main()
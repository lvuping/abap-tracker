#!/usr/bin/env python3
"""
ABAP Tracker 테스트 러너
"""

import os
import csv
import json
from datetime import datetime
from typing import List, Dict, Optional
from unified_analyzer import UnifiedAnalyzer


class TestRunner:
    """테스트 러너"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.results = []
        self.analyzer = UnifiedAnalyzer(seed_variables=['SY-UNAME'])
        
    def load_test_cases(self, csv_file: str) -> List[Dict]:
        """CSV 파일에서 테스트 케이스 로드"""
        test_cases = []
        
        if not os.path.exists(csv_file):
            print(f"❌ 테스트 파일을 찾을 수 없습니다: {csv_file}")
            return test_cases
        
        with open(csv_file, 'r', encoding='utf-8') as f:
            reader = csv.DictReader(f)
            for row in reader:
                test_case = {
                    'id': int(row.get('id', 0)),
                    'file': row.get('file_path', '').strip(),
                    'line': int(row.get('line_number', 0))
                }
                
                # 파일 경로 정규화
                if not test_case['file'].endswith('.abap'):
                    test_case['file'] += '.abap'
                if not test_case['file'].startswith('input/'):
                    test_case['file'] = 'input/' + test_case['file']
                
                test_cases.append(test_case)
        
        return test_cases
    
    def run_single_test(self, test_case: Dict) -> Dict:
        """단일 테스트 실행"""
        result = {
            'id': test_case['id'],
            'file': test_case['file'],
            'line': test_case['line'],
            'found': False,
            'passed': False,
            'type': None,
            'table': None,
            'fields': [],
            'message': ''
        }
        
        # 파일 존재 확인
        if not os.path.exists(test_case['file']):
            result['message'] = f"File not found: {test_case['file']}"
            return result
        
        # 파일 읽기
        with open(test_case['file'], 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()
        
        # 라인 범위 확인
        if test_case['line'] > len(lines):
            result['message'] = f"Line {test_case['line']} out of range"
            return result
        
        # 분석 실행
        analysis_result = self.analyzer.analyze(lines, test_case['line'] - 1)
        
        # 결과 검증
        if analysis_result.get('status') == 'Not Found':
            result['message'] = analysis_result.get('reason', 'Analysis failed')
            return result
        
        # 데이터베이스 작업 확인
        sinks = analysis_result.get('database_sinks', [])
        flows = analysis_result.get('data_flows', [])
        
        if sinks:
            # 관련 sink 찾기
            relevant_sinks = [
                sink for sink in sinks 
                if abs(sink['line'] - test_case['line']) <= 50
            ]
            
            if relevant_sinks:
                sink = relevant_sinks[0]
                result['found'] = True
                result['passed'] = True
                result['type'] = f"DB-{sink['operation']}"
                result['table'] = sink['table']
                result['fields'] = sink['fields']
        
        elif flows:
            # 데이터 흐름 확인
            relevant_flows = [
                flow for flow in flows
                if abs(flow['line'] - test_case['line']) <= 20
            ]
            
            if relevant_flows:
                flow = relevant_flows[0]
                result['found'] = True
                result['passed'] = True
                result['type'] = flow['operation']
        
        elif analysis_result.get('tainted_variables'):
            # 오염된 변수만 있는 경우
            result['found'] = True
            result['passed'] = True
            result['type'] = 'ASSIGNMENT'
        
        if not result['found']:
            result['message'] = 'No relevant flow or sink found'
        
        return result
    
    def run_all_tests(self, test_cases: List[Dict]) -> None:
        """모든 테스트 실행"""
        print(f"총 {len(test_cases)}개 테스트 실행 중...\n")
        
        for i, test_case in enumerate(test_cases, 1):
            if self.verbose:
                print(f"[{i}/{len(test_cases)}] {test_case['file']}:{test_case['line']}")
            
            result = self.run_single_test(test_case)
            self.results.append(result)
        
        self.print_summary()
    
    def print_summary(self) -> None:
        """테스트 요약 출력"""
        total = len(self.results)
        found = len([r for r in self.results if r['found']])
        passed = len([r for r in self.results if r['passed']])
        
        accuracy = (found / total * 100) if total > 0 else 0
        
        print("\n" + "=" * 60)
        print("테스트 결과")
        print("=" * 60)
        print(f"총 테스트: {total}")
        print(f"성공: {found} ({accuracy:.1f}%)")
        
        # 실패 원인 요약
        failures = [r for r in self.results if not r['passed']]
        if failures and self.verbose:
            print("\n실패 원인:")
            failure_types = {}
            for f in failures:
                msg = f['message'] or 'Unknown'
                failure_types[msg] = failure_types.get(msg, 0) + 1
            
            for msg, count in sorted(failure_types.items(), key=lambda x: x[1], reverse=True)[:5]:
                print(f"  • {msg}: {count}건")
    
    def export_csv(self, output_file: str) -> None:
        """결과를 CSV로 내보내기"""
        with open(output_file, 'w', newline='', encoding='utf-8') as f:
            fieldnames = ['id', 'file', 'line', 'found', 'type', 'table', 'fields']
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            
            writer.writeheader()
            for result in self.results:
                result_copy = result.copy()
                result_copy['fields'] = ';'.join(result['fields']) if result['fields'] else ''
                result_copy.pop('passed', None)
                result_copy.pop('message', None)
                writer.writerow(result_copy)
        
        print(f"CSV 결과: {output_file}")
    
    def export_json(self, output_file: str) -> None:
        """결과를 JSON으로 내보내기"""
        report = {
            'timestamp': datetime.now().isoformat(),
            'summary': {
                'total': len(self.results),
                'found': len([r for r in self.results if r['found']]),
                'accuracy': f"{(len([r for r in self.results if r['found']]) / len(self.results) * 100):.1f}%" if self.results else "0%"
            },
            'results': self.results
        }
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(report, f, indent=2, ensure_ascii=False)
        
        print(f"JSON 결과: {output_file}")
"""
Unit tests for ABAP SY-UNAME Tracker Improvements
Tests INSERT VALUES pattern parsing and taint propagation optimization
"""

import unittest
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

from insert_values_handler import InsertValuesHandler, InsertValuesPattern
from taint_propagation_optimizer import TaintPropagationOptimizer, PropagationType
from csv_analyzer import EnhancedCSVAnalyzer


class TestInsertValuesHandler(unittest.TestCase):
    """Test INSERT VALUES pattern improvements"""
    
    def setUp(self):
        self.handler = InsertValuesHandler()
    
    def test_value_constructor_pattern(self):
        """Test VALUE # constructor pattern parsing"""
        code = [
            "INSERT ztable FROM @( VALUE #(",
            "  field1 = 'VALUE1'",
            "  created_by = sy-uname",
            "  created_on = sy-datum ) )."
        ]
        
        patterns = self.handler.analyze_insert_values(code)
        
        self.assertEqual(len(patterns), 1)
        pattern = patterns[0]
        self.assertEqual(pattern.table, 'ZTABLE')
        self.assertEqual(pattern.pattern_type, 'value_constructor')
        self.assertIn('CREATED_BY', pattern.fields)
        self.assertEqual(pattern.fields['CREATED_BY'], 'sy-uname')
        self.assertGreater(pattern.confidence, 0.90)
    
    def test_direct_values_pattern(self):
        """Test direct INSERT VALUES pattern"""
        code = [
            "INSERT INTO zuser_log VALUES (",
            "  sy-mandt,",
            "  sy-uname,",
            "  'LOGIN',",
            "  sy-uzeit )."
        ]
        
        patterns = self.handler.analyze_insert_values(code)
        
        self.assertEqual(len(patterns), 1)
        pattern = patterns[0]
        self.assertEqual(pattern.table, 'ZUSER_LOG')
        self.assertEqual(pattern.pattern_type, 'direct_values')
        self.assertTrue(len(pattern.fields) >= 4)
    
    def test_multi_values_pattern(self):
        """Test multiple VALUES in one INSERT"""
        code = [
            "INSERT INTO ztransaction_log VALUES",
            "  ( '001', sy-uname, 'CREATE', sy-datum ),",
            "  ( '002', lv_user, 'UPDATE', sy-datum ),",
            "  ( '003', sy-uname, 'DELETE', sy-datum )."
        ]
        
        patterns = self.handler.analyze_insert_values(code)
        
        self.assertGreater(len(patterns), 0)
        for pattern in patterns:
            self.assertEqual(pattern.table, 'ZTRANSACTION_LOG')
            self.assertEqual(pattern.pattern_type, 'multi_values')
    
    def test_tainted_field_detection(self):
        """Test detection of tainted fields"""
        pattern = InsertValuesPattern(
            table='ZTABLE',
            fields={
                'CREATED_BY': 'sy-uname',
                'CREATED_DATE': 'sy-datum',
                'STATUS': "'ACTIVE'"
            },
            line_number=10,
            pattern_type='value_constructor',
            confidence=0.95
        )
        
        tainted_fields = self.handler.find_tainted_fields(pattern, ['LV_USER'])
        
        self.assertIn('CREATED_BY', tainted_fields)
        self.assertIn('CREATED_DATE', tainted_fields)
        self.assertNotIn('STATUS', tainted_fields)


class TestTaintPropagationOptimizer(unittest.TestCase):
    """Test taint propagation optimization"""
    
    def setUp(self):
        self.optimizer = TaintPropagationOptimizer()
    
    def test_direct_taint(self):
        """Test direct taint from system variable"""
        result = self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment from SY-UNAME')
        
        self.assertTrue(result)
        self.assertTrue(self.optimizer.is_tainted('LV_USER'))
        self.assertEqual(len(self.optimizer.tainted_variables), 1)
        
        var = self.optimizer.tainted_variables['LV_USER']
        self.assertEqual(var.confidence, 1.0)
        self.assertEqual(var.propagation_depth, 0)
    
    def test_structure_field_taint(self):
        """Test structure field taint propagation"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 11, 'Field assignment', 
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        
        self.assertTrue(self.optimizer.is_tainted('WA_TABLE-CREATED_BY'))
        self.assertTrue(self.optimizer.is_tainted('WA_TABLE'))
        
        # Check structure is marked correctly
        wa_table = self.optimizer.tainted_variables.get('WA_TABLE')
        self.assertIsNotNone(wa_table)
        self.assertTrue(wa_table.is_structure)
        self.assertIn('CREATED_BY', wa_table.affected_fields)
    
    def test_redundancy_prevention(self):
        """Test that redundant propagations are prevented"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        
        # First propagation
        result1 = self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 11, 'Field assignment',
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        
        # Redundant propagation (same source, later line)
        result2 = self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 15, 'Redundant assignment',
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        
        self.assertTrue(result1)
        self.assertFalse(result2)
    
    def test_confidence_decay(self):
        """Test confidence decay through propagation levels"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 11, 'Field assignment',
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        self.optimizer.mark_tainted(
            'LT_TABLE', 12, 'Append work area',
            'WA_TABLE', PropagationType.INTERNAL_TABLE
        )
        
        lv_user = self.optimizer.tainted_variables['LV_USER']
        wa_field = self.optimizer.tainted_variables['WA_TABLE-CREATED_BY']
        lt_table = self.optimizer.tainted_variables['LT_TABLE']
        
        self.assertEqual(lv_user.confidence, 1.0)
        self.assertLess(wa_field.confidence, lv_user.confidence)
        self.assertLess(lt_table.confidence, wa_field.confidence)
        self.assertGreater(lt_table.confidence, 0.85)  # Still high confidence
    
    def test_alias_tracking(self):
        """Test alias relationship tracking"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        self.optimizer.add_alias('LV_TEMP_USER', 'LV_USER')
        
        self.assertTrue(self.optimizer.is_tainted('LV_TEMP_USER'))
        
        source = self.optimizer.get_taint_source('LV_TEMP_USER')
        self.assertIsNotNone(source)
        self.assertEqual(source.line, 10)
    
    def test_clean_taint(self):
        """Test taint removal (CLEAR statement)"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 11, 'Field assignment',
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        
        # Clean the source variable
        self.optimizer.clean_taint('LV_USER')
        
        self.assertFalse(self.optimizer.is_tainted('LV_USER'))
        self.assertTrue(self.optimizer.is_tainted('WA_TABLE-CREATED_BY'))  # Child still tainted
    
    def test_propagation_summary(self):
        """Test propagation summary generation"""
        self.optimizer.mark_tainted('LV_USER', 10, 'Direct assignment')
        self.optimizer.mark_tainted(
            'WA_TABLE-CREATED_BY', 11, 'Field assignment',
            'LV_USER', PropagationType.STRUCTURE_FIELD
        )
        
        summary = self.optimizer.get_propagation_summary()
        
        self.assertEqual(summary['total_tainted'], 3)  # LV_USER, WA_TABLE-CREATED_BY, WA_TABLE
        self.assertEqual(summary['system_sources'], 1)
        self.assertEqual(summary['structures'], 1)
        self.assertGreater(summary['high_confidence'], 0)


class TestIntegration(unittest.TestCase):
    """Integration tests for the complete system"""
    
    def setUp(self):
        self.analyzer = EnhancedCSVAnalyzer()
    
    def test_complete_flow(self):
        """Test complete analysis flow with improvements"""
        code_snippet = [
            "DATA: lv_user TYPE sy-uname.",
            "lv_user = sy-uname.",
            "INSERT ztable FROM @( VALUE #(",
            "  created_by = lv_user",
            "  created_date = sy-datum ) )."
        ]
        
        # Analyze with line 1 (where sy-uname assignment is)
        result = self.analyzer.analyze_location(
            id_value='TEST001',
            file_path='test.abap',
            code_snippet=code_snippet,
            line_number=1,
            actual_line_number=10
        )
        
        self.assertIsNotNone(result)
        self.assertEqual(result.source_file, 'test.abap')
        self.assertEqual(result.line_number, 10)
        
        # Check if INSERT VALUES was detected
        if result.tables:
            self.assertIn('ZTABLE', result.tables)
        
        # Check if confidence is properly calibrated
        self.assertGreater(result.confidence, 0.60)


def run_tests():
    """Run all tests and return results"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestInsertValuesHandler))
    suite.addTests(loader.loadTestsFromTestCase(TestTaintPropagationOptimizer))
    suite.addTests(loader.loadTestsFromTestCase(TestIntegration))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result


if __name__ == '__main__':
    result = run_tests()
    
    # Print summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success rate: {((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100):.1f}%")
    
    # Exit with appropriate code
    sys.exit(0 if result.wasSuccessful() else 1)
#!/usr/bin/python
import os
import os.path

def run_test_sources():
	failed = []
	for root, dirs, files in os.walk('tests/lang'):
		for f in files:
			if f.endswith('.gf'):
				source_path = os.path.join(root, f)
				expected_path = source_path + '.expected'
				actual_path = source_path + '.actual'
				if not os.path.exists(expected_path):
					raise Exception(expected_path + ' doesn''t exist')
				os.system('./goofy "%s" > "%s"' % (source_path, actual_path))
				retcode = os.system('diff "%s" "%s" &> /dev/null' % (expected_path, actual_path))
				if retcode != 0:
					failed.append((source_path, expected_path, actual_path))
	if failed == []:
		print("All tests passed")
	else:
		for s, e, a in failed:
			print("Source %s failed" % s)

if __name__ == '__main__':
	run_test_sources()



















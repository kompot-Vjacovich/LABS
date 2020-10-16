import subprocess
import sys
import traceback

def check_sys():
	reqs = subprocess.check_output([sys.executable, '-m', 'pip', 'freeze'])
	return [r.decode().split('==')[0] for r in reqs.split()]

def parseScript(filename):
	modules = []
	for line in open(filename, "r"):
		values = line.split(" ")
		if values[0] == "from":
			modules.append(values[1].replace("\n", ""))
		if values[0] == "import":
			packages = values[1:]
			for x in packages:
				modules.append(x.replace(",", "").replace("\n", ""))
	return modules

if __name__ == '__main__':
	if len(sys.argv) < 2:
		print(f"Usage: {sys.argv[0]} <path/to/file.py>")
		sys.exit(1)
	filepath = sys.argv[1]

	imported = parseScript(filepath)

	installed = list(set(check_sys()) | set(sys.builtin_module_names))
	missing = list(set(imported) - set(installed))
	
	errors = []
	cnt = 0
	for lib in missing:
		try:
			res = subprocess.run(['pip', 'install', lib], capture_output=True)
		except:
			errors.append(traceback.format_exc())
		else: 
			cnt += 1

	print(f"{cnt} scripts were installed")
	if len(missing) - cnt:
		print(f"{len(missing) - cnt} scripts were not installed")
	for e in errors:
		print(e)
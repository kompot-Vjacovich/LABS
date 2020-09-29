import subprocess
import sys
import pkutil

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
	filepath = "test.py"
	imported = parseScript(filepath)
	installed = check_sys()
	result = list(set(imported) - set(installed))
	# print(installed)
	# print(imported)
	print(pkgutil.iter_modules())
	print(result)
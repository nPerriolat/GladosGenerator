#!/usr/bin/env python3

from pathlib import Path
from os import remove
import subprocess
import sys

RED : str = "\033[0;31m"
YELLOW : str = "\033[1;33m"
GREEN : str = "\033[0;32m"
BLANK : str = "\033[0m"
EXAMPLES_DIR : str = "examples"
EXPECTED_DIR : str = "examples-expected"

def example_couple(path : Path) -> tuple[Path, Path]:
    return (path, Path(str(path).replace(EXAMPLES_DIR, EXPECTED_DIR).replace(".scm", ".txt")))

TESTS : list[str] = list(map(example_couple, list(Path(EXAMPLES_DIR).rglob("*.scm"))))
GLADOS : str = "./glados"
COMPILED_FILE : str = "./.compiled.bin"

if len(TESTS) == 0:
    print(f"No .scm test file in {EXAMPLES_DIR}")
    quit()

def group_multiline_string(lines : list[str]) -> list[str]:
    new : list[str] = []
    is_in_str : bool = False

    for line in lines:
        if is_in_str:
            new[-1] += line
            if "'" in line:
                is_in_str = False
            continue
        new.append(line)
        is_in_str = line.count("'") % 2 == 1
    return new

def parse_expected(path : Path) -> dict[str, str]:
        config = { key : value for key, value in map(lambda line: map(str.strip, line.strip("\n").split(":", 1)), group_multiline_string(open(str(path), "r").readlines())) }
        if "RETCODE" not in config:
            config.update({ "RETCODE" : 0 })
        else:
            if config["RETCODE"].lower() == "nonzero":
                config["RETCODE"] = "nonzero"
            else:
                config["RETCODE"] = int(config["RETCODE"])
        if "COMPILE" not in config:
            config.update({ "COMPILE" : "YES" })
        else:
            config["COMPILE"] = config["COMPILE"].upper()
            if config["COMPILE"] not in ("YES", "NO"):
                print(f"COMPILE setting can only be YES or NO (case-insensitive), but got '{config['COMPILE']}' !", file = sys.stderr)
                exit(1)
        for output in ("STDOUT", "STDERR"):
            if output in config:
                config[output] = config[output].strip("'")
        return config

def check_return_code(expected : str | int, actual : int) -> bool:
    if expected == "nonzero":
        return actual != 0
    return actual == expected

n_passed = 0

for i, (test, expected) in enumerate(TESTS):
    if i > 0:
        print()
    config = parse_expected(expected)
    print(f"Test {i+1}: {GLADOS} {test}")
    passed = True
    compile = subprocess.run([GLADOS, "-c", str(test), COMPILED_FILE], stdout=subprocess.PIPE, stderr = subprocess.PIPE)
    compiled = True
    if compile.returncode != 0:
        if config["COMPILE"] == "NO":
            compiled = False
            if "STDERR" in config:
                output = compile.stderr.decode("utf-8").strip("\n")
                expected = config["STDERR"].strip("'")
                if output != expected:
                    print(f"--> {RED}Got STDERR '{output}' but expected '{expected}'{BLANK}")
                    passed = False
        else:
            print(f"--> {RED}Cannot compile {test} !{BLANK}\t" + compile.stderr.decode("utf-8"))
            continue

    if compiled:
        run = subprocess.run([GLADOS, "-r", COMPILED_FILE], stdout = subprocess.PIPE, stderr = subprocess.PIPE)
        if not check_return_code(config["RETCODE"], run.returncode):
            print(f"--> {RED}Got return code {run.returncode} but expected {config['RETCODE'] if config['RETCODE'] != 'nonzero' else "!= 0"}{BLANK}", file = sys.stderr)
            passed = False
        outputs = [("STDOUT", run.stdout), ("STDERR", run.stderr)]
        for output_name, output in outputs:
            output = output.decode("utf-8").strip("\n")
            output = output.strip("'")
            if output_name in config and output != config[output_name]:
                print(f"--> {RED}Got {output_name} '{output}' but expected '{config[output_name].strip("'")}'{BLANK}")
                passed = False
        if not passed and "STDERR" not in config:
            if run.stderr.decode("utf-8") != "":
                print(f"--> {RED}Also, got STDERR '{run.stderr.decode("utf-8")}'{BLANK}")
    if passed:
        print(f"--> {GREEN}PASSED{BLANK}")
        n_passed += 1

try:
    remove(COMPILED_FILE)
except FileNotFoundError: # thrown if file not found (i.e. failed to compile)
    pass
print(f"\nSummary: passed {n_passed}/{len(TESTS)} test{'s' if len(TESTS) > 1 else ''}")

if n_passed != len(TESTS):
    sys.exit(1)
#!/usr/bin/python3
import subprocess
import sys
import os.path
import tempfile

contents = b"""
#!/bin/sh
echo "stdout"
>&2 echo "stderr"
echo "stdout"
"""
with tempfile.NamedTemporaryFile() as f:
    f.write(contents)
    f.flush()

    proc = subprocess.Popen(["sh", f.name], stdin=None,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT)
    foo = None
    try:
        foo = proc.stdout.read()
        proc.communicate()
    except:
        proc.kill()

    print("Captured:")
    print(foo)

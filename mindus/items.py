import re
from sys import argv
import pyperclip

string = pyperclip.paste()
reg = re.compile(r"with\((.+)\)\);")
match = reg.search(string)[1].replace("Items", "")  # .replace(",", ":")
out: str = ""
i: int = -1
j: int = 0
while i < len(match) - 1:
    i += 1
    if match[i] == ".":
        out += match[i + 1].upper()
        i += 1
        continue
    if match[i] == ",":
        j += 1
        if j % 2 != 0:
            out += ":"
            continue
    out += match[i]
pyperclip.copy(f"cost!({out})")

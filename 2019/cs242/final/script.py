import os
from asciicvtv import TerminalOutput

"""
# If you are using Windows, 
# please use colorama to add ANSI support
 
import colorama
colorama.init()
"""

out = 'output'

frames = os.listdir(out)
frames.sort(key=lambda x: int(''.join(filter(str.isdigit, x))))

for frame in frames:
    path = os.path.join(os.getcwd(), out, frame)
    i = TerminalOutput(path)
    i.convert()
    print('\x1b[1;1H') 
    i.output_terminal()
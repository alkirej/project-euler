''' cmd_line.py '''
"""
Command line utilities
"""
import sys

def get_numeric_command_line_argument():
    if len(sys.argv) < 2:
        raise ValueError("Not enough command line arguments.  If found 0!")

    return int(sys.argv[1])


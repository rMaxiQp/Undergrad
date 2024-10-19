import unittest
from asciiv2v.TerminalOutput import TerminalOutput


"""
Test: Output converted image into terminal.
"""


class TerminalTest(unittest.TestCase):
    def setUp(self):
        self.terminal_converter = TerminalOutput("../Sample/sample.jpg")

    def test_convert(self):
        self.terminal_converter.convert()

    def test_output_txt(self):
        """
        Output without given file name.
        """
        self.terminal_converter.convert()
        self.terminal_converter.output_txt()

    def test_output_txt(self):
        """
        Output with given file name.
        """
        self.terminal_converter.convert()
        self.terminal_converter.output_txt('out.txt')

    def test_output_terminal(self):
        self.terminal_converter.convert()
        self.terminal_converter.output_terminal()

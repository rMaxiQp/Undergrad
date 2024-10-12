import unittest
from EmailServer.Server import Server

"""
Test: Format and size checker works with given params.
"""


class UtilTest(unittest.TestCase):

    def test_right_format_checker(self):
        server = Server()
        self.assertEqual(server.format_checking('image/png'), True)
        self.assertEqual(server.format_checking('video/mp4'), True)

    def test_wrong_format_checker(self):
        server = Server()
        self.assertEqual(server.format_checking('application/xml'), False)
        self.assertEqual(server.format_checking('random/blabla'), False)

    def test_size_checker(self):
        server = Server()
        self.assertEqual(server.size_checking(999999999), False)
        self.assertEqual(server.size_checking(99), True)
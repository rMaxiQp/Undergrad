import unittest
from EmailServer import LocalToken
from EmailServer.Sender import Sender
from EmailServer.SenderSuccess import SenderSuccess
from EmailServer.SenderFail import SenderFail

"""
Test: Email Sender can send general email and successful/failed message. 
"""


class SendTest(unittest.TestCase):

    def test_send(self):
        sender = Sender(LocalToken.TEST_RECEIVER, 'TEST_SUBJECT', 'TEST_BODY')
        sender.send()

    def test_send_success(self):
        sender = SenderSuccess(LocalToken.TEST_RECEIVER, 'testfile.png', 'TEST_TOKEN')
        sender.send()

    def test_size_fail(self):
        sender = SenderFail(LocalToken.TEST_RECEIVER, 'testfile.png', 'size')
        sender.send()

    def test_format_fail(self):
        sender = SenderFail(LocalToken.TEST_RECEIVER, 'testfile.iiii', 'format')
        sender.send()



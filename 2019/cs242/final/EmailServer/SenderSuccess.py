from EmailServer import Sender


class SenderSuccess(Sender.Sender):
    """
    Email Sender using SMTP server.
    Reply client to inform received their files and their unique token.
    """
    def __init__(self, receiver, file_name, token):
        subject = '[No reply] Your file has been received!'
        body = """  
               Thank you for using the ASCII converter. Your file (%s) has been received!
               Please use this token to download the ASCII version: %s
               """ % (file_name, token)

        self.sender = super().__init__(receiver, subject, body)

    def send(self):
        super().send()

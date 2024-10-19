from EmailServer.Sender import Sender


class SenderFail(Sender):
    """
    Email Sender using SMTP server.
    Reply client with information why their file is not valid.
    """
    def __init__(self, receiver, file_name, fail_reason):
        subject = "[No reply] Sorry we can't accept your file."
        if fail_reason == 'size':
            body = """  
                Thank you for using the ASCII converter. 
                Your file (%s) exceeds the size limit. Please try with a smaller file. 
                """ % file_name

        if fail_reason == 'format':
            body = """  
                Thank you for using the ASCII converter. 
                Your file (%s) format is not supported. Please try with a image/video file. 
                """ % file_name

        super().__init__(receiver, subject, body)

    def send(self):
        super().send()

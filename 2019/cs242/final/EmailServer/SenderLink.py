from EmailServer import Sender


class SenderLink(Sender.Sender):
    """
    Email Sender using SMTP server.
    Reply client to inform received their files and their unique token.
    """
    def __init__(self, receiver, file_name, link):
        subject = '[No reply] Your ASCII video is here!'
        body = """  
               Thank you for using the ASCII converter. Your file (%s) has been processed!
               
               Here is the link to download the ASCII video and all you need to play it in a terminal window: %s
               To download the video, you need to enter the email address and password which you sent the file to
               the server. The file link will expire after 10 days, and you have two times to download the file.
               
               Enjoy the video! :-)
               
               """ % (file_name, link)

        self.sender = super().__init__(receiver, subject, body)

    def send(self):
        super().send()

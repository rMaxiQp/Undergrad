import smtplib
from EmailServer import LocalToken


class Sender:
    """
    Email Sender using SMTP server.
    """
    def __init__(self, receiver, subject, body):
        self.receiver = receiver
        self.subject = subject
        self.body = body

    def send(self):
        body = self.body
        text = 'Subject: {}\n\n{}'.format(self.subject, body)
        try:
            server = smtplib.SMTP_SSL(LocalToken.SMTP_URL, LocalToken.SMTP_PORT)
            server.ehlo()
            server.login(LocalToken.SERVER_ADDRESS, LocalToken.SERVER_PWD)
            server.sendmail(LocalToken.SERVER_ADDRESS, self.receiver, text)
            server.close()
            print('EMAIL SENDER: Email to (%s) is sent!' % self.receiver)
        except:
            print('EMAIL SENDER: Email to (%s) is failed to send!' % self.receiver)


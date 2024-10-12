import imaplib
import os
import email
from EmailServer import LocalToken
from EmailServer.FormatChecker import FormatChecker
from EmailServer.EmailFactory import EmailFactory
from EmailServer.SenderSuccess import SenderSuccess
from EmailServer.SenderFail import SenderFail
from EmailServer.SenderLink import SenderLink

class Server:
    """
    IMAP Email Server.
    """
    def __init__(self):
        """
        Setup IMAP Email Server.
        Set incoming file format and size limitation (in Bytes).
        """
        self.mailbox = imaplib.IMAP4_SSL(LocalToken.IMAP_URL)
        self.format_checker = FormatChecker(['image', 'video'], 999999990000)

    def connect(self):
        """
        Connect Email Server and only read content under 'INBOX'.
        """
        self.mailbox.login(LocalToken.SERVER_ADDRESS, LocalToken.SERVER_PWD)
        self.mailbox.select('INBOX')
        print("SERVER: Server connected!")

    def format_checking(self, file_type):
        """
        Check incoming file format.
        """
        if self.format_checker.check_format(file_type):
            print("FORMAT CHECKER: Converter support file format: %s" % file_type)
            return True
        else:
            print("FORMAT CHECKER: Failed: Converter doesn't support file format: %s" % file_type)
            return False

    def size_checking(self, size):
        """
        Check incoming file size.
        """
        print_size = float(size/1024)
        if self.format_checker.check_size(size):
            print("File size is: ", print_size, "M")
            return True
        else:
            print("File size (", print_size, "M) exceeds limit.")
            return False

    def read_content(self):
        """
        Read incoming files.
        Format and size checking.
        Download files.
        Reply email.
        """

        res, data = self.mailbox.search(None, '(UNSEEN)')  # Only read new message
        mail_ids = data[0]
        id_list = mail_ids.split()

        for id in id_list:
            typ, data = self.mailbox.fetch(id, '(RFC822)')
            raw_email = data[0][1]
            raw_email_string = raw_email.decode('utf-8')
            email_message = email.message_from_string(raw_email_string)

            for msg in email_message.walk():
                """
                Iterate all parts and sub-parts.
                """
                if msg.get_content_maintype() == 'multipart':
                    # if the message’s payload is a list of sub-Message objects
                    continue
                if msg.get('Content-Disposition') is None:
                    # skip if has no content
                    continue

                file_name = msg.get_filename()
                file_type = msg.get_content_type()

                parser = EmailFactory(email_message)
                subject = parser.get_subject()
                SMTP_id = parser.get_SMTP_id()
                file_sender = parser.get_sender()

                if not self.format_checking(file_type):  # Format checking
                    sender = SenderFail(file_sender, file_name, 'format')
                    sender.send()
                    continue

                if bool(file_name):
                    print('SERVER: Received file: {file}'.format(file=file_name))
                    file_path = os.path.join(LocalToken.LOCAL_STORAGE_PATH, file_name)
                    if not os.path.isfile(file_path):
                        fp = open(file_path, 'wb')
                        content = msg.get_payload(decode=True)
                        content_size = len(content)
                        if not self.size_checking(content_size):  # Size checking
                            sender = SenderFail(file_sender, file_name, 'size')
                            sender.send()
                            continue
                        fp.write(content)  # Download file
                        fp.close()

                    sender = SenderSuccess(file_sender, file_name, SMTP_id)
                    sender.send()
                    print('SERVER: Downloaded "{file}" from email titled "{subject}".'.format(file=file_name, subject=subject))
                    return file_sender, file_name

    def send_link(self, sender, name, link):
        sender = SenderLink(sender, name, link)
        sender.send()


if __name__ == '__main__':
    server = Server()
    server.connect()
    server.read_content()
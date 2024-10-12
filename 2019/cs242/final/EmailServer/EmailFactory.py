class EmailFactory:
    """
    Process email server raw data.
    """

    def __init__(self, msg):
        self.msg = msg

    def get_subject(self):
        return str(self.msg).split("Subject: ", 1)[1].split("\n", 1)[0]

    def get_SMTP_id(self):
        return str(self.msg).split("with SMTP id ", 1)[1].split(";", 1)[0]

    def get_sender(self):
        return str(self.msg).split("<", 1)[1].split(">", 1)[0]


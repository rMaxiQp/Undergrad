class FormatChecker:
    """
    Check file format and size is valid or not.
    """
    def __init__(self, support_format, max_size):
        self.support_format = support_format
        self.max_size = max_size

    def check_format(self, given_format):
        parent_format = given_format.split('/')[0]
        return parent_format in self.support_format

    def check_size(self, given_size):
        return given_size <= self.max_size

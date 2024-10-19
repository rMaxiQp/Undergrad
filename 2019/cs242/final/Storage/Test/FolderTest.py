import unittest
from Storage import FolderManage
from Storage import Authentication
from Storage import LocalToken

TOKEN_HOSTNAME = 'https://ASCIIConverter.sharefile.com/oauth/token'


class FolderTest(unittest.TestCase):

    def test_get_root_folder(self):
        """
        Test: get root folder
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        header = Authentication.get_request_header(token)
        FolderManage.get_root_folder(header)

    def test_get_file_count(self):
        """
        Test: get file count given folder
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        header = Authentication.get_request_header(token)
        root_folder = FolderManage.get_root_folder(header)
        FolderManage.get_folder_file_count(root_folder)

    def test_get_folder_id(self):
        """
        Test: get folder id
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        header = Authentication.get_request_header(token)
        root_folder = FolderManage.get_root_folder(header)
        FolderManage.get_folder_id(root_folder)

    def test_create_folder(self):
        """
        Test: create a new folder with given name and description
              get id of the new folder
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        header = Authentication.get_request_header(token)
        root_folder = FolderManage.get_root_folder(header)
        id = FolderManage.get_folder_id(root_folder)
        new_folder = FolderManage.create_folder(header, id, "TestFolder", "TestDescription")
        FolderManage.get_folder_id(new_folder)

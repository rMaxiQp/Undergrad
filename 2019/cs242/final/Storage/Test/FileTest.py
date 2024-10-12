import unittest
from Storage import FolderManage
from Storage import Authentication
from Storage import LocalToken
from Storage import FileManage
from Storage import ShareLink


TOKEN_HOSTNAME = 'https://ASCIIConverter.sharefile.com/oauth/token'
TEST_LOCALPATH = 'c:/Users/Xizi Yang/cs242/FinalProject/EmailServer/IncomingFile/'


class FolderTest(unittest.TestCase):
    def setUp(self):
        """
        Set up for testing.
        Authentication -> find root folder -> create a new folder for holding the testing file
        """
        self.token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        self.header = Authentication.get_request_header(self.token)
        self.json_header = Authentication.get_json_request_header(self.token)
        self.root_folder = FolderManage.get_root_folder(self.header)
        id = FolderManage.get_folder_id(self.root_folder)
        new_folder = FolderManage.create_folder(self.header, id, "FileTestFolder", "FileTestDescription")
        self.folder_id = FolderManage.get_folder_id(new_folder)
        self.file_id = 0

    def test_upload_image(self):
        """
        Test: upload an image
        """
        FileManage.upload_file(self.json_header, self.folder_id, TEST_LOCALPATH + 'sample1.jpg')

    def test_upload_video(self):
        """
        Test: upload a video
        """
        FileManage.upload_file(self.json_header, self.folder_id, TEST_LOCALPATH + 'apple.map4')

    def test_get_file_id(self):
        """
        Test: get a file id
        """
        new_file = FileManage.upload_file(self.json_header, self.folder_id, TEST_LOCALPATH + 'sample1.jpg')
        self.file_id = FileManage.get_file_id(new_file)

    def test_create_share_link(self):
        """
        Test: create a share link for a given file
        """
        new_file = FileManage.upload_file(self.json_header, self.folder_id, TEST_LOCALPATH + 'sample1.jpg')
        self.file_id = FileManage.get_file_id(new_file)
        link = ShareLink.create_share_link(self.json_header, self.file_id, '2019-05-01', 2)

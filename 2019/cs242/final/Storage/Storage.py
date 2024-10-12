from Storage import FolderManage
from Storage import Authentication
from Storage import LocalToken
from Storage import FileManage
from Storage import ShareLink

"""
[Storage black box class]
Provide functionality: upload file, folder and create a share link.
"""


class Storage:
    def __init__(self):
        """
        Set up:
        Get token from authenticate. Create headers ready for requesting.
        """
        self.token = Authentication.authenticate(LocalToken.TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                                 LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        self.header = Authentication.get_request_header(self.token)
        self.json_header = Authentication.get_json_request_header(self.token)
        self.root_folder = FolderManage.get_root_folder(self.header)

    def get_root_id(self):
        """
        Return root folder id.
        """
        return FolderManage.get_folder_id(self.root_folder)

    def create_folder(self, folder_name, folder_description, parent_folder_id):
        """
        Create a folder under given parent directory.
        Params: new folder name, description, parent directory id.
        """
        new_folder = FolderManage.create_folder(self.header
                                                , parent_folder_id, folder_name, folder_description)
        return FolderManage.get_folder_id(new_folder)

    def create_file(self, parent_folder_id, file_path):
        """
        Upload a file from local path to a parent directory.
        Params: parent directory id, local file path.
        """
        new_file = FileManage.upload_file(self.json_header, parent_folder_id, file_path)
        return FileManage.get_file_id(new_file)

    def create_share_link(self, target_id, expire_date, max_download):
        """
        Create a share link.
        Params: file/folder id which will be shared, link expired data, max download time.
        """
        return ShareLink.create_share_link(self.json_header, target_id, expire_date, max_download)
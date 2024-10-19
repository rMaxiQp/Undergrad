import datetime
import os
from EmailServer.Server import Server
from Storage.Storage import Storage
from src.ImageParser import ImageParser
import LocalPath
import ConverterUtils


"""
Converter class:
Manage email server, video converter, cloud storage.
Automate the process from reading incoming file to convert to upload product and share.
"""


class Converter:
    def __init__(self):
        """
        Initialize server and storage. Set up connection.
        """
        self.server = Server()
        self.server.connect()
        self.storage = Storage()

    def email_server_read(self):
        """
        Email server: start reading incoming file.
        """
        return self.server.read_content()

    def email_server_send(self, sender, filename, link):
        """
        Email server: send a link to client.
        """
        self.server.send_link(sender, filename, link)

    def consume_incoming_files(self, directory, output_dir):
        """
        Feed downloaded incoming file into converter.
        Save output into a single directory.
        """
        for file in os.listdir(directory):
            if file.endswith(".mp4"):
                converter = ImageParser(directory+file, output_dir)
                converter.unzip(True)
                converter.convert()
                converter.zip()
                ConverterUtils.tear_down_file(directory+file)

    def upload_directory(self, home_directory):
        """
        Create a folder to share.
        Upload needed contents in output directory to cloud.
        """
        root_id = self.storage.get_root_id()

        new_folder_id = self.storage.create_folder("DemoFolder", "SharedFolder", root_id)
        for file in os.listdir(home_directory):
            if file.endswith(".mp4") or file.endswith(".zip"):
                full_path = home_directory + file
                self.storage.create_file(new_folder_id, full_path)
        ConverterUtils.tear_down_dir(home_directory)
        return new_folder_id

    def share_folder(self, folder_id):
        """
        Create a share link for output folder.
        """
        expire_date = (datetime.datetime.now() + datetime.timedelta(10)).date()
        return self.storage.create_share_link(folder_id, str(expire_date), 2)


if __name__ == '__main__':

    test_converter = Converter()
    sender, filename = test_converter.email_server_read()
    test_converter.consume_incoming_files(LocalPath.INCOMING_PATH, LocalPath.OUTPUT_PATH)
    share_folder_id = test_converter.upload_directory(LocalPath.OUTPUT_PATH)
    link = test_converter.share_folder(share_folder_id)
    test_converter.email_server_send(sender, filename, link)

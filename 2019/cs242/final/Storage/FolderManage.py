import requests
import json
import logging


"""
Folder storage manager class:

1. Folder storage is tree-like structure. 
   There is a root folder, and under it children folder can be created.
2. Each folder has an unique id.
   Folder id and name are stored in metadata.
"""

def get_root_folder(header):
    """
    Fetch root folder info.
    Return: Json object containing metadata of root folder.
    """
    root_url = 'https://asciiconverter.sf-api.com/sf/v3/Items'
    response = requests.get(root_url, headers=header)
    print("ROOT FOLDER REQUEST: ", response.status_code)
    return json.loads(response.content)


def get_folder_file_count(folder):
    """
    Param: Metadata of a folder.
    Return: File count of given folder.
    """
    count = folder['FileCount']
    print("FOLDERMANAGE: FileCount is: ", count)
    return count


def get_folder_id(folder):
    """
    Param: Metadata of a folder.
    Return: Id of given folder.
    """
    print(folder)
    id = folder['Id']
    print("FOLDERMANAGE: folder id is: ", id)
    return id


def create_folder(headers, parent_id, name, description):
    """
    Create a new folder in the given parent folder.
    Params: new folder's name and description
    """
    uri_path = 'https://asciiconverter.sf-api.com/sf/v3/Items(%s)/Folder' % parent_id
    folder = {'Name': name, 'Description': description}

    print(name, uri_path
          )
    response = requests.post(uri_path, data=folder, headers=headers)

    new_folder = json.loads(response.content)
    try:
        print("Created Folder: %s at time: %s" % (new_folder['Name'], new_folder['CreationDate']))
    except KeyError:
        # Folder name cannot repeat.
        logging.error("Create folder failed. Please try with a different folder name or check configuration.")
    return new_folder


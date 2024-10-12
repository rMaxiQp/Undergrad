import requests
import json
import os
import time
import mimetypes
import logging


"""
File storage manager class:

1. File stored under a folder. 
2. Each file has an unique id.
   File id and name are stored in metadata.
"""


def upload_file(auth_header, folder_id, local_path):
    """
    Uploads a File under given folder.
    Params: parent folder id.
            local path of the file which want to upload.

    Upload Step:
    1. Got authenticated token
    2. Use upload configuration to fetch(GET) upload URL
    3. POST metadata to the url.
    """

    uri_path = 'https://asciiconverter.sf-api.com/sf/v3/Items(%s)/Upload' % folder_id
    response = requests.get(uri_path, headers=auth_header)

    upload_config = json.loads(response.content)
    if 'ChunkUri' in upload_config:
        upload_response = multipart_form_post_upload(upload_config['ChunkUri'], local_path)
        print("FILEMANEGE: Upload configuration: ", upload_response.status_code, upload_response.reason)
        return json.loads(upload_response.content)['value'][0]
    else:
        logging.error('Upload config failed.')


def multipart_form_post_upload(url, filepath):
    """
    Multipart form post a file to url.
    POST metadata structure(in bytes not string):

    --------boundary-------
    content-disposition: form-data; name: <filename>; filename: <filename>
    content-type: <filetype>

    <binary file here>

    --------boundary-------
    """

    filename = os.path.basename(filepath)
    headers = {}
    boundary = '----------%d' % int(time.time())
    headers['content-type'] = 'multipart/form-data; boundary=%s' % boundary

    # data are all in bytes
    data = []
    data.append(('--%s' % boundary).encode('raw_unicode_escape'))
    data.append(('Content-Disposition: form-data; name="%s"; filename="%s"' % ('File1', filename))
                .encode('raw_unicode_escape'))
    data.append(('Content-Type: %s' % get_content_type(filename)).encode('raw_unicode_escape'))
    data.append(''.encode('raw_unicode_escape'))
    file = open(filepath, 'rb')
    content = file.read()
    print("FILEMANAGE: uploaded file size: ", len(content))

    data.append(content)
    data.append(('--%s--' % boundary).encode('raw_unicode_escape'))
    data.append(''.encode('raw_unicode_escape'))
    data = b'\r\n'.join(data)

    headers['content-length'] = str(len(content))
    file.close()
    return requests.post(url+'&fmt=json', data=data, headers=headers)


def get_content_type(filename):
    """
    Use library get file type.
    """
    return mimetypes.guess_type(filename)[0] or 'application/octet-stream'


def get_file_id(file_metadata):
    """
    Get id for a given file.
    """
    print(file_metadata)
    id = file_metadata['id']
    print("FILEMANAGE: file id is: ", id)
    return id
import requests
import json


def create_share_link(auth_header, file_id, expiration_date, cap_download):
    """
    Create a share link for given file
    Require enter user info for authentication
    """
    shareModel = {
        "ShareType": "Send",
        "Items": [{'Id': file_id}],
        "ExpirationDate": expiration_date,
        "MaxDownloads": cap_download,
        "RequireUserInfo": "true"
    }
    rep = requests.post('https://asciiconverter.sf-api.com/sf/v3/Shares', json.dumps(shareModel), headers=auth_header)
    link = json.loads(rep.content)['Uri']
    print("LINKCREATE: create a link: %s of file (%s)" % (link, file_id))
    return link

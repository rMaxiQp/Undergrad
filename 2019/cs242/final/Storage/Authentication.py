import requests
import json
import logging


"""
Cloud Storage Authentication:
1. username + password -> OAuth2 key + secret
2. username + password + OAuth2 key + secret (OData)-> token
3. Bearer token -> request
"""


def authenticate(hostname, client_id, client_secret, username, password):
    """ Authenticate via username/password. Returns json token object.
    Args:
    string hostname
    string client_id client_secret - OAuth2
    string username password
    """

    headers = {'Content-Type': 'application/x-www-form-urlencoded'}
    params = {'grant_type': 'password', 'client_id': client_id, 'client_secret': client_secret,
              'username': username, 'password': password}
    response = requests.post(hostname, params, headers)

    print("AUTHENTICATION:  ", response.status_code, response.reason)
    token = None
    if response.status_code == 200:
        token = json.loads(response.content)
        logging.info("Authentication succeed.")
    else:
        logging.error("Authentication failed. Please refresh your OAuth2 key.")

    return token


def get_request_header(token):
    """
    Create request header with token.
    """
    return {'Authorization': 'Bearer %s' % (token['access_token'])}


def get_json_request_header(token):
    """
    Create request header with token.
    Use Json Content type.
    """
    return {'Authorization': 'Bearer %s' % (token['access_token']), 'content-type': 'application/json'}


import unittest
from Storage import Authentication
from Storage import LocalToken


TOKEN_HOSTNAME = 'https://ASCIIConverter.sharefile.com/oauth/token'


class AuthenticationTest(unittest.TestCase):

    def test_fetch_token(self):
        """
        Test: fetch token with username + password + OAuth2 key + secret
        """
        Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                    LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)

    def test_build_request_authentication_header(self):
        """
        Test: extract access token from json object and build header for request
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        access_token = token['access_token']
        header = Authentication.get_request_header(token)
        self.assertEqual(header['Authorization'], 'Bearer ' + access_token)

    def test_json_header(self):
        """
        Test: extract access token from json object and build header for request with Json content
        """
        token = Authentication.authenticate(TOKEN_HOSTNAME, LocalToken.CLIENT_ID, LocalToken.CLIENT_SECRET,
                                            LocalToken.SERVER_USERNAME, LocalToken.SERVER_PASSWORD)
        header = Authentication.get_json_request_header(token)
        self.assertEqual(header['content-type'], 'application/json')


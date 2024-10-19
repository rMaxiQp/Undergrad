import os
import shutil

"""
Utils class: manage expired files
"""


def tear_down_dir(directory):
    """
    Recursively delete all files in the given directory.
    """
    shutil.rmtree(directory)


def tear_down_file(path):
    """
    Delete given file.
    """
    os.remove(path)
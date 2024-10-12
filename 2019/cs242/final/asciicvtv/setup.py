import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="asciicvtv",
    version="0.0.5",
    author="Xizi Yang, Zhengru Qian",
    author_email="xiziy2@illinois.edu, zq@illinois.edu",
    description="Convert from Original video/image to ASCII video/image",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://gitlab.engr.illinois.edu/zq2/sp19-cs242-final",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
)

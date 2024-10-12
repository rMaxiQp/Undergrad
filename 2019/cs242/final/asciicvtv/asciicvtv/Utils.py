"""
Util functions to process pixel.
"""


def map_char(r, g=-1, b=-1):
    """
    Mapping intensity value to ASCII characters.
    """
    ascii_char = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/|()1{}[]?-_+~<>i!lI;:,""^`'.  "
    length = len(ascii_char)
    if g == -1 and b == -1:  # Handle monochrome image.
        gray = r
    else:  # Handle colored image.
        gray = int(0.2126 * r + 0.7152 * g + 0.0722 * b)
    unit = (256.0 + 1) / length
    return ascii_char[int(gray / unit)]

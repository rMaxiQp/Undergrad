import unittest
from asciiv2v.ImageOutput import ImageOutput
"""
Test: Output converted image into image.
"""


class ImageTest(unittest.TestCase):
    def setUp(self):
        self.image_converter = ImageOutput("../sample1.jpg", 10)

    def test_convert(self):
        """
        Test: convert image and show in pop-up window
        """
        self.image_converter.convert()
        self.image_converter.generate_output_image()
        self.image_converter.show_output()

    def test_write_output(self):
        """
        Test: write converted image into file
        """
        self.image_converter.convert()
        self.image_converter.generate_output_image()
        self.image_converter.write_output("output.png")

    def test_set_granularity(self):
        """
        Test: set valid granularity
        """
        self.image_converter = ImageOutput("../sample1.jpg", 10)
        self.assertEqual(self.image_converter.granularity, 10)

    def test_fail_set_granularity(self):
        """
        Test: set invalid granularity
        """
        self.image_converter = ImageOutput("../sample1.jpg", 0)
        self.assertEqual(self.image_converter.granularity, 1)

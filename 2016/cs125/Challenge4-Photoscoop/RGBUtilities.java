/* Manipulates RGB values
 * 
 * 
 * @author
 */

public class RGBUtilities {

/**
 * Extracts the red component (0..255)
 * Hint: see ch13.1.2 Working With Pixels 
 * http://math.hws.edu/javanotes/c13/s1.html#GUI2.1.2
 * 
 * ... also see the notes in READ-ME-FIRST
 * 
 * @param rgb the encoded color int
 * @return the red component (0..255)
 */
	public static int toRed(int rgb) {
		return (rgb & 0xFF0000) >>16; // ((rgb >> 16) & 0xFF)
	}

	public static int toGreen(int rgb) {
		return (rgb & 0xFF00) >> 8;
	}

	public static int toBlue(int rgb) {
		return rgb & 0xFF;
	}

	/**
	 * 
	 * @param r the red component (0..255)
	 * @param g the green component (0..255)
	 * @param b the blue component (0..255)
	 * @return a single integer representation the rgb color (8 bits per component) rrggbb
	 */
	static int toRGB(int r, int g, int b) {
		return (r & 0xFF) <<16 | (g & 0xFF) <<8 | (b & 0xFF);
	}

}

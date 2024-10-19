
/* A class to implement the various pixel effects.
 *
 * 
 * @author
 */
public class PixelEffects {

	/** Copies the source image to a new 2D integer image */
	public static int[][] copy(int[][] source) {
		int width = source.length, height = source[0].length;
		int[][] newCopy = new int [width][height];
		for (int i = 0; i < width; i++)
			for (int j = 0; j < height; j++)
				newCopy[i][j] = source[i][j];
		return newCopy;
	}
	/**
	 * Resize the array image to the new width and height
	 * You are going to need to figure out how to map between a pixel
	 * in the destination image to a pixel in the source image
	 * @param source
	 * @param newWidth
	 * @param newHeight
	 * @return
	 */
	public static int[][] resize(int[][] source, int newWidth, int newHeight) {
		int[][] array = new int[newWidth][newHeight];
		for (int i = 0; i < newWidth; i++)
			for(int j = 0; j < newHeight; j++){
				int x = (int)((i/(double)newWidth) * source.length);
				int y = (int)((j/(double)newHeight) * source[0].length);
				array[i][j] = source[x][y];
			}
		return array;
	}

	/**
	 * Half the size of the image. This method should be just one line! Just
	 * delegate the work to resize()!
	 */
	public static int[][] half(int[][] source) {
		return resize(source, source.length/2, source[0].length/2);
	}
	
	/**
	 * Create a new image array that is the same dimensions of the reference
	 * array. The array may be larger or smaller than the source. Hint -
	 * this methods should be just one line - delegate the work to resize()!
	 * 
	 * @param source
	 *            the source image
	 * @param reference
	 * @return the resized image
	 */
	public static int[][] resize(int[][] source, int[][] reference) {
		return (resize(source,reference.length,reference[0].length));
	}

	/** Flip the image vertically */
	public static int[][] flip(int[][] source) {
		int[][] newSource = new int[source.length][source[0].length];
		for(int i = 0; i < source.length ; i++)
			for(int j = 0; j < (source[0].length) ; j++)
				newSource[i][j] = source[i][source[0].length - 1 - j];
		return newSource;
	}

	/** Reverse the image horizontally */
	public static int[][] mirror(int[][] source) {
		int[][] newSource = new int[source.length][source[0].length];
		for(int i = 0; i < source.length ; i++)
			for(int j = 0; j < (source[0].length) ; j++)
				newSource[i][j] = source[source.length-1-i][j]; 
		return newSource;
	}

	/** Rotate the image */
	public static int[][] rotateLeft(int[][] source) {
		int[][] newSource = new int[source[0].length][source.length];
		for(int i = 0; i < source[0].length; i++)
			for (int j = 0; j < source.length; j++)
				newSource[i][j] = source[source.length-1-j][i];
		return newSource;
	}

	/** Merge the red,blue,green components from two images */
	public static int[][] merge(int[][] sourceA, int[][] sourceB) {
		int[][] source = new int[sourceA.length][sourceA[0].length];
		sourceB = resize(sourceB, sourceA);
		for (int i = 0; i < sourceA.length; i++)
			for (int j = 0; j < sourceA[0].length; j++){
				int mergeRed = (RGBUtilities.toRed(sourceA[i][j]) + RGBUtilities.toRed(sourceB[i][j]))/2;
				int mergeGreen = (RGBUtilities.toGreen(sourceA[i][j]) + RGBUtilities.toGreen(sourceB[i][j]))/2;
				int mergeBlue = (RGBUtilities.toBlue(sourceA[i][j]) + RGBUtilities.toBlue(sourceB[i][j]))/2;
				int mergeColor = RGBUtilities.toRGB(mergeRed, mergeGreen, mergeBlue);
				source[i][j] = mergeColor;
			}
		return source;
	}

	/**
	 * Replace the green areas of the foreground image with parts of the back
	 * image
	 */
	public static int[][] chromaKey(int[][] foreImage, int[][] backImage) {
		int[][] newSource = new int [backImage.length][backImage[0].length];
		newSource = resize(foreImage,backImage.length,backImage[0].length);
		for (int i = 0; i < newSource.length; i++)
			for (int j = 0; j < newSource[0].length; j++){
				int rgb = newSource[i][j];
				int red = RGBUtilities.toRed(rgb);
				int green = RGBUtilities.toGreen(rgb);
				int blue = RGBUtilities.toBlue(rgb);
				if(green > 4 * Math.max(red, blue) && green > 64)
					newSource[i][j] = backImage[i][j];
			}
		return newSource;
	}

	/** Removes "redeye" caused by a camera flash. sourceB is not used */
	public static int[][] redeye(int[][] source, int[][] sourceB) {

		int width = source.length, height = source[0].length;
		int[][] result = new int[width][height];
		for (int i = 0; i < width; i++)
			for (int j = 0; j < height; j++) {
				int rgb = source[i][j];
				int red = RGBUtilities.toRed(rgb);
				int green = RGBUtilities.toGreen(rgb);
				int blue = RGBUtilities.toBlue(rgb);
				if (red > 4 * Math.max(green, blue) && red > 64)
					red = green = blue = 0;
				result[i][j] = RGBUtilities.toRGB(red, green, blue);
			}

		return result;
	}

	/* Up to you! do something fun to the image */
	public static int[][] funky(int[][] source, int[][] sourceB) {
		int[][] source1 = new int[source.length][source[0].length];
		for (int i = 0; i< source.length-1; i++)
			for(int j =0; j< source[0].length; j++){
				int mergeRed = (RGBUtilities.toRed(source[i][j]) + RGBUtilities.toRed(source[i+1][j]))/2;
				int mergeGreen = (RGBUtilities.toGreen(source[i][j]) + RGBUtilities.toGreen(source[i+1][j]))/2;
				int mergeBlue = (RGBUtilities.toBlue(source[i][j]) + RGBUtilities.toBlue(source[i+1][j]))/2;
				int mergeColor = RGBUtilities.toRGB(mergeRed, mergeGreen, mergeBlue);
				source1[i][j] = mergeColor;
			}
		int[][] source2 = new int[sourceB.length][sourceB[0].length];
		for (int i = 0; i< sourceB.length-1; i++)
			for(int j =0; j< sourceB[0].length; j++){
				int mergeRed = (RGBUtilities.toRed(sourceB[i][j]) + RGBUtilities.toRed(sourceB[i+1][j]))/2;
				int mergeGreen = (RGBUtilities.toGreen(sourceB[i][j]) + RGBUtilities.toGreen(sourceB[i+1][j]))/2;
				int mergeBlue = (RGBUtilities.toBlue(sourceB[i][j]) + RGBUtilities.toBlue(sourceB[i+1][j]))/2;
				int mergeColor = RGBUtilities.toRGB(mergeRed, mergeGreen, mergeBlue);
				source2[i][j] = mergeColor;
			}
		int[][] result = new int[source.length][source[0].length];
		result = merge(source1, source2);
		return result;
	}
}

/*A class to delegate each effect request.
 * The process method is called whenever the user selects a menu item.
 * However it wouldnt be hard to build a batch program that also uses this process method.
 * 
 * 
 * @author
 */
public class Effects {
	/**
	 * Returns a new image (2D array) based on the command and the source parameters.
	 * This method delegates all of the work to PixelEffects class
	 * @param cmd - the command to execute
	 * @param source - the primary image source (not changed)
	 * @param background - the secondary (background) image (not changed)
	 * @return the new image or null if the command failed.
	 */
	public static int[][] process(String cmd, int[][] source, int[][] background) {
		if(cmd.equals("half")) return PixelEffects.half(source);
		else if(cmd.equals("resize")) return PixelEffects.resize(source,background);
		else if(cmd.equals("flip")) return PixelEffects.flip(source);
		else if(cmd.equals("copy")) return PixelEffects.copy(source);
		else if(cmd.equals("merge")) return PixelEffects.merge(source, background);
		else if(cmd.equals("mirror")) return PixelEffects.mirror(source);
		else if(cmd.equals("rotate")) return PixelEffects.rotateLeft(source);
		else if(cmd.equals("redeye")) return PixelEffects.redeye(source, background);
		else if(cmd.equals("funky")) return PixelEffects.funky(source, background);
		else if(cmd.equals("key"))return PixelEffects.chromaKey(source, background);
		else{
			System.out.println("Todo: Implement Effects.process("+cmd+")");
			throw new RuntimeException("Unknown command:"+cmd);
		}
	}// end method

} // end class

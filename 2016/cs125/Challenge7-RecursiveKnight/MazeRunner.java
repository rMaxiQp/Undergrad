//@author
public class MazeRunner {

	private int x, y;

	/** Initializes the MazeRunner with the x,y values */
	public MazeRunner(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}


	/** Moves the runner one unit. No error checking is performed.
	 * 'N':go North (increment y), S:decrement y, E(increment x), W(decrement x)
	 * character values other than N,S,E or W are ignored.
	 */
	void moveOne(char dir) {
		switch (dir){
		case 'N': 
			y++;
			break;
		case 'S':
			y--;
			break;
		case 'E':
			x++;
			break;
		case 'W':
			x--;
			break;
		default:
			break;
		}
	}
	/** Returns true if this maze runner is on the same (x,y) square
	 * as the parameter. Assumes that the parameter is non-null.
	 */
	public boolean caught(MazeRunner other) {
		return other.x == x && other.y == y;
	}

	/**
	 * Uses recursion to find index of the shortest string.
	 * Null strings are treated as infinitely long.
	 * Implementation notes:
	 * The base case if lo == hi.
	 * Use safeStringLength(paths[xxx]) to determine the string length.
	 * Invoke recursion to test the remaining paths (lo +1)
	 */
	static int findShortestString(String[] paths, int lo, int hi) {
		if(lo == hi) return lo;
		int result = findShortestString(paths, lo+1, hi);
		if (safeStringLength(paths[result]) > safeStringLength(paths[lo])) return lo; 
		return result;
	}

	/** Returns the length of the string or Integer.MAX_VALUE
	 * if the string is null.
	 * @param s
	 * @return
	 */
	static int safeStringLength(String s) {
		return (s != null) ? s.length(): Integer.MAX_VALUE ;
	}


	/** Returns a string representation of the shortest path between
	 * (x,y) and (tx,ty). e.g. a result of "NNEE"
	 * means to travel from (x,y) -> (tx,ty) go North twice, then East twice.
	 * blocked is a square boolean grid of points that cannot be used.
	 * If(x,y) are invalid coords (outside of the grid array) this method returns null.
	 * If(x,y) is on a blocked square, this method returns null. Otherwise,
	 * If(x,y) are already the same as the target position, returns an empty string.
	 * If there is no path between (x,y) and (tx,ty) the method returns null.
	 * 
	 * Implementation notes:
	 * Use the statements above for the base cases.
	 * For the recursion part:
	 * 1. Set the current position to blocked (so that the recursive method does not
	 * attempt to re-use this square again)
	 * 2. Collect all paths from the NSEW neighbors
	 * 3. Reset the current blocked position to false.
	 * 4. Use findShortestString to determine the shortest path
	 * 5. If its non-null then PREPEND the compass direction of that neighbor's path.
	 * e.g. if the Northern neighbor returned "EWWS" 
	 * the East neighbor returned "NWWWWWWWSEEEESS" and W and S Neighbor return null
	 * then return "N" + "EWWS"
	 * Otherwise, just return null as none of the neighbors found a path.
	 */
	public static String shortestPath(int x, int y, int tX, int tY,
			boolean blocked[][]) {
		
		if(x > blocked.length-1 || y > blocked.length-1 || x<0 || y <0) return null;
		if(blocked[x][y] == true) return null;
		if(x==tX && y==tY) return "";
		blocked[x][y] = true;
		
		String up = shortestPath(x,y+1,tX,tY,blocked);
		String down = shortestPath(x,y-1,tX,tY,blocked);
		String left = shortestPath(x-1,y,tX,tY,blocked);
		String right = shortestPath(x+1,y,tX,tY,blocked);
		blocked[x][y] = false;

		if(up == null && down == null && left == null && right == null) return null;
		if(up != null) up = "N" + up;
		if(down != null) down = "S" + down;
		if(left != null) left = "W" + left;
		if(right != null) right = "E" + right;
				
		String[] paths = {up, down, left, right};
		
		return paths[findShortestString(paths, 0, 3)];
	}

	/** Moves the runner towards the target position, if the
	 * shortest path can be found between the current and target position.
	 * Implementation notes: calls shortestPath, 
	 * if result is not null then send the first char to moveOne()
	 * Hint: watch out for the empty string when target = current position...
	 */
	public void chase(boolean maze[][], int targetX, int targetY) {
		String s = shortestPath(x,y,targetX,targetY,maze);
		if(s != null && s != "") moveOne(s.charAt(0));
		// Use shortestPath, string.charAt,  moveOne
	}
}

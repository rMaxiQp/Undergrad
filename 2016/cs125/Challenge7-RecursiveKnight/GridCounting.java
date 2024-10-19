//@author
public class GridCounting {
	/** Returns the total number of possible routes (paths) from
	 * (x,y) to (tx,ty).
	 * There are only three valid kinds of moves:
	 *  Increment x by one.
	 *  Increment x by two.
	 *  Increment y by one.
	 *  
	 *  Hint: You'll need to test two base cases.
	 */
	public static int count(int x,int y, int tx, int ty) {
		if(x == tx-2 && y == ty) return 2;
		if(x == tx-1 && y == ty) return 1;
		if(y == ty && x == tx) return 1;
		if(y>ty || x>tx) return 0;
		return count(x+1,y,tx,ty) + count(x,y+1,tx,ty) + count(x+2,y,tx,ty);
	}
}

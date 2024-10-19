/*
 * 
 * 
 * @author
 *
 */
public class PlayListUtil {

	/**
	 * Debug ME! Use the unit tests to reverse engineer how this method should
	 * work. Hint: Fix the formatting (shift-ctrl-F) to help debug the following
	 * code
	 * 
	 * @param list
	 * @param maximum
	 */
	public static void list(String[] list, int maximum) {
		int i;
		if (maximum == -1)
			for(i=0;i<list.length;i++)
				TextIO.putln("" + (i+1) + ". " + list[i]);
		else
			for (i = 0; i < maximum;i++)
				TextIO.putln("" + (i+1) + ". " + list[i]);
	}

	/**
	 * Appends or prepends the title
	 * 
	 * @param list
	 * @param title
	 * @param prepend
	 *            if true, prepend the title otherwise append the title
	 * @return a new list with the title prepended or appended to the original
	 *         list
	 */
	public static String[] add(String[] list, String title, boolean prepend) {
		String[] listNew = new String [list.length+1];
		if(prepend){
			listNew[0] = title;
			for (int i=0; i < list.length; i++)
				listNew[i+1] = list[i];
		}
		else{
			for(int j = 0; j < list.length; j++)
				listNew[j] = list[j];
			listNew[listNew.length-1] = title;
		}
		return listNew;
	}

	/**
	 * Returns a new list with the element at index removed.
	 * 
	 * @param list
	 *            the original list
	 * @param index
	 *            the array index to remove.
	 * @return a new list with the String at position 'index', absent.
	 */
	public static String[] discard(String[] list, int index) {
		String[] listNew= new String [list.length-1];
		for (int i = index; i > 0; i--){
			String temp = "";
			temp = list[i];
			list[i] = list[i-1];
			list[i-1] = temp;
		}
		for (int j = 0; j < listNew.length; j++)
			listNew[j]=list[j+1];
		return listNew;
	}

}

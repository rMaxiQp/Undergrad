package introduction;

import java.util.ArrayList;
 
public class ArrayListExample {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// In real Java programs, ArrayList and HashMap
		// are commonly used
		ArrayList list = new ArrayList();
		list.add("ONE");
		list.add("TWO");
		list.remove(0);
		list.get(0);
		
		ArrayList another = new ArrayList();
		another.addAll(list);
		another.removeAll(list);
		
		// Google search HashMap examples too.
	}

}

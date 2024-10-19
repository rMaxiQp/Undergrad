//@author
public class Stack {
	
	private String[] list = new String[0];
	private int size = 0;
	
	/** Adds a value to the top of the stack.*/
	public void push(String value){
		size++;
		String[] old = list;
		list = new String[size];
		for (int i = 1; i < size; i++)
			list[i] = old[i-1];
		list[0] = value;
	}
	
	/** Removes the topmost string. If the stack is empty, returns null. */
	public String pop() {
		if(size > 0){
			size--;
			String[] old = list;
			list = new String[size];
			for (int i = 0; i < size; i++)
				list[i] = old[i+1];
			return old[0];
		}
		else
			return null;
	}
	
	/** Returns the topmost string but does not remove it. If the stack is empty, returns null. */
	public String peek() {
		if (0 == size)
			return null;
		else
			return list[0];

	}
	
	/** Returns true iff the stack is empty */
	public boolean isEmpty() {
		return 0 == size;
	}

	/** Returns the number of items in the stack. */
	public int length() {
		return size;
	}
	
	/** Returns a string representation of the stack. Each string is separated by a newline. Returns an empty string if the stack is empty. */
	public String toString() {
		if (isEmpty())
			return null;
		else{
			String str = "";
			for (int i = size - 1; i >= 0; i--)
				str += list[i] +"\n";
			return str;
		}
	}
}

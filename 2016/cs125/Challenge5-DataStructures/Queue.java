//@author
public class Queue {
	
	private double[] list = new double[0];
	private int size = 0;
	
	/** Adds the value to the front of the queue.
	 * Note the queue automatically resizes as more items are added. */
	public void add(double value) {
		size++;
		double[] old = list;
		list = new double[size];
		for (int i = 0; i < size-1; i++)
			list[i+1] = old[i];
		list[0] = value;
	}
	/** Removes the value from the end of the queue. If the queue is empty, returns 0 */
	public double remove() {
		if (0 == size)
			return 0;
		else{
			size--;
			double[] old = list;
			list = new double[size];
			for(int i =0; i < size; i++)
				list[i] = old[i];
			return old[size];
		}
	}
	
	/** Returns the number of items in the queue. */
	public int length() {
		return list.length;		
	}
	
	/** Returns true iff the queue is empty */
	public boolean isEmpty() {
		return 0 == size;
	}
	
	/** Returns a comma separated string representation of the queue. */
	public String toString() {
		StringBuffer str = new StringBuffer("");
		for(int i = size-1; i >= 0; i--){
			if(i < size-1)
				str.append(',');
			str.append(list[i]);
		}
		return str.toString();
	}
}

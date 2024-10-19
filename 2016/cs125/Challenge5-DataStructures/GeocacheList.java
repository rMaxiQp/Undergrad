/**
 * Complete the following GeocacheList, to ensure all unit tests pass.
 * There are several errors in the code below
 *
 * Hint: Get the Geocache class working and passing its tests first.
 */
//@author
public class GeocacheList {
	private Geocache[] data = new Geocache[0];
	private int size = 0;

	public Geocache getGeocache(int i) {
		return data [i];
	}

	public int getSize() {
		return this.size;
	}

	public GeocacheList() {
	}

	public GeocacheList(GeocacheList other, boolean deepCopy) {
		this.size = other.size;
		this.data = new Geocache[this.size];
		if(deepCopy)
			for (int i = 0; i < this.size ; i++){
				data[i] = new Geocache(other.data[i].getX(),other.data[i].getY());
			}
		else
			this.data = other.data;
	}

	public void add(Geocache p) {
		size++;
		if (size > data.length) {
			Geocache[] old = data;
			data = new Geocache[size * 2];
			for (int i = 0; i < old.length; i++)
				data[i] = old[i];
		}
		data[size-1] = p;
	}

	public Geocache[] removeFromTop() {
		Geocache[] temp = data;
		size--;
		data = new Geocache[size];
		for (int i = 0; i < size; i++)
			data[i] = temp[i+1];
		return data;
	}

	public String toString() {
		StringBuffer s = new StringBuffer("GeocacheList:");
		for (int i = 0; i < size; i++) {
			if (i > 0)
				s.append(',');
			s.append(data[i]);
		}
		return s.toString();
	}
	
	public static void main (String[] args){
		GeocacheList a = new GeocacheList();
		Geocache p1 = new Geocache(2.2, 3.0);
		a.add(p1);
		System.out.println(a.getGeocache(0));
		GeocacheList b = new GeocacheList(a,true);
		System.out.println(b.getGeocache(0));
	}
}
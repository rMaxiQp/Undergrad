import java.awt.Color;
//@author
public class KeyValueMap { // aka Dictionary or Map
	private int size = 0;
	private String[] keyList = new String[0];
	private Color[] valueList = new Color[0];
	/**
	 * Adds a key and value. If the key already exists, it replaces the original
	 * entry.
	 */
	public void add(String key, Color value) {
		for (int i =0; i<keyList.length; i++)
			if(keyList[i].equals(key)){
				valueList[i] = value;
				break;
			}
		size++;
		String[] old = keyList;
		keyList = new String[size];
		for (int i = 0; i < size-1; i++)
			keyList[i] = old[i];
		keyList[size-1] = key;
		Color[] temp = valueList;
		valueList = new Color[size];
		for (int i = 0; i < size-1; i++)
			valueList[i] = temp[i];
		valueList[size-1] = value;
	}

	/**
	 * Returns particular Color object previously added to this map.
	 */
	public Color find(String key) {
		for(int i =0; i<size; i++)
			if(keyList[i].equals(key))
				return valueList[i];
		return null;
	}

	/**
	 * Returns true if the key exists in this map.
	 */
	public boolean exists(String key) {
		for (int i =0; i < size; i++){
			if(keyList[i].equals(key))
				return true;
		}
		return false;
	}

	/**
	 * Removes the key (and the color) from this map.
	 */
	public void remove(String key) {
		boolean check = false;
		size--;
		String[] old = keyList;
		keyList = new String[size];
		Color[] temp = valueList;
		valueList = new Color[size];
		for(int i =0; i < size; i++){
			if(old[i].equals(key))
				check = true;
			if(!check){
				keyList[i] = old[i];
				valueList[i] = temp[i];
			}
			else{
				keyList[i] = old[i+1];
				valueList[i] = temp[i+1];
			}
		}
	}

}

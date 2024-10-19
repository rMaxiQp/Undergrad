//@author
/**
	* write code here to determine the secret password
	* to unlock the given lock object.
	* You do not need to use recursion.
	* Hint: Read the source code of InsecurePasswordLock
	* The lock has a weakness....
	* Understand it and you can write an algorithm to quickly find the
	* secret password
	* (A brute force guess of a 40 character password would take a long
	* time...
	* as there are 26^40 combinations!
	* Your method should find it in a few seconds.
	* 
	* Beginner: You should complete this code in less than an hour

	* Advanced Code-Golf: Can you complete this method in 8 lines
	* (excluding the top and bottom given
	* lines and after autoformating your code)
	
	* Crazy Instructor level:
	* I can write a complete albeit-inefficient solution using single while loop :-)
	* expression: while (____){//NoCodeHere}
	*/
public class InsecurePasswordLockBreaker {
	/*
	public static char[] breakLock(InsecurePasswordLock lock) {
		char[] key = new char[1];
		for(int i = 30; lock.open(key) == -1; i++)
			key = new char[i];
		int j = 0;
		while(j!=key.length){
			for(int i = 0; lock.open(key) == j; i++)
				key[j] = (char) ('A' + i);
			j++;
		}
		return key;
	}
	*/
	public static char[] breakLock(InsecurePasswordLock lock) {
		char[] key = new char[1];
		int i = 29;
		int j = 0;
		while(lock.open(key)== -1 || lock.open(key) != key.length){
			if(lock.open(key) == -1) key = new char[i++];
			else if(lock.open(key) != key.length) key[lock.open(key)] = (char)('A' + j);
			j++;
			if(j==26) j = j%26;
		}
		return key;
	}

	public static void main(String[] args) {
		InsecurePasswordLock lock = new InsecurePasswordLock();
		char[] key = breakLock(lock);
		System.out.println(key);
		System.out.println(lock.isUnlocked());
	}
}

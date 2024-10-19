
/**
 * This program prints "a","b","c" depending on who has the highest score. The
 * given code may not be correct. Fix it and additional code to pass the unit
 * tests.
 * 
 * @see Winner-ReadMe.txt for details on how to complete this program.
 * @author
 * 
 */
public class Winner {
	public static void main(String[] args) {
		System.out.println("Enter three unique integer scores.");

		int a = TextIO.getlnInt();
		int b = TextIO.getlnInt();
		int c = TextIO.getlnInt();

		TextIO.put("1st Place:");
		if (a > b)
			if (a > c)
				TextIO.putln("a");
			else 
				TextIO.putln("c");
		else
			if (b > c)
				TextIO.putln("b");
			else
				TextIO.putln("c");
		// the logic and text output need further work...
	}
}


/**
 * A program to calculate a factorial. The given code may contain errors. Fix the
 * given code and add additional code to calculate a factorial and pass the unit
 * tests. Hint sometimes an 'int' just 'aint big enough.
 * 
 * @see Factorial-ReadMe.txt for details on how to complete this program.
 * @author
 */
public class Factorial {
	public static void main(String[] args) {
		long max = 0;
		while (max < 1 || max >= 21) {
			System.out.println("Enter a number between 1 and 20 inclusive.");
			max = TextIO.getlnInt();
		}
		TextIO.put(max+"! = ");
		for (long n = (max - 1); n > 0; n--){
			max = max * n;
		}
		TextIO.putln(max);
	}
}

/**
 * Prints sum of odd numbers in a specific format.
 * @author
 */
public class OddSum { 
/**
Example output if user enters 10:
Max?
1 + 3 + 5 + 7 + 9 = 25
25 = 9 + 7 + 5 + 3 + 1

Example output if user enters 11:
Max?
1 + 3 + 5 + 7 + 9 + 11 = 36
36 = 11 + 9 + 7 + 5 + 3 + 1

 */
 public static void main(String[] args) {
 
	 TextIO.putln("Max?");
	 int max = TextIO.getlnInt();
	 int count = 0;
	 String sum = "";
	 String sumReverse = "";
	 if (max%2==0)
			 max--;
	 for (int i = 1; i <= max; i += 2){
		 if (i != max) { 
			 sum = sum + i + " + ";
		 	sumReverse = " + "+ i + sumReverse;
		 }
		 else{
			 sum = sum + i;
			 sumReverse = i + sumReverse;
		 }
		 count += i;
	 }
	 sum = sum + " = " + count;
	 sumReverse = count + " = " + sumReverse;
	 TextIO.putln(sum);
	 TextIO.putln(sumReverse);
	 
  } // end of main method
} // end of class 

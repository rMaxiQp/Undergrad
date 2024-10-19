/**
 * A program to search for to encrypt and decrypt lines of text. See
 * CaesarCipher.txt
 * Hints: line.charAt( int ) is useful.
 * You'll need loops, and conditions and variables
 * You'll need to read the Test cases to understand how your program should work.
 * Good Programming Hints: "DRY: Don't Repeat Yourself"
 * Try to make your program as short as possible.
 * @author
 */
public class CaesarCipher {

	public static void main(String[] strings) {
		boolean valid= true;
		int shift = 0;
		
		while(valid){//If the shift is valid
			TextIO.putln("Please enter the shift value (between -25..-1 and 1..25)");
			shift = TextIO.getlnInt();
			valid = false;
			if (999==shift || -999==shift)
				TextIO.putln("Using position shift");
			else if (shift!=0 && shift <= 25 && shift >= -25)
				TextIO.putln("Using shift value of "+shift);
			else{
				TextIO.putln(shift + " is not a valid shift value.");
				valid = true;
			}
		}//end of while loop
		
		while(!valid){//Shift the string
			TextIO.putln("Please enter the source text (empty line to quit)");
			String source = TextIO.getln();
			if (0 == source.trim().length())
				break;
			TextIO.putln("Source   :" + source);
			TextIO.putln("Processed:" + cipher(source.toUpperCase(), shift));
		}//end of while loop
		
		TextIO.putln("Bye.");
	}//end of main
	
	public static String cipher(String sUpper, int shift){
		String s = "";
		int count = shift;
		if (shift == 999 || shift == -999 )
			count = 0;
		for (int i = 0; i < sUpper.length(); i++){
			char temp = sUpper.charAt(i);
			int distance = sUpper.charAt(i) - 'A';
			if (temp>='A' && temp <='Z')
				if ((distance + count)%26 < 0)
					temp = (char) ('A' + 26 + (distance + count)%26);
				else
					temp = (char) ('A' + (distance + count)%26);
			s = s + temp;
			if (shift == 999)
				count ++;
			if (shift == -999)
				count --;
		}
		return s;	
	}//end of cipher

}//end of program
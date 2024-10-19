/**
 * See CipherBreaker.txt for instructions.
 * @author
 */
public class CipherBreaker {

	public static void main(String[] args) {
		TextIO.putln("Text?");
		String line = TextIO.getln();
		TextIO.putln(line);
		
		String lineUpper = line.toUpperCase();
		int numPunctuation = 0;
		int numSpace = 0;
		int numDigit = 0;
		int[] letters = new int[26];
		for (int i = 0; i <lineUpper.length(); i++){
			if (lineUpper.charAt(i)>='0'&& lineUpper.charAt(i)<='9')
				numDigit ++;
			else if (lineUpper.charAt(i) == ' ' )
				numSpace ++;
			else if (lineUpper.charAt(i)=='"'||lineUpper.charAt(i)=='!'||(lineUpper.charAt(i)>=','&&lineUpper.charAt(i)<=';')||lineUpper.charAt(i)=='?'||lineUpper.charAt(i)=='\'')
				numPunctuation ++;
			else if (lineUpper.charAt(i)>='A'&&lineUpper.charAt(i)<='Z')
			{
				letters[(int)(lineUpper.charAt(i)-'A')]++;
			}
		}
		for (int i = 0; i<letters.length;i++)
			if (letters[i]>0)
				TextIO.putln((char)(i +'A') + ":" + letters[i]);
		if(numDigit>0) TextIO.putln("DIGITS:"+numDigit);
		if(numSpace>0) TextIO.putln("SPACES:"+numSpace);
		if(numPunctuation>0) TextIO.putln("PUNCTUATION:"+numPunctuation);

	}
}
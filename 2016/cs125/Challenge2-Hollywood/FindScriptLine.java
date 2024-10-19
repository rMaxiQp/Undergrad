/**
 * A program to search for specific lines and print their line number.
 * See FindScriptLine.txt for more details
 * TODO: add your netid to the line below
 * @author
 */
public class FindScriptLine {

	public static void main(String[] args) {
		String name ="";
		String nameLower = "";
		System.out.println("Please enter the word(s) to search for");
		name = TextIO.getlnString();
		nameLower = name.toLowerCase();
		System.out.printf("Searching for \'%s\'\n",name);
		int line = 0;
		TextIO.readFile("thematrix.txt");
		while(!TextIO.eof()) {
			String str = "";
			String s = "";
			str = TextIO.getln().trim();
			s = str.toLowerCase();
			boolean ifExist = (s.indexOf(nameLower) >= 0);
			if (ifExist) {
				System.out.print((line+1)+" - ");
				System.out.println(str);
			}			
			line ++;
		}
		System.out.printf("Done Searching for \'%s\'",name);
		
// TODO: Implement the functionality described in FindScriptLine.txt
// TODO: Test your code manually and using the automated unit tests in FindScriptLineTest		
	}
}

/**
 * A program to print one actor's lines. 
 * See ScriptPrinter.txt for more information.
 * TODO: add your netid to the line below
 * @author
 */
public class MyScriptPrinter {
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		boolean output=false; //Set to true when we find the desired character
		String name=""; // Only print lines for this character
		
		TextIO.putln("Which character's lines would you like? (NEO,MORPHEUS,ORACLE)");
		name = TextIO.getln().toUpperCase();

		TextIO.readFile("thematrix.txt"); // stop reading from the keyboard- use the script
		System.out.printf("%s\'s lines:\n",name);//Print the name here
		
		output = false; // initially don't print anything

		// This loop will read one line at a time from the script until it
		// reaches the end of the file and then TextIO.eof() will return true.
		// eof means end-of-file
		while (false == TextIO.eof()) {
			String line = TextIO.getln().trim(); // Read the next line

			if (0 == line.length())//If it's a blank line set 'output' to false
				output = false;
			if (line.indexOf(name) == 0){
				line = TextIO.getln().trim();
				output = true;
			}
			if (output)
				TextIO.putf("%s:\"%s\"\n",name,line);// Only print the line if 'output' is true

		}
		System.out.println("---");//Print 3 dashes here to indicate processing is complete
	}

}
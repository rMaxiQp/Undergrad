/**
 * A program to run a simple survey and report the results. See MovieSurvey.txt
 * for more information. TODO: add your netid to the line below
 * 
 * @author
 */
public class MovieSurvey {
	public static void main(String[] arg) {
		// TODO: Write your program here
		// Hints :
		// Formatted output
		// http://math.hws.edu/javanotes/c2/s4.html#basics.4.4
		double sum =0.0;
		double cinemaMovies = 0.0;
		double DVDMovies = 0.0;
		double computerMovies= 0.0;
		double percentage1 = 0.0;
		double percentage2 = 0.0;
		//Asking questions
		System.out.println("Welcome. We're interested in how people are watching movies this year.");
		System.out.println("Thanks for your time. - The WRITERS GUILD OF AMERICA.");
		System.out.println("Please tell us about how you've watched movies in the last month.");
		System.out.println("How many movies have you seen at the cinema?");
		cinemaMovies = TextIO.getlnDouble();
		System.out.println("How many movies have you seen using a DVD or VHS player?");
		DVDMovies = TextIO.getlnDouble();
		System.out.println("How many movies have you seen using a Computer?");
		computerMovies = TextIO.getlnDouble();
		
		sum = cinemaMovies + DVDMovies + computerMovies;
		percentage1 = cinemaMovies / sum * 100;
		percentage2 = (DVDMovies + computerMovies)/ sum * 100;
		
		//Feedback 
		TextIO.putf("Summary: %1.0f Cinema movies, %1.0f DVD/VHS movies, %1.0f Computer movies\n", cinemaMovies, DVDMovies, computerMovies);
		TextIO.putf("Total: %1.0f movies\n",sum);
		TextIO.putf("Fraction of movies seen at a cinema: %1.2f%%\n", percentage1);
		TextIO.putf("Fraction of movies seen outside of a cinema: %1.2f%%\n", percentage2);
	}
}

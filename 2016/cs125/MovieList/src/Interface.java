
//@author
public class Interface {
	// 1) add a movie
	// 2) remove a movie
	// 3) display movies alphabetically
	// 4) display movies >= a certain rating
	// 5) display movies in a specified genre
	// 6) list all movies with a specified actor/actress
	// 7) list all movies the user has yet to see
	// 8) quit
	private static MovieList movieList = new MovieList();

	public static void display() {
		System.out.println("Welcome to the Movie Rating App! Select an option below:\n");
		System.out.println("1) add a movie");
		System.out.println("2) remove a movie");
		System.out.println("3) display movies alphabetically");
		System.out.println("4) display movies >= a certain rating");
		System.out.println("5) display movies in a specified genre");
		System.out.println("6) list all movies with a specified actor/actress");
		System.out.println("7) list all movies the user has yet to see");
		System.out.println("8) quit\n");
		System.out.println("Select an option above:");
	}

	public static int pickATerm(int num) {
		if (num == 1)
			movieList = movieList.addMovie();
		else if (num == 2)
			movieList = movieList.removeMovie();
		else if (num == 3)
			movieList.printAll();
		else if (num == 4)
			movieList.printByRate();
		else if (num == 5)
			movieList.printByGenre();
		else if (num == 6)
			movieList.printByActor();
		else if (num == 7)
			movieList.printNoSeen();
		else if (num == 8)
			return quit();
		return 0;
	}

	private static int quit() {
		System.out.println("Are you sure you want to quit? - all your data will be lost (Y/N)");
		boolean c = false;
		c = TextIO.getlnBoolean();
		if (c) return -1;
		return 0;
	}

	public static void main(String args[]) {
		int response = 0;
		while (response != -1) {
			display();
			int choice;
			do
				choice = TextIO.getlnInt();
			while (choice < 0 || choice > 8);
			response = pickATerm(choice);
		}
		System.out.println("See You");
	}
}
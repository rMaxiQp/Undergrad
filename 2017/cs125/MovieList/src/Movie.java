
public class Movie {
	public enum genre {
		ACTION, ROMANCE, COMEDY, ANIME, DRAMA, DOCUMENTARY
	}

	protected genre Genre;
	protected String description;
	protected int rating;// 1-5
	protected String title;
	protected ActorList actors;
	protected boolean haveSeen;

	public Movie() {
	}

	public Movie(String title, int rating, String description, genre Genre, boolean haveSeen, ActorList actors) {// Constructor
		this.title = title;
		this.rating = rating;
		this.description = description;
		this.Genre = Genre;
		this.haveSeen = haveSeen;
		this.actors = actors;
	}

	public Movie(Movie mv, ActorList actors) {
		title = mv.title;
		rating = mv.rating;
		description = mv.description;
		Genre = mv.Genre;
		haveSeen = mv.haveSeen;
		this.actors = actors;
	}

	public Movie addMovie() {
		String title = "";
		int rating = -1;
		String description = "";
		String str = "";
		Movie.genre genre = null;
		boolean haveSeen;

		System.out.print("Titile: ");
		title = TextIO.getln();// Title
		System.out.print("Rating (1-5): ");
		rating = TextIO.getlnInt();// Rating
		System.out.print("Brief Description: ");
		description = TextIO.getln();
		while (!str.equals("ACTION") && !str.equals("ROMANCE") && !str.equals("COMEDY") && !str.equals("ANIME")
				&& !str.equals("DRAMA") && !str.equals("DOCUMENTARY")) {
			System.out.print("Genre (Action, Romance, Comedy, Anime, Drama, Documentary): ");
			str = TextIO.getln().toUpperCase();// Genre
		}
		genre = Movie.genre.valueOf(str);
		System.out.print("Have you seen it (T/F): ");// HaveSeen
		haveSeen = TextIO.getlnBoolean();
		Movie mv = new Movie(title, rating, description, genre, haveSeen, null);
		return mv;
	}
}

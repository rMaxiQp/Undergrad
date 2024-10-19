
/***
 * list by a specific genre 
 * sort by alphabetical order 
 * list all movies with a specified actor/actress 
 * list all movies that the user has yet to see
 * list all movies >= rating
 ****/
public class MovieList {
	private Movie currentMovie;
	private MovieList next;
	private static boolean emptyList = true;

	public MovieList() {
	}

	public MovieList(Movie currentMovie, MovieList next) {
		this.currentMovie = currentMovie;
		this.next = next;
	}

	public MovieList addMovie() {
		/************ Input Values *************/
		String actor = "";
		ActorList actors;
		Movie temp = new Movie();
		
		temp = temp.addMovie();
		System.out.println("Please Add The Actor/Actress's name:");
		System.out.println("NOTE: There has to be at least one actor/actress!!!");
		while (actor.equals(""))
			actor = TextIO.getln();
		actors = new ActorList(actor, null);
		actors.addActor();

		/************ Create New Object *********/
		Movie mv = new Movie(temp, actors);
		return insert(mv);
	}

	/************* Insert Movie ***************/
	private MovieList insert(Movie name) {
		if(emptyList){
			emptyList = false; 
			return new MovieList(name, null);
		}
		if (name.title.compareTo(currentMovie.title) < 0){
			return new MovieList(name,this);
		}
		if (this.next == null){
			this.next = new MovieList(name,null);
			return this;
		}
		this.next = next.insert(name);
		return this;
	}

	/************ Remove Movie *************/
	public MovieList removeMovie() {
			System.out.print("Input The Title Of The Movie: ");
			String name = TextIO.getln();
			return removeMovie(name);
	}

	private MovieList removeMovie(String name) {
		if (name.equals(currentMovie.title) && next!= null) {
			this.currentMovie = next.currentMovie;
			next = next.next;
			return this;
		}

		if (next == null)
			return this;
		if(name.equals(next.currentMovie.title)){
			next = next.next;
			return this;
		}
		return next.removeMovie(name);
	}

	/*** Print All Alphabetically *********/
	public void printAll() {// print all list
			print();
			if (next != null) next.printAll();
	}

	/***** Print By Rating ****************/
	public void printByRate() {// list by rating
			System.out.print("Movies That Are Larger Or Equal Than This Rate: ");
			int num = TextIO.getlnInt();
			printByRate(num);
	}

	private void printByRate(int num) {
		if (currentMovie.rating >= num) print();
		if (next != null) next.printByRate(num);
	}

	/*********** Print By Genre **********/
	public void printByGenre() {// list by genre
			String str = "";
			do {
				System.out.print("Genre (Action, Romance, Comedy, Anime, Drama, Documentary): ");
				str = TextIO.getln();
			} while (!str.toUpperCase().equals("ACTION") && !str.toUpperCase().equals("ROMANCE")
					&& !str.toUpperCase().equals("COMEDY") && !str.toUpperCase().equals("ANIME")
					&& !str.toUpperCase().equals("DRAMA") && !str.toUpperCase().equals("DOCUMENTARY"));
			printByGenre(str);
	}

	private void printByGenre(String name) {
			if (currentMovie.Genre.toString().equals(name)) print();
			if (next != null) next.printByGenre(name);
	}

	/********** Print By Actor ************/
	public void printByActor() {// list by actors
			String name = "";
			name = TextIO.getln();
			printByActor(name);
	}

	private void printByActor(String name) {
		if (currentMovie.actors.actorExist(name))
			print();
		if (next == null)
			return;
		next.printByActor(name);
	}

	/*********** Print NoSeen One *********/
	public void printNoSeen() {
		printNoSeenList();
		System.out.println("We recommend: " + bestRate().title);
	}

	private void printNoSeenList() {// list by haveSeen
		if (!currentMovie.haveSeen) print();
		if (next != null) next.printNoSeenList();
	}
	
	private Movie bestRate(){
		if(currentMovie.rating == 5 && !currentMovie.haveSeen) return currentMovie;
		if(next == null && !currentMovie.haveSeen) return currentMovie;
		else if(next == null) return null;
		Movie temp = new Movie();
		temp = next.bestRate();
		if(currentMovie.haveSeen) return temp;
		else return currentMovie.rating > temp.rating ? currentMovie : temp;
	}

	/*****
	 * General Print
	 *******/
	private void print() {
		System.out.print("Titile: " + currentMovie.title + " Description: " + currentMovie.description + " Genre: " + 
	currentMovie.Genre + " Actors/Actresses: ");
		currentMovie.actors.printActors();
		System.out.println(" HaveSeen: " + currentMovie.haveSeen);
	}
}
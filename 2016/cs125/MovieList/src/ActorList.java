
public class ActorList {
	protected String actor;
	protected ActorList next;

	public ActorList(String actor, ActorList next) {
		this.actor = actor;
		this.next = next;
	}

	public void addActor() {
		String name = "";
		while (!name.toUpperCase().equals("QUIT")) {
			System.out.println("Please Add Another Actor/Actress's name (\"QUIT\" to end the input): ");
			name = TextIO.getln();
			if(!name.toUpperCase().equals("QUIT")) append(name);
		}
	}

	private void append(String name) {
		if (next == null)
			this.next = new ActorList(name, next);
		else
			next.append(name);
	}

	public int listLength() {// Count the number of actors
		if (next == null)
			return 1;
		return next.listLength() + 1;
	}

	public String getName() {// Getter
		return actor;
	}

	public boolean actorExist(String name) {
		if (actor.equals(name))
			return true;
		if (next == null)
			return false;
		return next.actorExist(name);
	}
	
	public void printActors(){
		System.out.print(actor + " ");
		if(next == null) return;
		next.printActors();
	}
}

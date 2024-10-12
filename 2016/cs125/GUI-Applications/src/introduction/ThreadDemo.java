package introduction;

public class ThreadDemo extends Thread {

	
	public void run() {
		while (true)
			hello("H");
	}

	public static void main(String[] args) {
		ThreadDemo demo = new ThreadDemo();
		demo.start();
		while (true) {
			hello("2");
		}
	}

	private synchronized static void hello(String string) {
		System.out.println(string);
	}

}

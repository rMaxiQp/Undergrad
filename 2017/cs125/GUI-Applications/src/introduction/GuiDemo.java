package introduction;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

public class GuiDemo extends JFrame {
	
	
	public static void main(String[] args) {
		new GuiDemo();
	}
	
	public GuiDemo() { 
		
		JMenuBar bar = new JMenuBar();
		JMenu menu = new JMenu("File");
		JMenuItem openItem = new JMenuItem("Open...");
		
		setJMenuBar(bar);
		bar.add(menu);
		menu.add(openItem);
		
		JButton btn = new JButton("Dont click this!");
		getContentPane().add(btn);
		
		pack();
		setVisible(true);
	}
}

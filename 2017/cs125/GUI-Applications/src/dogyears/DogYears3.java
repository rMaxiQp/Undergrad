package dogyears;

// Original File   : gui-tutorial/DogYears2.java
// Purpose: Use textfields and button to convert dog to human years.
//          A button listener has been added in this version.
//          No error checking on input values.
// Author : Fred Swartz, 2006-11-09,  Placed in public domain.
// Additions: Added Menu Items (L. Angrave 2008-10-29)
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

//////////////////////////////////////////////////////// class DogYears2
class DogYears3 extends JFrame {

	private static final int TEXT_WIDTH = 3;

	// ======================================================== constants
	private static final int DOG_YEARS_PER_HUMAN_YEAR = 7;

	// =============================================== instance variables
	private JTextField _humanYearsTF = new JTextField(TEXT_WIDTH);
	private JTextField _dogYearsTF = new JTextField(TEXT_WIDTH);

	// =============================================== Actions
	class DisplayAboutMessage implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			JOptionPane.showMessageDialog(null,
					"Dog Years v.0.2 (Halloween Release)\nOriginal by F Swartz\nMenu Items by L Angrave 2008");
		}
	};

	class ConvertToHumanYears implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			try {
			// ... Get the value from the dog years textfield.
				String dyStr = _dogYearsTF.getText();
				int dogYears = Integer.parseInt(dyStr);
				// ... Convert it - each dog year is worth 7 human years.
				int humanYears = dogYears * DOG_YEARS_PER_HUMAN_YEAR;
				// ... Convert to string and set human yrs textfield
				_humanYearsTF.setText("" + humanYears);
			} catch(NumberFormatException ex) {
				_humanYearsTF.setText("Hey cat please get off the keyboard");
				_humanYearsTF.setToolTipText("Read the manual");
			}
		}
	}

	// ====================================================== constructor
	public DogYears3() {

		JButton convertBtn = new JButton("Convert");
		_humanYearsTF.setEditable(false);

		JMenuBar menuBarb = new JMenuBar();
		JMenu menu = new JMenu("My Menu Bar");
		JMenuItem convertMenuItem = new JMenuItem("Convert");
		JMenuItem aboutMenuItem = new JMenuItem("About...");
		menuBarb.add(menu);
		menu.add(convertMenuItem);
		menu.add(aboutMenuItem);
		setJMenuBar(menuBarb);
		try {
			setIconImage(ImageIO.read(getClass().getResourceAsStream("dog.jpg")));
		} catch (IOException e) {
			e.printStackTrace();
		}

		JPanel content = new JPanel();
		content.setLayout(new FlowLayout());

		content.add(new JLabel("Dog Years"));
		content.add(_dogYearsTF); // Add input field
		content.add(convertBtn); // Add button
		content.add(new JLabel("Human Years"));
		content.add(_humanYearsTF); // Add output field

		ActionListener convertToHumanYearsAction = new ConvertToHumanYears();
		ActionListener displayAboutMessage = new DisplayAboutMessage();

		convertBtn.addActionListener(convertToHumanYearsAction);
		convertMenuItem.addActionListener(convertToHumanYearsAction);
		_dogYearsTF.addActionListener(convertToHumanYearsAction);
		aboutMenuItem.addActionListener(displayAboutMessage);

		setContentPane(content);
		pack(); // Layout components.
		setTitle("Dog Year Converter");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLocationRelativeTo(null); // Center window.
	}

	public static void main(String[] args) {
		DogYears3 window = new DogYears3();
		window.setVisible(true);
	}
}
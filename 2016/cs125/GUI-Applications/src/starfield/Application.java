package starfield;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class Application extends JFrame {

	private StarCanvas starCanvas = new StarCanvas();
	private JTextArea textArea;
	private JTextField numStarEditField;
	private JCheckBox toggleRotationCheckBox;
	private JPanel controlPanel;
	private JPanel mainContent;

	class ToggleRotateStars extends AbstractAction {
		ToggleRotateStars() {
			putValue(Action.SHORT_DESCRIPTION, // Tooltip
					"Turns star rotations on and off");
		}

		public void actionPerformed(ActionEvent arg0) {
			toggleRotationCheckBox.setSelected(starCanvas.toggleRotationMode());
		}
	};

	class ChangeStarCount extends AbstractAction {
		public void actionPerformed(ActionEvent arg0) {
			int num = Integer.parseInt(numStarEditField.getText());
			if (num >= 0)
				starCanvas.setStarCount(num);

		}
	}

	public Application() {
		// set Icon
		try {
			setIconImage(ImageIO.read(getClass().getResourceAsStream(
					"starfield.jpg")));
		} catch (IOException e) {
			e.printStackTrace();
		}
		// Create the 3 main parts of the window
		starCanvas = new StarCanvas();
		textArea = new JTextArea("A star field demo for CS125");
		controlPanel = new JPanel(new FlowLayout()); // sub panel -

		mainContent = new JPanel();
		mainContent.setLayout(new BoxLayout(mainContent, BoxLayout.Y_AXIS));
		mainContent.add(starCanvas);
		mainContent.add(textArea);
		mainContent.add(controlPanel);

		setContentPane(mainContent);
		// Create the control panel
		numStarEditField = new JTextField(5);
		numStarEditField.setText("" + starCanvas.getNStars());
		numStarEditField.setAction(new ChangeStarCount());
		toggleRotationCheckBox = new JCheckBox(new ToggleRotateStars());
		toggleRotationCheckBox.setText("Rotation");
		controlPanel.add(toggleRotationCheckBox);
		controlPanel.add(numStarEditField);
		controlPanel.add(new JLabel("# Stars"));
		// set sizes
		starCanvas.setPreferredSize(new Dimension(300, 300));
		textArea.setMaximumSize(new Dimension(Integer.MAX_VALUE, 10));
		controlPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 10));
		pack();

		setTitle("I can see the stars");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setLocationRelativeTo(null); // Center window
		setVisible(true);

		Thread backgroundThread = new Thread("Tick tock") {
			public void run() {
				while (true) { // move,sleep,move,sleep...
					starCanvas.move();
					try {
						sleep(16);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			}
		};
		backgroundThread.setDaemon(true);
		backgroundThread.setPriority(Thread.MIN_PRIORITY);
		backgroundThread.start();
	}

	public static void main(String[] args) {
		new Application();
	}
}

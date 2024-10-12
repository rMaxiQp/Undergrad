package starfield;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

public class StarCanvas extends JPanel {
	private int nstars;
	private boolean rotate = false;
	private double[] starX, starY;

	public int getNStars() {
		return nstars;
	}

	public StarCanvas() {
		setStarCount(300);
		setOpaque(true); // we agree to paint the entire region
		setDoubleBuffered(true);
	}

	public boolean toggleRotationMode() {
		rotate = !rotate;
		return rotate;
	}

	/**
	 * Update the star positions. We use 'synchronized' to ensure that only one
	 * thread can be using this object at any one time e.g. the painting thread
	 * or the moving thread or GUI event thread to change the number of stars
	 */
	public synchronized void move() {
		for (int i = 0; i < nstars; i++) {
			if (Math.abs(starX[i]) > 0.5 || Math.abs(starY[i]) > .5) {
				starX[i] = (Math.random() - 0.5) / 300.;
				starY[i] = (Math.random() - 0.5) / 300.;
			} else {
				starX[i] *= 1.05;
				starY[i] *= 1.05;
				if (rotate) {
					double t = starX[i];
					starX[i] += 0.02 * starY[i];
					starY[i] -= 0.02 * t;
				}
			} // if-else
		} // for
		repaint();
	}

	public synchronized void update(Graphics g) {
		paint(g);
	}

	public synchronized void paint(Graphics g) {
		final double xScale = getWidth(), yScale = getHeight();
		g.setColor(Color.BLACK);
		g.fillRect(0, 0, getWidth(), getHeight());
		g.setColor(Color.WHITE);
		for (int i = 0; i < nstars; i++) {
			int x = (int) (xScale * (starX[i] + 0.5));
			int y = (int) (yScale * (starY[i] + 0.5));
			g.fillRect(x, y, 1, 1);
		}
	}

	public synchronized void setStarCount(int num) {
		starX = new double[num];
		starY = new double[num];
		nstars = num;
		for (int i = 0; i < nstars; i++) {
			double z = Math.pow(2, 1 + 10 * Math.random());
			starX[i] = (Math.random() - 0.5) / z;
			starY[i] = (Math.random() - 0.5) / z;
		}
	}
}

package main.gui;

import main.game.Game;

import javax.swing.*;
import java.awt.*;

import static javax.swing.JOptionPane.showMessageDialog;

public class GameGUI extends JFrame {

    private final int  windowWidth = 630;
    private final int  windowHeight = 630;
    private JFrame window;
    private Game game;
    private int[] score;

    public GameGUI() {
        score = new int[2];

        game = new Game();
        game.setGameGUI(this);
        setWindow();
    }

    /**
     * Generate GUI window
     */
    private void setWindow() {
        window = new JFrame("Chess");

        ImageIcon webIcon = new ImageIcon("src/resources/Icon.png");
        window.setIconImage(webIcon.getImage());

        window.setJMenuBar(new MenuBarGUI(this));

        setBoard();

        window.setSize( windowWidth, windowHeight );
        window.setLocationRelativeTo( null );
        window.setResizable( false );
        window.setDefaultCloseOperation(EXIT_ON_CLOSE);
        window.setVisible( true );
    }

    /**
     * Generate GUI board that is nested inside of GUI window
     */
    private void setBoard() {
        BoardGUI boardGUI = new BoardGUI( game );

        Container container = this.getContentPane();
        boardGUI.setBoardPanel( container );
        window.add( container );

        pack();
    }

    /**
     * Restart Game
     */
    public void restart() {
        int reply = JOptionPane.showConfirmDialog(null, "Try another match?", "Restart?",  JOptionPane.YES_NO_OPTION);
        if (reply == JOptionPane.NO_OPTION)
        {
            System.exit(0);
        }

        this.game.setGameGUI(null);
        this.game = new Game();
//        game.setGameGUI(this);
    }

    /**
     * Wrapper function that undo the most recent move
     */
    public void undo() {
        this.game.undo();
    }

    /**
     * Surrender, and update opponent's score
     */
    public void surrender() {
        showMessageDialog(null, "You Surrendered...");

        int winner = (this.game.getCurrentTeam() + 1 ) % 2;

        this.updateWinner(winner);
    }

    /**
     * helper function used to display score when there is a update
     */
    public void updateWinner(int team) {
        score[team] ++;
        showMessageDialog(null ,score[0] + " : " + score[1]);
    }

}

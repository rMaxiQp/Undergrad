package main.gui;

import main.chess.Chess;
import main.game.Game;
import main.util.CoordinatePair;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ChessGUI extends JButton implements ActionListener {

    private CoordinatePair position;
    private Game game;
    private BoardGUI boardGUI;


    /**
     * Extended from JButton Class, generate Clickable Chess with icon to indicate identity
     *
     * */
    public ChessGUI(Chess chess, int x, int y) {
        super();
        setBackground(Color.BLACK);
        setForeground(Color.GRAY);
        setPreferredSize(new Dimension(60,60));
        setMinimumSize(new Dimension(50,50));
        setMaximumSize(new Dimension(70,70));

        position = new CoordinatePair(x, y);

        if (chess != null) {
            ImageIcon icon = new ImageIcon("src/resources/" + chess.getName() + chess.getTeam() + ".png");
            setIcon(icon);
        }

        this.addActionListener(this);

    }

    public void setGame(Game game) {
        this.game = game;
    }

    /**
     * Overriding them for default properties
     */
//    @Override
//    public boolean isOpaque() {
//        return false;
//    }

    @Override
    public boolean isContentAreaFilled() {
        return false;
    }

    @Override
    public boolean isBorderPainted() {
        return false;
    }

    @Override
    public boolean isFocusPainted() {
        return false;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        int xAxis = position.getX();
        int yAxis = position.getY();
        Chess currentPicked = game.getCurrentChess();
        if (currentPicked == null) {
            if ( game.pickChess(xAxis, yAxis) ) {
                this.boardGUI.highlightMoves(game.getBuffer());
            }
        } else {
            this.boardGUI.resetHighlightMoves(game.getBuffer());

            if ( game.moveChess(xAxis, yAxis) ) {
                this.boardGUI.updateIcon();
            }
        }
    }

    public void setBoard(BoardGUI board) {
        this.boardGUI = board;
    }
}

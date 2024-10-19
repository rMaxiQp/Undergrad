package main.gui;

import main.board.Board;
import main.chess.Chess;
import main.game.Game;
import main.util.CoordinatePair;

import javax.swing.*;
import java.awt.*;
import java.util.List;

public class BoardGUI {
    private JLabel[][] boardGUI;
    private JButton[][] buttonGUI;
    private Board board;
    private int width;
    private int height;
    private Game game;
    private final Color colorBlue = new Color(102,127,255);
    private final Color colorRed = new Color(255,102,102);


    /**
     * Constructor, generate Labels as board background
     */
    public BoardGUI(Game game) {
        Color color;
        JLabel backgroundLabel;
        this.game = game;
        this.board = game.getBoard();
        this.width = board.getWidth();
        this.height = board.getHeight();

        this.boardGUI = new JLabel[width][height];

        for (int col = 0; col < width; col++) {
            for (int row = 0; row < height; row++) {

                color = (col + row) % 2 == 1 ? Color.black : Color.white;

                backgroundLabel = new JLabel();

                backgroundLabel.setMinimumSize(new Dimension(70,70));
                backgroundLabel.setOpaque(true);
                backgroundLabel.setLayout(new FlowLayout(FlowLayout.CENTER));
                backgroundLabel.setBackground(color);
                this.boardGUI[col][row] = backgroundLabel;
            }
        }
    }

    /**
     * Generate Board Panel
     *
     * @param container used to contain the panel
     */
    public void setBoardPanel(Container container) {

        JPanel panel = new JPanel();

        container.add(panel);
        container.setLayout(new GridBagLayout());

        renderBackground(panel);

        renderChess();
    }

    /**
     * Generate Board Background
     *
     * @param panel used to hold all labels
     */
    private void renderBackground(JPanel panel) {
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);

        GroupLayout.ParallelGroup parallelGroup = layout.createParallelGroup();
        GroupLayout.SequentialGroup sequentialGroup = layout.createSequentialGroup();

        for (int row = 0; row < width; row++) {
            GroupLayout.ParallelGroup subSequential = layout.createParallelGroup(GroupLayout.Alignment.CENTER);
            GroupLayout.SequentialGroup subParallel = layout.createSequentialGroup();

            for(int col = 0; col < height; col++) {
                subSequential.addComponent(boardGUI[col][row]);
                subParallel.addComponent(boardGUI[col][row]);
            }
            parallelGroup.addGroup(subParallel);
            sequentialGroup.addGroup(subSequential);
        }

        layout.setHorizontalGroup(parallelGroup);
        layout.setVerticalGroup(sequentialGroup);
    }

    /**
     * render Chess button
     */
    private void renderChess() {
        buttonGUI = new JButton[height][width];
        for(int row = 0; row < width; row ++) {
            for(int col = 0; col < height; col++) {
                buttonGUI[col][row] = new ChessGUI( board.at(col, row), col, row);
                ((ChessGUI)buttonGUI[col][row]).setGame( game );
                ((ChessGUI)buttonGUI[col][row]).setBoard( this );
                boardGUI[col][row].add( buttonGUI[col][row] );
            }
        }
    }

    /**
     * highlight valid moves
     * @param validMoves a list of valid moves
     */
    protected void highlightMoves(List<CoordinatePair> validMoves) {
        int xAxis, yAxis;
        Chess chess;
        for (CoordinatePair point : validMoves) {
            xAxis = point.getX();
            yAxis = point.getY();
            chess = board.at(xAxis, yAxis);

            if (chess == null) {
                buttonGUI[xAxis][yAxis].setBackground(colorBlue);
            } else {
                buttonGUI[xAxis][yAxis].setBackground(colorRed);
            }

            buttonGUI[xAxis][yAxis].setOpaque(true);
        }
    }

    protected void resetHighlightMoves(List<CoordinatePair> validMoves) {
        int xAxis, yAxis;
        for (CoordinatePair point : validMoves) {
            xAxis = point.getX();
            yAxis = point.getY();
            buttonGUI[xAxis][yAxis].setBackground(null);
            buttonGUI[xAxis][yAxis].setOpaque(false);
        }
    }

    protected void updateIcon() {
        Chess chess;
        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                chess = board.at(col, row);
                if (chess != null) {
                    ImageIcon icon = new ImageIcon("src/resources/" + chess.getName() + chess.getTeam() + ".png");
                    buttonGUI[col][row].setIcon(icon);
                } else {
                    buttonGUI[col][row].setIcon(null);
                }
            }
        }
    }
}

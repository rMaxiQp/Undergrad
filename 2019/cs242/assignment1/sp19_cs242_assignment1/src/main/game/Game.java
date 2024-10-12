package main.game;

import main.board.Board;
import main.chess.Chess;
import main.gui.GameGUI;
import main.util.CoordinatePair;

import java.util.List;

import static javax.swing.JOptionPane.showMessageDialog;

/**
 * Game Init -- + --> Pick Chess --> Move Chess --> *Update Moves* -- + --> Empty Moves --> King Check -- > CheckMate
 *              |                                                     |                         |
 *              |                                                     |                         |
 *              +     <---------    Switch Turn     <-----------      +                         + --> StaleMate
 *
 * */

public class Game {

    private Board board;

    private int currentTeam;

    private Chess currentChess;

    private GameGUI gameGUI;

    private List<CoordinatePair> validMoves;

    /**
     * -1 when no king is checked
     * N when team N's King is checked
     */
    private int check;

    public Game() {
        board = new Board();
        currentTeam = 0;
        currentChess = null;
        check = -1;
        board.updateAllChess();
    }

    public void setGameGUI(GameGUI gameGUI) {
        this.gameGUI = gameGUI;
    }

    /**
     * lock a chess in given location
     *
     * @param x x-axis
     * @param y y-axis
     * @return true when chess is locked successfully, false otherwise
     */
    public boolean pickChess(int x, int y) {
        Chess pickedChess = board.at(x, y);

        if (pickedChess == null) {
            return false;
        }

        if (pickedChess.getTeam() != currentTeam) {
            return false;
        }

        currentChess = pickedChess;

        validMoves = pickedChess.getBuffer();

        return true;
    }

    /**
     * move the locked chess to given location
     *
     * @param x x-axis of destination
     * @param y y-axis of destination
     * @return true when chess is moved successfully, false otherwise
     */
    public boolean moveChess(int x, int y) {
        if (currentChess == null) {
            return false;
        }

        CoordinatePair nextPosition = new CoordinatePair(x, y);
        CoordinatePair currentPosition = currentChess.getPosition();

        currentChess = null;

        if ( !validMoves.contains(nextPosition) ) {
            return false;
        }

        board.moveChess(currentPosition, nextPosition);

        board.updateAllChess();

        updateCheck();

        switchPlayer();

        return true;
    }

    /**
     * check if endCondition has met, use the fact of whether current player's King is checked or not
     * to differentiate Checkmate and Stalemate
     *
     * @return true when endCondition is satisfied, false otherwise
     */
    public boolean endCondition() {

        if (noLegalMove()) {

            if (check == currentTeam) {
                System.out.println("Checkmate...");
            } else {
                System.out.println("Stalemate...");
            }

            return true;
        }

        return false;
    }

    /**
     * helper function for endCondition(), check if there is still a valid move to make
     *
     * @return true when no legal moves left for current player --> endCondition is satisfied, false otherwise
     */
    private boolean noLegalMove() {

        Chess[] currentChess = board.getTeam(currentTeam);

        for (Chess item : currentChess) {
            if (item.getStatus() == 0) {
                continue;
            }

            if (item.getBuffer().size() > 0) {
                return false;
            }
        }

        return true;
    }


    /**
     * check the status of King on opponentTeam
     *
     * update variable check
     */
    private void updateCheck() {
        int opponentTeam = (currentTeam + 1) % 2;

        Chess king = board.getKing(opponentTeam);
        CoordinatePair kingPos =  king.getPosition();

        check = -1;

        Chess[] currentChess = board.getTeam(currentTeam);
        for ( Chess item : currentChess) {
            if (item.getStatus() == 0) {
                continue;
            }

            List<CoordinatePair> captureList = item.getBuffer();
            if (captureList.contains(kingPos)) {
                showMessageDialog(null, "Player" + currentTeam + " Raised Check");
                check = opponentTeam;
                if (endCondition()) {
                    showMessageDialog(null, "Player" + currentTeam + " Win...");
                    gameGUI.restart();
                }
                break;
            }
        }
    }

    /**
     * switchPlayer by changing currentTeam
     */
    private void switchPlayer() {
        currentTeam = (currentTeam + 1) % 2;
    }

    /**
     * @return board of this game
     */
    public Board getBoard() {
        return this.board;
    }
    /**
     * @return current picked chess
     */
    public Chess getCurrentChess() {
        return this.currentChess;
    }

    /**
     * @return current chess's buffer
     */
    public List<CoordinatePair> getBuffer() {
        return this.validMoves;
    }

    /**
     * @return current team
     */
    public int getCurrentTeam() {
        return this.currentTeam;
    }

    /**
     * undo the most recent movement
     */
    public void undo() {

    }
}

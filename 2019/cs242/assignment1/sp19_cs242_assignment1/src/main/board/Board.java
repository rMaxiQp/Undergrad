package main.board;

import main.chess.*;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * *Filter Buffer*
 *
 *     Will King (Still Be/ Be) Checked? -- Yes --> False
 *                                           |
 *                                           +  --> True
 */

public class Board extends Object {

    Chess[][] board;

    Chess[][] chessTeams;

    private final int height = 8;

    private final int width = 8;

    public Board() {
        chessTeams = initChess();
        board = initBoard();
    }

    /**
     * get Chess of the given location, return null when the input is invalid
     *
     * @param x x-axis
     * @param y y-axis
     *
     * @return Chess object on the given location
     */
    public Chess at(int x, int y) {
        if (outBound(x, y)) {
            return null;
        }

        return board[x][y];
    }


    /**
     * @return height
     */
    public int getHeight() {
        return height;
    }

    /**
     * @return width
     */
    public int getWidth() {
        return width;
    }

    /**
     * @param team
     * @return Chess[] chess of the given player
     */
    public Chess[] getTeam(int team) {
        return chessTeams[team];
    }

    /**
     * @param team
     * @return Chess King of the given player
     */
    public Chess getKing(int team) {
        return chessTeams[team][12];
    }

    /**
     * Helper function used to determine if the input is out of bound
     *
     * @param x x-axis
     * @param y y-axis
     */
    public boolean outBound(int x, int y) {
        return x < 0 || y < 0 || x >= width || y >= height;
    }

    /**
     * Create a hard copy of caller Object
     */
    @Override
    public Object clone() throws CloneNotSupportedException {
        Board newBoard = new Board();
        newBoard.board = new Chess[newBoard.width][newBoard.height];
        Chess current;
        CoordinatePair position;

        for (int team = 0; team < 2; team++) {
            for (int item = 0; item < 16; item++) {
                current = this.chessTeams[team][item].getCopy();
                position = current.getPosition();
                newBoard.board[position.getX()][position.getY()] = current;
                newBoard.chessTeams[team][item] = current;
            }
        }

        return newBoard;
    }


    /**
     * Move chess on board, invalid input would cause error in this function.
     * It is expected to be a helper function only.
     *
     * @param origin Current Position of the chess
     * @param attempt The destination of the chess is going to
     */
    public void moveChess(CoordinatePair origin, CoordinatePair attempt) {

        int originX = origin.getX();
        int originY = origin.getY();
        Chess chess = at(originX, originY);

        int attemptX = attempt.getX();
        int attemptY = attempt.getY();
        Chess other = at(attemptX, attemptY);

        if ( outBound(attemptX, attemptY) ) {
            return;
        }

        if (other != null) {
            chess.capture(other);
        }

        chess.setPosition(attempt);
        board[attemptX][attemptY] = board[originX][originY];
        board[originX][originY] = null;
    }

    /**
     * Update buffer of each alive chess, then filter moves that may lead King to check situation.
     *
     * Flow Chart:
     *  Update Buffer[updateAllBuffer()] --> Filter Buffer[filterBuffer()] --> Update Move or Capture [assignBuffer()*]
     *
     * NOTE:
     * Player only make one move, so each move is independent to each other, we can combine filter and update
     * in one iteration. Thus, assignBuffer() is called inside of filterBuffer() instead
     *
     */
    public void updateAllChess()  {

        this.updateAllBuffer();

        try {
            this.filterBuffer();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Update buffer of each alive chess by move and capture behaviors
     *
     */
    private void updateAllBuffer() {
        for (int team = 0; team < 2; team++) {
            for (Chess chessItem : chessTeams[team]) {
                if (chessItem.getStatus() == 0) continue;

                chessItem.setBuffer( chessItem.update(this) );
            }
        }
    }

    /**
     * Create a hard copied board to simulate likely consequence of each move, and filter out illegal moves
     *
     * @throws CloneNotSupportedException inherited from Object.clone()
     */
    private void filterBuffer() throws CloneNotSupportedException {
        for (int team = 0; team < 2; team ++) {
            for (Chess chessItem : this.chessTeams[team]) {

                if (chessItem.getStatus() == 0) continue;

                List<CoordinatePair> buffer = chessItem.getBuffer();
                List<CoordinatePair> filteredBuffer = new ArrayList<>();

                for (CoordinatePair point : buffer) {
                    if (filter(chessItem.getPosition(), point)) {
                        filteredBuffer.add(point);
                    }
                }

                chessItem.setBuffer(filteredBuffer);
            }
        }
    }


    /**
     * Simulate possible moves of each chess after one possible move of one chess
     *
     * @param origin current coordinate of given chess
     * @param attempt next coordinate of given chess
     *
     * @return boolean True[valid move]/ False[Invalid move]
     *
     * @throws CloneNotSupportedException inherited from Object.clone()
     */
    private boolean filter(CoordinatePair origin, CoordinatePair attempt) throws CloneNotSupportedException {
        Board clonedBoard = (Board) this.clone();

        int originX = origin.getX();
        int originY = origin.getY();
        int status;
        int currentTeam = clonedBoard.at(originX, originY).getTeam();
        int opponentTeam = ( currentTeam + 1 ) % 2;

        clonedBoard.moveChess(origin, attempt);

        clonedBoard.updateAllBuffer();

        Chess king = clonedBoard.getKing(currentTeam);
        CoordinatePair kingPos = king.getPosition();

        for (Chess chessItem : clonedBoard.chessTeams[opponentTeam]) {
            status = chessItem.getStatus();
            if (status == 0) {
                continue;
            }

            List<CoordinatePair>buffer = chessItem.getBuffer();
            if (buffer.contains(kingPos)) {
                return false;
            }
        }

        return true;
    }


    /**
     * @return initialized chessTeams
     */
    private Chess[][] initChess() {
        Chess[][] result = new Chess[2][16];
        int [] yIndex = {1, 0, this.height - 2, this.height - 1};
        int yIndexCounter = 0;
        int currentY, index;

        for (int team = 0; team < 2; team++) {

            currentY = yIndex[yIndexCounter++];

            for (index = 0; index < 8; index++) {
                result[team][index] = new Pawn(new CoordinatePair(index, currentY), team);
            }

            currentY = yIndex[yIndexCounter++];

            result[team][index] = new Rook(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Knight(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Bishop(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Queen(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new King(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Bishop(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Knight(new CoordinatePair(index % this.width, currentY), team);
            result[team][++index] = new Rook(new CoordinatePair(index % this.width, currentY), team);
        }

        return result;
    }

    /**
     * @return initialized Chess Board from given chessTeams
     */
    private Chess[][] initBoard() {
        Chess[][] result = new Chess[this.width][this.height];
        Chess current;
        CoordinatePair position;

        for (int team = 0; team < 2; team++) {
            for (int i = 0; i < 16; i++) {
                current = this.chessTeams[team][i];
                position = current.getPosition();
                result[position.getX()][position.getY()] = current;
            }
        }

        return result;
    }
}

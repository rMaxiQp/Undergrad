package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.List;

public class Bishop extends Chess {

    public Bishop() {
        super();
    }

    public Bishop(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Bishop";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Bishop(position, team);
        result.setStatus(this.getStatus());
        return result;
    }

    /**
     * @param board
     * @return List<CoordinatePair> return raw possible moves of a Bishop
     */
    @Override
    public List<CoordinatePair> update(Board board) {

        CoordinatePair upperBound = new CoordinatePair(board.getWidth(), board.getHeight());
        CoordinatePair lowerBound = new CoordinatePair(-1, -1);

        return updateDiagonal(board, upperBound, lowerBound);
    }
}

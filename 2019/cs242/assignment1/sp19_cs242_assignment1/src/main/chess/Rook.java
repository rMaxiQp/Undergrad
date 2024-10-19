package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.List;

public class Rook extends Chess {

    public Rook() {
        super();
    }

    public Rook(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Rook";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Rook(position, team);
        result.setStatus(this.getStatus());
        return result;
    }

    /**
     * Generate Possible Moves on vertical or horizontal direction
     *
     * @param board given board
     * @return Possible Moves
     */
    @Override
    public List<CoordinatePair> update(Board board) {

        CoordinatePair upperBound = new CoordinatePair(board.getWidth(), board.getHeight());
        CoordinatePair lowerBound = new CoordinatePair(-1, -1);

        return updateCross(board, upperBound, lowerBound);
    }
}

package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

public class King extends Chess {

    public King() {
        super();
    }

    public King(CoordinatePair position, int team) {
        super(position, team);
        this.name = "King";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new King(position, team);
        result.setStatus(this.getStatus());
        return result;
    }

    /**
     * @param board
     * @return List<CoordinatePair> return raw possible moves of a King
     */
    @Override
    public List<CoordinatePair> update(Board board) {
        List<CoordinatePair> result = new ArrayList<>();

        int currentX = position.getX();
        int currentY = position.getY();

        int upperX = (currentX != board.getWidth() - 1) ? (currentX + 2) : board.getWidth();
        int upperY = (currentY != board.getHeight() - 1) ? (currentY + 2) : board.getHeight();

        int lowerX = (currentX != 0) ? (currentX - 2) : -1;
        int lowerY = (currentY != 0) ? (currentY - 2) : -1;

        CoordinatePair upperBound = new CoordinatePair(upperX, upperY);
        CoordinatePair lowerBound = new CoordinatePair(lowerX, lowerY);

        result.addAll(updateCross(board, upperBound, lowerBound));
        result.addAll(updateDiagonal(board, upperBound, lowerBound));

        return result;
    }
}

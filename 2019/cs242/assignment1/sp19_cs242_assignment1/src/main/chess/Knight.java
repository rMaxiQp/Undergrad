package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

public class Knight extends Chess {

    public Knight() {
        super();
    }

    public Knight(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Knight";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Knight(position, team);
        result.setStatus(this.getStatus());
        return result;
    }


    /**
     * @param board given board
     * @return List<CoordinatePair> return raw possible moves of a Knight
     */
    @Override
    public List<CoordinatePair> update(Board board) {
        List<CoordinatePair> result = new ArrayList<>();
        int currentX = position.getX();
        int currentY = position.getY();


        if (moveL(board, -2, -1)) {
            result.add(new CoordinatePair(currentX - 2, currentY - 1));
        }

        if (moveL(board, -2, 1)) {
            result.add(new CoordinatePair(currentX - 2, currentY + 1));
        }

        if (moveL(board, 2, 1)) {
            result.add(new CoordinatePair(currentX + 2, currentY + 1));
        }

        if (moveL(board, 2, -1)) {
            result.add(new CoordinatePair(currentX + 2, currentY - 1));
        }

        if (moveL(board, 1, -2)) {
            result.add(new CoordinatePair(currentX + 1, currentY - 2));
        }

        if (moveL(board, 1, 2)) {
            result.add(new CoordinatePair(currentX + 1, currentY + 2));
        }

        if (moveL(board, -1, -2)) {
            result.add(new CoordinatePair(currentX - 1, currentY - 2));
        }

        if (moveL(board, -1, 2)) {
            result.add(new CoordinatePair(currentX - 1, currentY + 2));
        }

        return result;
    }
}

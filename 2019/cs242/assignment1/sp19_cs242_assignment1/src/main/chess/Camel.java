package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

/**
 * https://en.wikipedia.org/wiki/Camel_(chess)
 *
 * Jumps 2 squares orthogonally followed by one square diagonally outwards.
 */
public class Camel extends Chess {

    public Camel() {
        super();
    }

    public Camel(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Camel";
    }

    @Override
    public List<CoordinatePair> update(Board board) {
        int currentX = position.getX();
        int currentY = position.getY();

        List<CoordinatePair> result = new ArrayList<>();

        if (moveL(board, -1, -3)) {
            result.add(new CoordinatePair(currentX - 1, currentY - 3));
        }

        if (moveL(board, 1, -3)) {
            result.add(new CoordinatePair(currentX + 1, currentY - 3));
        }

        if (moveL(board, 3, -1)) {
            result.add(new CoordinatePair(currentX + 3, currentY - 1));
        }

        if (moveL(board, -3, -1)) {
            result.add(new CoordinatePair(currentX - 3, currentY - 1));
        }

        if (moveL(board, -1, 3)) {
            result.add(new CoordinatePair(currentX - 1, currentY + 3));
        }

        if (moveL(board, 1, 3)) {
            result.add(new CoordinatePair(currentX + 1, currentY + 3));
        }

        if (moveL(board, 3, 1)) {
            result.add(new CoordinatePair(currentX + 3, currentY + 1));
        }

        if (moveL(board, -3, 1)) {
            result.add(new CoordinatePair(currentX - 3, currentY + 1));
        }

        return result;
    }
}

package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

/**
 * https://en.wikipedia.org/wiki/Fairy_chess_piece
 *
 * Bishop/Knight-hunter: moves forward as a bishop, and backward as a knight.
 */
public class Bishight extends Chess {

    public Bishight() {
        super();
    }

    public Bishight(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Bishight";
    }

    @Override
    public List<CoordinatePair> update(Board board) {
        int currentX = position.getX();
        int currentY = position.getY();
        int sign = this.team == 0 ? 1 : -1;
        CoordinatePair upperBound = new CoordinatePair(board.getWidth(), board.getHeight());
        CoordinatePair lowerBound = new CoordinatePair(currentX, currentY);
        List<CoordinatePair> result = new ArrayList<>();

        result.addAll(updateCross(board, upperBound, lowerBound));

        if (moveL(board, sign * -1, sign * -2)) {
            result.add(new CoordinatePair(currentX - sign * 1, currentY - sign * 2));
        }

        if (moveL(board, sign * 1, sign * -2)) {
            result.add(new CoordinatePair(currentX + sign * 1, currentY - sign * 2));
        }

        if (moveL(board, sign * 2, sign * -1)) {
            result.add(new CoordinatePair(currentX + sign * 2, currentY - sign * 1));
        }

        if (moveL(board, sign * -2, sign * -1)) {
            result.add(new CoordinatePair(currentX - sign * 2, currentY - sign * 1));
        }

        return result;
    }

}

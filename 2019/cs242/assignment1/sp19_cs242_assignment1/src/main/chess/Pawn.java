package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.ArrayList;
import java.util.List;

public class Pawn extends Chess {

    public Pawn() {
        super();
    }

    public Pawn(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Pawn";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Pawn(position, team);
        result.setStatus(this.getStatus());
        return result;
    }

    /**
     * @param board given board
     * @return Possible Moves (Move + Capture)
     */
    @Override
    public List<CoordinatePair> update(Board board) {
        List<CoordinatePair> result = new ArrayList<>();

        result.addAll(updateMove(board));

        result.addAll(updateCapture(board));

        return result;
    }

    /**
     * @param board given board
     * @return Possible Moves
     */
    private List<CoordinatePair> updateMove(Board board) {
        List<CoordinatePair> result = new ArrayList<>();
        int currentX = position.getX();
        int currentY = position.getY();
        int firstMoveLine = team == 0 ? 1 : (board.getHeight() - 2);
        int direction = team == 0 ? 1 : -1;

        if (board.at(currentX, currentY + direction) == null) {
            result.add(new CoordinatePair(currentX, currentY + direction));

            if (firstMoveLine == currentY) {
                if (board.at(currentX, currentY + direction * 2) == null) {
                    result.add(new CoordinatePair(currentX, currentY + direction * 2));
                }
            }
        }

        return result;
    }

    /**
     * @param board
     * @return Possible Captures
     */
    private List<CoordinatePair> updateCapture(Board board) {
        int currentX = position.getX();
        int currentY = position.getY();
        int direction = team == 0 ? 1 : -1;
        Chess leftChess = board.at(currentX - 1, currentY + direction);
        Chess rightChess = board.at(currentX + 1, currentY + direction);
        List<CoordinatePair> result = new ArrayList<>();

        if (leftChess != null && leftChess.getTeam() != this.team) {
            result.add(leftChess.getPosition());
        }

        if (rightChess != null && rightChess.getTeam() != this.team) {
            result.add(rightChess.getPosition());
        }

        return result;
    }
}

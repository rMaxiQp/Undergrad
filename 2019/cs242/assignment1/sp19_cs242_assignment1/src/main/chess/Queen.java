package main.chess;

import main.util.CoordinatePair;

public class Queen extends Chess {

    public Queen() {
        super();
    }

    public Queen(CoordinatePair position, int team) {
        super(position, team);
        this.name = "Queen";
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Queen(position, team);
        result.setStatus(this.getStatus());
        return result;
    }
}

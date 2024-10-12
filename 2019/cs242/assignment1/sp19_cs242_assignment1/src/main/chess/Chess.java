package main.chess;

import main.board.Board;
import main.util.CoordinatePair;

import java.util.List;
import java.util.ArrayList;

public class Chess extends Object {

    CoordinatePair position;

    List<CoordinatePair> buffer;

    String name;

    int team;

    int status;

    public Chess() {

    }

    public Chess(CoordinatePair position, int team) {
        this.name = "Chess";
        this.position = position;
        this.team = team;
        this.status = 1;
        buffer = new ArrayList<>();
    }

    /**
     * return a hard copied Object from caller Object
     *
     * @return Object hard copied Object
     * @throws CloneNotSupportedException
     */
    @Override
    protected Object clone() throws CloneNotSupportedException {
        Chess result = new Chess(position, team);
        result.setStatus(this.getStatus());
        return result;
    }

    protected void setStatus(int status) {
        this.status = status;
    }

    /**
     * @return CoodPair Chess's position
     */
    public CoordinatePair getPosition() {
        return position;
    }

    /**
     * @param position update position
     */
    public void setPosition(CoordinatePair position) {
        this.position = position;
    }

    /**
     * @return List<CoordinatePair> buffer
     */
    public List<CoordinatePair> getBuffer() {
        return buffer;
    }

    /**
     * @param  buffer update buffer
     */
    public void setBuffer(List<CoordinatePair> buffer) {
        this.buffer = buffer;
    }

    /**
     * @return int status of this Object (alive[1]/ dead[0])
     */
    public int getStatus() {
        return status;
    }

    /**
     * @return String class name of this Object
     */
    public String getName() {
        return name;
    }

    /**
     * @return int team of this chess
     */
    public int getTeam() {
        return team;
    }

    /**
     * wrapper function for clone()
     *
     * @return Chess hard copied Chess Object
     */
    public Chess getCopy() {
        try {
            return (Chess) this.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * set other chess's status to 0
     *
     * @param other given chess to be captured
     */
    public void capture(Chess other) {
        other.status = 0;
    }


    /**
     * Generate possible moves of the given chess on the given board
     *
     * @param board given board
     * @return List<Coordpair> Possible Moves
     */
    public List<CoordinatePair> update(Board board) {
        List<CoordinatePair> result = new ArrayList<>();

        CoordinatePair upperBound = new CoordinatePair(board.getWidth(), board.getHeight());
        CoordinatePair lowerBound = new CoordinatePair(-1, -1);

        result.addAll(updateCross(board, upperBound, lowerBound));
        result.addAll(updateDiagonal(board, upperBound, lowerBound));

        return result;
    }

    /**
     * Generate possible horizontal or vertical moves of the given chess on the given board in a given limit
     *
     * @param board
     * @param upperBound maximum x and y values (exclusive)
     * @param lowerBound minimum x and y values (exclusive)
     * @return List<Coordpair> Possible moves
     */
    protected List<CoordinatePair> updateCross(Board board, CoordinatePair upperBound, CoordinatePair lowerBound) {
        List<CoordinatePair> result = new ArrayList<>();

        result.addAll(updateHelper(board, upperBound, lowerBound, -1, 0));
        result.addAll(updateHelper(board, upperBound, lowerBound, 1, 0));
        result.addAll(updateHelper(board, upperBound, lowerBound, 0, 1));
        result.addAll(updateHelper(board, upperBound, lowerBound, 0, -1));

        return result;
    }

    /**
     * Generate possible diagonal moves of the given chess on the given board in a given limit
     *
     * @param board
     * @param upperBound maximum x and y values (exclusive)
     * @param lowerBound minimum x and y values (exclusive)
     * @return List<Coordpair> Possible moves
     */
    protected List<CoordinatePair> updateDiagonal(Board board, CoordinatePair upperBound, CoordinatePair lowerBound) {
        List<CoordinatePair> result = new ArrayList<>();

        result.addAll(updateHelper(board, upperBound, lowerBound, 1, 1));
        result.addAll(updateHelper(board, upperBound, lowerBound, 1, -1));
        result.addAll(updateHelper(board, upperBound, lowerBound, -1, 1));
        result.addAll(updateHelper(board, upperBound, lowerBound, -1, -1));

        return result;
    }


    /**
     * Generate possible moves of the given chess on the given board in a given limit by given direction
     *
     * @param board given board
     * @param upperBound maximum x and y values (exclusive)
     * @param lowerBound minimum x and y values (exclusive)
     * @param dirX magnitude and sign of each step on x axis
     * @param dirY magnitude and sign of each step on y axis
     * @return List<Coordpair> Possible moves
     */
    private List<CoordinatePair> updateHelper(Board board, CoordinatePair upperBound, CoordinatePair lowerBound, int dirX, int dirY) {
        List<CoordinatePair> result = new ArrayList<>();

        int upperX = upperBound.getX();
        int upperY = upperBound.getY();

        int lowerX = lowerBound.getX();
        int lowerY = lowerBound.getY();

        int currentX = position.getX() + dirX;
        int currentY = position.getY() + dirY;
        Chess chess;

        while ( currentX > lowerX && currentX < upperX &&
                currentY > lowerY && currentY < upperY ) {

            chess = board.at(currentX, currentY);



            if (chess == null || chess.getTeam() != this.team) {
                result.add( new CoordinatePair(currentX, currentY));
            }

            if (chess != null) {
                break;
            }

            currentX += dirX;
            currentY += dirY;
        }

        return result;
    }

    /**
     * @param board given board
     * @param directionX magnitude and sign on X axis
     * @param directionY magnitude and sign on Y axis
     *
     * @return boolean
     */
    protected boolean moveL(Board board, int directionX, int directionY) {
        int currentX = position.getX();
        int currentY = position.getY();

        if (board.outBound(currentX + directionX, currentY + directionY)) {
            return false;
        }

        Chess otherChess = board.at(currentX + directionX, currentY + directionY);

        return otherChess == null || otherChess.getTeam() != this.team;
    }

    @Override
    public boolean equals(Object obj) {
        Chess other = (Chess) obj;
        return this.team == other.team && this.name.equals(other.name);
    }
}

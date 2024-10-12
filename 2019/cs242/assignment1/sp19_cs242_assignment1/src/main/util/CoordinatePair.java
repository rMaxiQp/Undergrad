package main.util;

public class CoordinatePair extends Object {
    private int x;

    private int y;

    public CoordinatePair(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return x;
    }

    public int getY() {
        return y;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof CoordinatePair)) {
            return false;
        }

        CoordinatePair other = (CoordinatePair) obj;

        return this.x == other.getX() && this.y == other.getY();
    }
}

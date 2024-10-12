package test;

import main.board.Board;
import main.chess.*;
import main.util.CoordinatePair;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

public class BoardTest {
    private Board board;

    @Before
    public void restart() throws Exception {
        board = new Board();
    }

    @Test
    public void testGetKing() {
        Chess chess = board.getKing(0);
        assertTrue( chess instanceof King );
        chess = board.getKing(1);
        assertTrue( chess instanceof King );
    }

    @Test
    public void testGetChess() {
        int i;
        for (int team = 0; team < 2; team++ ) {
            Chess[] chessTeam = board.getTeam(team);

            assertEquals(16, chessTeam.length);

            for ( i = 0; i < 8; i++) {
                assertTrue(chessTeam[i] instanceof Pawn);
            }

            assertTrue(chessTeam[i++] instanceof Rook);
            assertTrue(chessTeam[i++] instanceof Knight);
            assertTrue(chessTeam[i++] instanceof Bishop);
            assertTrue(chessTeam[i++] instanceof Queen);
            assertTrue(chessTeam[i++] instanceof King);
            assertTrue(chessTeam[i++] instanceof Bishop);
            assertTrue(chessTeam[i++] instanceof Knight);
            assertTrue(chessTeam[i++] instanceof Rook);

        }
    }

    @Test
    public void testAt() {
        assertEquals(null, board.at(8,8));
        assertEquals(null, board.at(3,3));
        assertTrue(board.at(0,1) instanceof Pawn);
        assertTrue(board.at(1,0) instanceof Knight);
        assertTrue(board.at(0,7) instanceof Rook);
        assertTrue(board.at(2,7) instanceof Bishop);
        assertTrue(board.at(3,7) instanceof Queen);
        assertTrue(board.at(3,0) instanceof Queen);
    }

    @Test
    public void testMoveChess() {
        assertEquals(null, board.at(0,2));
        board.moveChess(new CoordinatePair(0,1), new CoordinatePair(0,2));
        assertTrue(board.at(0,2) instanceof Pawn);
    }

    @Test
    public void testBuffer() {
        Chess chess = board.at(0,1);
        assertEquals(new CoordinatePair(0,1), chess.getPosition());
        assertEquals(0, chess.getBuffer().size());

        Chess otherChess = board.at(0,0);
        assertEquals(new CoordinatePair(0,0), otherChess.getPosition());
        assertEquals(0, otherChess.getBuffer().size());

        board.updateAllChess();

        assertEquals(2, chess.getBuffer().size());
        assertEquals(0, otherChess.getBuffer().size());

        board.moveChess(new CoordinatePair(0,1), new CoordinatePair(0,2));
        board.updateAllChess();

        assertEquals(1, chess.getBuffer().size());
        assertEquals(1, otherChess.getBuffer().size());
    }

    @Test
    public void testChess() {
        Chess chess = new Camel();
        assertTrue( chess instanceof Camel);

        chess = new Bishight();
        assertTrue( chess instanceof Bishight);
    }

    @Test
    public void testClone() {
        board.moveChess(new CoordinatePair(0,1), new CoordinatePair(0,2));
        board.moveChess(new CoordinatePair(0,6), new CoordinatePair(0,4));
        board.moveChess(new CoordinatePair(3,1), new CoordinatePair(0,3));
        board.moveChess(new CoordinatePair(0,7), new CoordinatePair(0,6));

        try {
            Board other = (Board) board.clone();
            for (int i = 0; i < 8; i++) {
                for (int j = 0; j < 8; j++) {
                    assertEquals(other.at(i,j), board.at(i,j));
                }
            }
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
    }
}

package test;

import main.game.Game;
import org.junit.Before;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

public class GameTest {

    private Game game;

    @Before
    public void setUpGame() {
        game = new Game();
    }

    @Test
    public void testPickChess() {

        assertEquals(false, game.pickChess(9, -1));

        assertEquals(false, game.pickChess(0, 7));

        assertEquals(false, game.pickChess(5, 5));

        assertEquals(true, game.pickChess(0, 0));
    }

    @Test
    public void testMoveChess() {
        assertEquals(true, game.pickChess(0,1));

        assertEquals(false, game.moveChess(3,3));

        assertEquals(false, game.moveChess(0,3));

        assertEquals(false, game.moveChess(0,4));

        assertEquals(true, game.pickChess(0,1));
    }


    @Test
    public void testSwitchTurns() {
        assertEquals(true, game.pickChess(0,1));

        assertEquals(true, game.moveChess(0,3));

        assertEquals(false, game.moveChess(0,4));

        assertEquals(true, game.pickChess(0,7));

        assertEquals(true, game.pickChess(0,6));

        assertEquals(true, game.moveChess(0,5));
    }
}

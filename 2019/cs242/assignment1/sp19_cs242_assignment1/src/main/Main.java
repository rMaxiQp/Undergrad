package main;

import main.gui.GameGUI;

import java.awt.*;

public class Main {
    public static void main(String argv[]) {
        /*
         * http://zetcode.com/tutorials/javaswingtutorial/firstprograms/
         *
         * The invokeLater() method places the application on the Swing Event Queue.
         * It is used to ensure that all UI updates are concurrency-safe.
         * In other words, it is to prevent GUI from hanging in certain situations.
         */
        EventQueue.invokeLater(() -> {
            GameGUI gameGUI = new GameGUI();
        });
    }
}

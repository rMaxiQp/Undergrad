package main.util;

import main.gui.GameGUI;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class SurrenderListener implements ActionListener {

    private GameGUI gameGUI;

    public SurrenderListener (GameGUI game) {
        this.gameGUI = game;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        gameGUI.surrender();
        gameGUI.restart();
    }
}
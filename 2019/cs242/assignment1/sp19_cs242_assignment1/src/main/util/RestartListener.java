package main.util;

import main.gui.GameGUI;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class RestartListener implements ActionListener {


    private GameGUI gameGUI;

    public RestartListener (GameGUI game) {
        this.gameGUI = game;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        gameGUI.restart();
    }
}

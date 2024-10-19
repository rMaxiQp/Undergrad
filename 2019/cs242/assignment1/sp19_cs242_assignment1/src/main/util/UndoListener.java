package main.util;

import main.gui.GameGUI;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class UndoListener implements ActionListener {

    private GameGUI gameGUI;

    public UndoListener(GameGUI gameGUI) {
        this.gameGUI = gameGUI;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        this.gameGUI.undo();
    }
}

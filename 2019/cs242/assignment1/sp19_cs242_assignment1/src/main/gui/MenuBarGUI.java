package main.gui;

import main.util.ExitListener;
import main.util.RestartListener;
import main.util.SurrenderListener;
import main.util.UndoListener;

import javax.swing.*;
import java.awt.event.KeyEvent;

public class MenuBarGUI extends JMenuBar {

    private GameGUI gameGUI;

    public MenuBarGUI(GameGUI game) {
        this.gameGUI = game;
        setMenuBar();
    }

    /**
     * Generate Customized Menu Bar
     */
    private void setMenuBar() {

        // Add Menu
        JMenu optionMenu = new JMenu("Option");
        optionMenu.setMnemonic(KeyEvent.VK_O);
        add(optionMenu);

        // Add Menu Item
        JMenuItem surrender = new JMenuItem("Surrender", KeyEvent.VK_S);
        surrender.addActionListener(new SurrenderListener(gameGUI));

        JMenuItem exit = new JMenuItem("Exit", KeyEvent.VK_E);
        exit.addActionListener(new ExitListener());

        JMenuItem undo = new JMenuItem("Undo", KeyEvent.VK_U);
        undo.addActionListener(new UndoListener(gameGUI));

        JMenuItem restart = new JMenuItem("Restart", KeyEvent.VK_R);
        restart.addActionListener(new RestartListener(gameGUI));

        optionMenu.add(restart);
        optionMenu.add(undo);
        optionMenu.add(surrender);
        optionMenu.add(exit);
    }
}

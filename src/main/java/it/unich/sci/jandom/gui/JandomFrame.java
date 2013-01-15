package it.unich.sci.jandom.gui;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JEditorPane;
import java.awt.BorderLayout;
import javax.swing.JTabbedPane;
import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

@SuppressWarnings("serial")
public class JandomFrame extends JFrame {

	private JPanel contentPane;
	private JEditorPane editorPane;
	private JButton btnAnalyze;
	private JEditorPane outputPane;
	private JTabbedPane tabbedPane;

	/**
	 * This is an enumeration for the tabs of the GUI
	 * @author Gianluca Amato
	 */
	public enum Tabs {
		EDITOR (0), 
		OUTPUT (1), 
		SETTINGS (2);
		
		int pos;
		Tabs (int pos) {
			this.pos = pos;
		}
	}
	
	/**
	 * Create the frame.
	 */
	public JandomFrame() {
		setTitle("Jandom GUI");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setBounds(100, 100, 450, 300);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);
		contentPane.setLayout(new BorderLayout(0, 0));
		
		tabbedPane = new JTabbedPane(JTabbedPane.TOP);
		contentPane.add(tabbedPane);
		
		editorPane = new JEditorPane();
		
		JScrollPane scrollPane = new JScrollPane(editorPane);
		tabbedPane.addTab("Editor", null, scrollPane, null);		
		
		outputPane = new JEditorPane();
		
		JScrollPane scrollPane_1 = new JScrollPane(outputPane);
		tabbedPane.addTab("Output", null, scrollPane_1, null);
		
		JPanel panel = new JPanel();
		tabbedPane.addTab("Parameters", null, panel, null);
		
		btnAnalyze = new JButton("ANALYZE");
		contentPane.add(btnAnalyze, BorderLayout.SOUTH);
		
		JMenuBar menuBar = new JMenuBar();
		contentPane.add(menuBar, BorderLayout.NORTH);
		
		JMenu mnNewMenu = new JMenu("File");
		menuBar.add(mnNewMenu);
		
		JMenuItem mntmNew = new JMenuItem("New");
		mntmNew.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmNew);
		
		JMenuItem mntmOpen = new JMenuItem("Open...");
		mntmOpen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmOpen);
		
		JSeparator separator = new JSeparator();
		mnNewMenu.add(separator);
		
		JMenuItem mntmSave = new JMenuItem("Save");
		mntmSave.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmSave);
		
		JMenuItem mntmSaveAs = new JMenuItem("Save As...");
		mnNewMenu.add(mntmSaveAs);
		
		JSeparator separator_1 = new JSeparator();
		mnNewMenu.add(separator_1);
		
		JMenuItem mntmQuit = new JMenuItem("Quit");
		mntmQuit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				System.exit(0);
			}
		});
		mntmQuit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmQuit);
	}

	public JEditorPane getEditorPane() {
		return editorPane;
	}
	public JButton getBtnAnalyze() {
		return btnAnalyze;
	}
	public JEditorPane getOutputPane() {
		return outputPane;
	}
	public void switchToTab(Tabs newtab) {
		tabbedPane.setSelectedIndex(newtab.pos);
	}
	
}

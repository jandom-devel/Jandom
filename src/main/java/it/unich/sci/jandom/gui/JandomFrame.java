package it.unich.sci.jandom.gui;

import it.unich.sci.jandom.domains.*;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;
import java.awt.GridBagLayout;
import javax.swing.JLabel;
import java.awt.GridBagConstraints;
import javax.swing.JComboBox;
import java.awt.Insets;
import javax.swing.border.EmptyBorder;

@SuppressWarnings("serial")
public class JandomFrame extends JFrame {

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
		setBounds(100, 100, 800, 600);
		JPanel contentPane = new JPanel();
		setContentPane(contentPane);
		contentPane.setLayout(new BorderLayout(0, 0));
		
		tabbedPane = new JTabbedPane(JTabbedPane.TOP);
		contentPane.add(tabbedPane);
		
		JEditorPane editorPane = new JEditorPane();
		final EditorController editorController = new EditorController(editorPane);
		
		JScrollPane scrollPane = new JScrollPane(editorPane);
		tabbedPane.addTab("Editor", null, scrollPane, null);		
		
		JEditorPane outputPane = new JEditorPane();
		outputPane.setEditable(false);
		final AnalysisController analysisController = new AnalysisController(editorPane, outputPane);
		
		JScrollPane scrollPane_1 = new JScrollPane(outputPane);
		tabbedPane.addTab("Output", null, scrollPane_1, null);
		
		JPanel panel = new JPanel();
		panel.setBorder(new EmptyBorder(5, 5, 5, 5));
		tabbedPane.addTab("Parameters", null, panel, null);
		GridBagLayout gbl_panel = new GridBagLayout();
		gbl_panel.columnWidths = new int[]{0, 0, 0};
		gbl_panel.rowHeights = new int[]{0, 0, 0};
		gbl_panel.columnWeights = new double[]{0.0, 1.0, Double.MIN_VALUE};
		gbl_panel.rowWeights = new double[]{0.0, 0.0, Double.MIN_VALUE};
		panel.setLayout(gbl_panel);
		
		JLabel lblDomain = new JLabel("Domain:");
		GridBagConstraints gbc_lblDomain = new GridBagConstraints();
		gbc_lblDomain.insets = new Insets(0, 0, 5, 5);
		gbc_lblDomain.gridx = 0;
		gbc_lblDomain.gridy = 0;
		panel.add(lblDomain, gbc_lblDomain);
		
		JComboBox comboBox = new JComboBox();
		GridBagConstraints gbc_comboBox = new GridBagConstraints();
		gbc_comboBox.insets = new Insets(0, 0, 5, 0);
		gbc_comboBox.fill = GridBagConstraints.HORIZONTAL;
		gbc_comboBox.gridx = 1;
		gbc_comboBox.gridy = 0;
		panel.add(comboBox, gbc_comboBox);
		
		JButton btnAnalyze = new JButton("ANALYZE");
		btnAnalyze.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				analysisController.analyze();
				switchToTab(Tabs.OUTPUT);
			}
		});
		contentPane.add(btnAnalyze, BorderLayout.SOUTH);		
		
		JMenuBar menuBar = new JMenuBar();
		contentPane.add(menuBar, BorderLayout.NORTH);
		
		JMenu mnNewMenu = new JMenu("File");
		menuBar.add(mnNewMenu);
		
		JMenuItem mntmNew = new JMenuItem("New");
		mntmNew.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) { 
				editorController.clear(); 
			}
		});
		mntmNew.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmNew);
		
		JMenuItem mntmOpen = new JMenuItem("Open...");
		mntmOpen.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editorController.open();
			}
		});
		mntmOpen.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmOpen);
		
		JSeparator separator = new JSeparator();
		mnNewMenu.add(separator);
		
		JMenuItem mntmSave = new JMenuItem("Save");
		mntmSave.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editorController.save();
			}
		});
		mntmSave.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK));
		mnNewMenu.add(mntmSave);
		
		JMenuItem mntmSaveAs = new JMenuItem("Save As...");
		mntmSaveAs.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editorController.saveAs();
			}
		});
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

	public void switchToTab(Tabs newtab) {
		tabbedPane.setSelectedIndex(newtab.pos);
	}	
}

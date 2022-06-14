Demo that LCL behavior is here compatible with Delphi VCL.

In Delphi also menu shortcuts have priority, and menu shortcut like Home, f,
prevents from using Home, f in expected way on a TEdit and TComboBox.

Moreover, in Delphi, menu shortcuts have priority even over TTreeView Home/End.
In this sample menu item Home prevents TTreeView from interpreting Home to "move to top".

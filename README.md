# lcl-test-menu-special-shortcuts

In Lazarus LCL (and also in Delphi VCL) if you have a menu item with shortcut Home, F, Ctrl+Z -- this prevents from using these shortcuts in normal fashion in edit boxes (like TEdit and TComboBox).

This is quite troublesome for _Castle Game Engine_ editor, that

- wants to have some simple natural shortcuts for various operations (Home, F, Ctrl+Z) as they are very useful to design components

- ... but also wants to enable using these shortcuts for normal editing in TEdit / TComboBox when they are focused.

In this repo, we explore a workaround (a really, really, really big hack...) to this.

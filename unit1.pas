unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  StdCtrls, ComCtrls, CastleControl, CastleLCLUtils, CastleKeysMouse,
  CastleNotifications;

type
  TForm1 = class(TForm)
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    CastleControl1: TCastleControl;
    CheckBoxKeyMenuHack: TCheckBox;
    Edit1: TEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    TreeView1: TTreeView;
    procedure ActionAnyExecute(Sender: TObject);
    procedure CastleControl1Open(Sender: TObject);
    procedure CastleControl1Press(Sender: TObject;
      const Event: TInputPressRelease);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure MenuItemAnyClick(Sender: TObject);
  private
    InsideFormKeyDown: Boolean;
    CastleNotifs: TCastleNotifications;
  public

  end;

var
  Form1: TForm1;

implementation

uses LCLType, LCLMessageGlue, StrUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  { Whether pressing a letter should make it upper?
    It sucks, but in this workaround we cannot depend on KeyPress reaching us
    (it doesn't reach us when menu item intercepts the key)
    so we need to calculate key here. }
  function LettersUpCase: Boolean;
  begin
    Result := false;
    { TODO: Windows-only:
    GetKeyboardState(KeyState);
    if KeyState[VK_CAPITAL] := 1 then
      Result := true; }
    if ssShift in Shift then
      Result := not Result;
  end;

var
  S: String;
begin
  { This is a hack to
    - Have some menu items with simple shortcuts, like Home, F, Ctrl+Z
      (and *visible* in meun with these shortcuts, to be discoverable)
    - but still allow user to interact with text input (like TEdit)
      using Home, F, Ctrl+Z etc.

    The intuitive result should be:
    - using Home, F, Ctrl+Z etc. when TEdit is focused, makes normal text-editing
      operation
    - only if no text-editing is focused,
      it goes to the specific custom action we wanted to assign to it.

    Rejected workarounds:
    - use only shortcuts that avoid common text-editing.
      Like Ctrl+Home, Ctrl+F, Ctrl+Alt+Z.
      Rejected because these are important shortcuts.
      For ease of use, for consistency with other software --
      we want to keep them easy.
    - use simple shortcuts, but not set them as menu item shortcuts.
      Just dispatch them yourself if e.g. TCastleControl is focused.
      Rejected: then UI sucks, we cannot show shortcuts in menu in standard way.

    This workaround depends on Lazarus behaviors (tested on WinAPI, GTK, Cocoa
    backends):
    - Key being used to activate menu item is still send to TForm1.FormKeyDown
      when form has KeyPreview
    - We can resign from "normal" key handling (which means running menu item
      for these special keys) by assigning Key := 0
    - Our 2 important text-editing states are TEdit and editing in TTreeView,
      the latter is luckily just an internal TEdit too.

    Note: This is still ugly and inheretntly unreliable workaround, because we
    interpret what some special built-in keys should do. So we need to
    capture all such keys, and implement same logic...
    E.g. pressing "f" key, with and without Shift, with and without CapsLock,
    should be consistent with system-handled "g" key.
    - What happens if we don't include special handling for some key,
      but we should? In the worst case it will not work with edit boxes,
      and always activate menu command.
    - What happens if we included unnecessarily some key in this treatment?
      Everything should be OK, if only we simulate default behavior OK.

    Tested: It is necessary, and good workaround,
    - (FPC 3.2.2, LCL 2.2) on Windows with WinAPI widgetset.
    - (FPC 3.2.2, LCL 2.2) on Linux with GTK widgetset.

    Note: TTreeView also handles Home / End.
    Strangely, it behaves like we want out-of-the-box:
    it intercepts Home / End when focused, regardless of if some menu item
    has such shortcut.
    Tested this is true on:
    - (FPC 3.2.2, LCL 2.2) on Windows with WinAPI widgetset.
    - (FPC 3.2.2, LCL 2.2) on Linux with GTK widgetset.
  }

  if not CheckBoxKeyMenuHack.Checked then
    Exit;

  { Avoid infinite loop since all LCLSendKeyDownEvent
    allow form to preview keys. }
  if InsideFormKeyDown then Exit;
  InsideFormKeyDown := true;
  try
    S := 'FormKeyDown: ' + IntToStr(Key);
    if ssShift in Shift then S += ' +Shift';
    if ssCtrl in Shift then S += ' +Ctrl';
    if ssAlt in Shift then S += ' +Alt';
    if ssMeta in Shift then S += ' +Meta';
    if Shift - [ssShift, ssCtrl, ssAlt, ssMeta] <> [] then S += ' +otherModifier';
    Memo1.Lines.Add(S);

    { Below I was testing LCLSendKeyDownEvent usefulness for this hack:

      https://wiki.lazarus.freepascal.org/LCL_Tips
      function LCLSendKeyDownEvent(const Target: TControl; var CharCode: word;
        KeyData: PtrInt; BeforeEvent, IsSysKey: boolean): PtrInt;

    // This is not useful -- would execute menu item action 2x
    //LCLSendKeyDownEvent(Self, Key, 0, true, false);

    // Not useful - sadly this doesn't invoke editing operations on TEdit
    //if (ActiveControl <> nil) and
    //   (ActiveControl <> Self) then
    //  LCLSendKeyDownEvent(ActiveControl, Key, 0, false, false);
    }

    if ActiveControl is TEdit then
    begin
      case Key of
        VK_HOME: TEdit(ActiveControl).SelStart := 0;
        VK_0..VK_9: TEdit(ActiveControl).SelText := Chr(Ord('0') + Key - VK_0);
        VK_F: TEdit(ActiveControl).SelText := IfThen(LettersUpCase, 'F', 'f');
        VK_Z:
          if Shift = [ssCtrl] then
            TEdit(ActiveControl).Undo
          else
            Exit; // resign from special handling
          //if Shift = [ssCtrl, ssShift] then
          //  TEdit(ActiveControl).Redo
          //else
        else
          Exit; // resign from special handling
      end;

      Memo1.Lines.Add('special key handling, focus is ' +
        (ActiveControl as TEdit).Name + ':' + ActiveControl.ClassName);
      Key := 0; // prevent key reaching normal menu/action event
    end;
  finally
    InsideFormKeyDown := false;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  // not executed when menu items handle it

  Memo1.Lines.Add('has key press ' + Key); //

  // Key := #0; // would prevent any key reaching TEdit
end;

procedure TForm1.ActionAnyExecute(Sender: TObject);
begin
  Memo1.Lines.Add((Sender as TComponent).Name);
end;

procedure TForm1.CastleControl1Open(Sender: TObject);
begin
  CastleNotifs := TCastleNotifications.Create(Self);
  CastleControl1.Controls.InsertFront(CastleNotifs);
end;

procedure TForm1.CastleControl1Press(Sender: TObject;
  const Event: TInputPressRelease);
begin
  CastleNotifs.Show('Pressed: ' + Event.ToString);
end;

procedure TForm1.MenuItemAnyClick(Sender: TObject);
begin
  Memo1.Lines.Add((Sender as TComponent).Name);
end;

end.


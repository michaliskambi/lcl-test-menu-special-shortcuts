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
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
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

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  LCLType, LCLMessageGlue, StrUtils;

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
    // TODO: This is Windows-only.
    {$ifdef MSWINDOWS}
    { See https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getkeystate .
      Read low-order bit of VK_CAPITAL (caps lock) state. }
    if (GetKeyState(VK_CAPITAL) and 1) <> 0 then
      Result := true;
    {$endif}
    if ssShift in Shift then
      Result := not Result;
  end;

  function SEnding(const S: string; P: integer): string;
  begin
    result := Copy(S, P, MaxInt)
  end;

var
  S: String;
  E: TEdit;
  SavedSelStart: Integer;
begin
  { This is a hack to
    - Have some menu items with simple shortcuts, like Home, F, Ctrl+Z
      (and *visible* in menu with these shortcuts, to be discoverable)
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

    Note: This is still ugly and inherently unreliable workaround, because we
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
    - (FPC 3.2.2, LCL 2.2) on macOS with Cocoa widgetset.

    Note: TTreeView also handles Home / End.
    Strangely, it behaves like we want out-of-the-box:
    it intercepts Home / End when focused, regardless of if some menu item
    has such shortcut.
    Tested this is true on:
    - (FPC 3.2.2, LCL 2.2) on Windows with WinAPI widgetset.
    - (FPC 3.2.2, LCL 2.2) on Linux with GTK widgetset.
    - (FPC 3.2.2, LCL 2.2) on macOS with Cocoa widgetset.
  }

  if not CheckBoxKeyMenuHack.Checked then
    Exit;

  { Avoid infinite loop since all LCLSendKeyDownEvent
    allow form to preview keys. }
  (*
  if InsideFormKeyDown then Exit;
  InsideFormKeyDown := true;
  try
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
  finally
    InsideFormKeyDown := false;
  end;
  *)

  S := 'FormKeyDown: ' + IntToStr(Key);
  if ssShift in Shift then S += ' +Shift';
  if ssCtrl in Shift then S += ' +Ctrl';
  if ssAlt in Shift then S += ' +Alt';
  if ssMeta in Shift then S += ' +Meta';
  if Shift - [ssShift, ssCtrl, ssAlt, ssMeta] <> [] then S += ' +otherModifier';
  Memo1.Lines.Add(S);

  if ActiveControl is TEdit then
  begin
    E := TEdit(ActiveControl);
    case Key of
      VK_HOME:
        begin
          if Shift = [ssShift] then
          begin
            SavedSelStart := E.SelStart;
            E.SelStart := 0;
            E.SelLength := SavedSelStart;
          end else
            E.SelStart := 0
        end;
      {.$ifdef LCLCocoa}
      { If user didn't adjust Home/End system-wide, then actually
        by default Home/End do nothing in TEdit.
        So we could disable Home...
        but it seems more useful to make Home/End just work in CGE TEdit.

        For easier testing, we enable our End handling on all platforms. }
      VK_END:
        begin
          if Shift = [ssShift] then
            E.SelLength := Length(E.Text) - E.SelStart
          else
            E.SelStart := Length(E.Text);
        end;
      {.$endif}
      VK_0..VK_9:
        begin
          if E.ReadOnly then
            Beep
          else
            E.SelText := Chr(Ord('0') + Key - VK_0);
        end;
      VK_F:
        begin
          if E.ReadOnly then
            Beep
          else
            E.SelText := IfThen(LettersUpCase, 'F', 'f');
        end;
      VK_Z:
        if Shift = [ssCtrl] then
          E.Undo
        else
          Exit; // resign from special handling
        //if Shift = [ssCtrl, ssShift] then
        //  E.Redo
        //else
      VK_C:
        if Shift = [ssCtrl] then
          E.CopyToClipboard
        else
          Exit; // resign from special handling
      VK_V:
        if Shift = [ssCtrl] then
        begin
          if E.ReadOnly then
            Beep
          else
            E.PasteFromClipboard;
        end else
          Exit; // resign from special handling
      VK_X:
        if Shift = [ssCtrl] then
        begin
          if E.ReadOnly then
            Beep
          else
            E.CutToClipboard
        end else
          Exit; // resign from special handling
      VK_DELETE:
        if E.ReadOnly then
          Beep
        else
        begin
          if E.SelText <> '' then
            E.SelText := ''
          else
          begin
            SavedSelStart := E.SelStart;
            E.Text :=
              Copy(E.Text, 1, E.SelStart) +
              SEnding(E.Text, E.SelStart + 2);
            E.SelStart := SavedSelStart;
          end;
        end;
      else
        Exit; // resign from special handling
    end;

    Memo1.Lines.Add('special key handling, focus is ' +
      (ActiveControl as TEdit).Name + ':' + ActiveControl.ClassName);
    Key := 0; // prevent key reaching normal menu/action event
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


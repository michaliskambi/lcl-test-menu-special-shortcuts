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
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
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
  LCLType, LCLMessageGlue, LMessages,
  StrUtils,
  CastleLclEditHack;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  S: String;
  E: TEditBox;

  (*
  Msg: TLMKey;

  TLMKey = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
    Unused: Word;
{$ELSE}
    Unused: Word;
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
{$ENDIF}
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    KeyData: PtrInt;
    Result: LRESULT;
  end;
  *)

begin
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

  if (ActiveControl is TComboBox) and
     (TComboBox(ActiveControl).Style.HasEditBox) then
  begin
    E := TEditBoxForComboBox.Create(TComboBox(ActiveControl));
    try
      E.ProcessKey(Key, Shift);
    finally FreeAndNil(E) end;
  end;

  if ActiveControl is TEdit then
  begin
    { See https://www.freepascal.org/docs-html/ref/refsu31.html about calling message methods. }
    // Doesn't work, unfortunately.

    //FillChar(Msg, SizeOf(Msg), #0);
    //Msg.Msg := LM_KEYDOWN;
    //Msg.CharCode := Key;
    //TEdit(ActiveControl).Dispatch(Msg);

    //FillChar(Msg, SizeOf(Msg), #0);
    //Msg.Msg := LM_CHAR;
    //Msg.CharCode := Ord('f');
    //TEdit(ActiveControl).Dispatch(Msg);

    E := TEditBoxForEdit.Create(TEdit(ActiveControl));
    try
      E.ProcessKey(Key, Shift);
    finally FreeAndNil(E) end;
  end;

  if Key = 0 then
  begin
    Memo1.Lines.Add('special key handling, focus is ' +
      ActiveControl.Name + ':' +
      ActiveControl.ClassName);
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


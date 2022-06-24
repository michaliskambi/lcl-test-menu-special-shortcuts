unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses LazUTF8;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  Label1.Caption := Format('Edit1: SelStart %d SelText %s SelLength %d, text len %d %d', [
    Edit1.SelStart,
    Edit1.SelText,
    Edit1.SelLength,
    Length(Edit1.SelText),
    UTF8Length(Edit1.SelText)
  ]);
  Label2.Caption := Format('ComboBox1: SelStart %d SelText %s SelLength %d, text len %d %d', [
    ComboBox1.SelStart,
    ComboBox1.SelText,
    ComboBox1.SelLength,
    Length(Edit1.SelText),
    UTF8Length(Edit1.SelText)
  ]);
end;

procedure TForm1.Button1Click(Sender: TObject);

  function UTF8SEnding(const S: string; P: integer): string;
  begin
    result := UTF8Copy(S, P, MaxInt)
  end;

begin
  Edit1.Text :=
    UTF8Copy(Edit1.Text, 1, Edit1.SelStart) +
    UTF8SEnding(Edit1.Text, Edit1.SelStart + 2)
  ;
end;

end.


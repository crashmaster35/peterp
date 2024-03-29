{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExExtCtrls.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvExExtCtrls.pas 10613 2006-05-19 19:21:43Z jfudickar $

// Initial port to Lazarus by Sergio Samayoa - september 2007.
// Conversion is done in incremental way: as types / classes / routines
// are needed they are converted.

unit JvExExtCtrls;

{MACROINCLUDE JvExControls.macros}

{*****************************************************************************
 * WARNING: Do not edit this file.
 * This file is autogenerated from the source in devtools/JvExVCL/src.
 * If you do it despite this warning your changes will be discarded by the next
 * update of this file. Do your changes in the template files.
 ****************************************************************************}
{$D-} // do not step into this unit

interface

uses
  Classes, Controls, ExtCtrls, Forms, Graphics, JvExControls, LCLIntf, LMessages;

type
  //******************** NOT CONVERTED
  //CONTROL_DECL_DEFAULT(Shape)

  //******************** NOT CONVERTED
  //CONTROL_DECL_DEFAULT(PaintBox)

  //******************** NOT CONVERTED
  //CONTROL_DECL_DEFAULT(Image)

  //******************** NOT CONVERTED
  //CONTROL_DECL_DEFAULT(Bevel)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(CustomPanel)

  (******************** NOT CONVERTED
  TJvExPubCustomPanel = class(TJvExCustomPanel)
  COMMON_PUBLISHED
  end;
  ******************** NOT CONVERTED *)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(CustomRadioGroup)

  TJvExSplitter = class(TSplitter)
  private
    // TODO:
    // FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FMouseOver: Boolean;
    FHintWindowClass: THintWindowClass;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    function BaseWndProc(Msg: Integer; WParam: Integer = 0; LParam: Longint = 0): Integer; overload;
    function BaseWndProc(Msg: Integer; WParam: Integer; LParam: TControl): Integer; overload;
    function BaseWndProcEx(Msg: Integer; WParam: Integer; var LParam): Integer;
  protected
    procedure WndProc(var Msg: TLMessage); override;
    procedure FocusChanged(AControl: TWinControl); dynamic;
    procedure VisibleChanged; reintroduce; dynamic;
    procedure EnabledChanged; reintroduce; dynamic;
    procedure TextChanged; reintroduce; virtual;
    procedure ColorChanged; reintroduce; dynamic;
    procedure FontChanged; reintroduce; dynamic;
    procedure ParentFontChanged; reintroduce; dynamic;
    procedure ParentColorChanged; reintroduce; dynamic;
    procedure ParentShowHintChanged; reintroduce; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean; reintroduce; virtual;
    function HintShow(var HintInfo: THintInfo): Boolean; reintroduce; dynamic;
    function HitTest(X, Y: Integer): Boolean; reintroduce; virtual;
    procedure MouseEnter(AControl: TControl); reintroduce; dynamic;
    procedure MouseLeave(AControl: TControl); reintroduce; dynamic;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property HintColor: TColor read FHintColor write FHintColor default clDefault;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    function GetCaption: TCaption; virtual;
    procedure SetCaption(Value: TCaption); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: TCaption read GetCaption write SetCaption;
    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  published
    // TODO:
    // property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(CustomControlBar)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(ControlBar)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(Panel)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(RadioGroup)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(Page)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(Notebook)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(Header)


  //******************** NOT CONVERTED
  //CONTROL_DECL_DEFAULT(BoundLabel)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(CustomLabeledEdit)

  //******************** NOT CONVERTED
  //WINCONTROL_DECL_DEFAULT(LabeledEdit)

  //******************** NOT CONVERTED - Exists in LCL?
  //WINCONTROL_DECL_DEFAULT(CustomColorBox)

  //******************** NOT CONVERTED - Exists in LCL?
  //WINCONTROL_DECL_DEFAULT(ColorBox)

implementation

//******************** NOT CONVERTED
//CONTROL_IMPL_DEFAULT(Shape)

//******************** NOT CONVERTED
//CONTROL_IMPL_DEFAULT(PaintBox)

//******************** NOT CONVERTED
//CONTROL_IMPL_DEFAULT(Image)

//******************** NOT CONVERTED
//CONTROL_IMPL_DEFAULT(Bevel)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(CustomPanel)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(CustomRadioGroup)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(CustomControlBar)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(ControlBar)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(Panel)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(RadioGroup)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(Page)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(Notebook)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(Header)

//******************** NOT CONVERTED
//CONTROL_IMPL_DEFAULT(BoundLabel)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(CustomLabeledEdit)

//******************** NOT CONVERTED
//WINCONTROL_IMPL_DEFAULT(LabeledEdit)

//******************** NOT CONVERTED - Exists in LCL?
//WINCONTROL_IMPL_DEFAULT(CustomColorBox)

//******************** NOT CONVERTED - Exists in LCL?
//WINCONTROL_IMPL_DEFAULT(ColorBox)

constructor TJvExSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clDefault;
end;

function TJvExSplitter.BaseWndProc(Msg: Integer; WParam: Integer = 0; LParam: Longint = 0): Integer;
var
  Mesg: TLMessage;
begin
  Mesg := CreateWMMessage(Msg, WParam, LParam);
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;

function TJvExSplitter.BaseWndProc(Msg: Integer; WParam: Integer; LParam: TControl): Integer;
var
  Mesg: TLMessage;
begin
  Mesg := CreateWMMessage(Msg, WParam, LParam);
  inherited WndProc(Mesg);
  Result := Mesg.Result;
end;

function TJvExSplitter.BaseWndProcEx(Msg: Integer; WParam: Integer; var LParam): Integer;
var
  Mesg: TStructPtrMessage;
begin
  Mesg := TStructPtrMessage.Create(Msg, WParam, LParam);
  try
    inherited WndProc(Mesg.Msg);
  finally
    Result := Mesg.Msg.Result;
    Mesg.Free;
  end;
end;

procedure TJvExSplitter.VisibleChanged;
begin
  BaseWndProc(CM_VISIBLECHANGED);
end;

procedure TJvExSplitter.EnabledChanged;
begin
  BaseWndProc(CM_ENABLEDCHANGED);
end;

procedure TJvExSplitter.TextChanged;
begin
  BaseWndProc(CM_TEXTCHANGED);
end;

procedure TJvExSplitter.FontChanged;
begin
  BaseWndProc(CM_FONTCHANGED);
end;

procedure TJvExSplitter.ColorChanged;
begin
  BaseWndProc(CM_COLORCHANGED);
end;

procedure TJvExSplitter.ParentFontChanged;
begin
  // LCL doesn't send this message but left it in case
  //BaseWndProc(CM_PARENTFONTCHANGED);
end;

procedure TJvExSplitter.ParentColorChanged;
begin
  BaseWndProc(CM_PARENTCOLORCHANGED);
  if Assigned(OnParentColorChange) then
    OnParentColorChange(Self);
end;

procedure TJvExSplitter.ParentShowHintChanged;
begin
  BaseWndProc(CM_PARENTSHOWHINTCHANGED);
end;

function TJvExSplitter.WantKey(Key: Integer; Shift: TShiftState; const KeyText: WideString): Boolean;
begin
  Result := BaseWndProc(CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExSplitter.HitTest(X, Y: Integer): Boolean;
begin
  Result := BaseWndProc(CM_HITTEST, 0, SmallPointToLong(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

function TJvExSplitter.HintShow(var HintInfo: THintInfo): Boolean;
begin
  GetHintColor(HintInfo, Self, FHintColor);
  if FHintWindowClass <> nil then
    HintInfo.HintWindowClass := FHintWindowClass;
  Result := BaseWndProcEx(CM_HINTSHOW, 0, HintInfo) <> 0;
end;

procedure TJvExSplitter.MouseEnter(AControl: TControl);
begin
  FMouseOver := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  BaseWndProc(CM_MOUSEENTER, 0, AControl);
end;

procedure TJvExSplitter.MouseLeave(AControl: TControl);
begin
  FMouseOver := False;
  BaseWndProc(CM_MOUSELEAVE, 0, AControl);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvExSplitter.FocusChanged(AControl: TWinControl);
begin
  BaseWndProc(CM_FOCUSCHANGED, 0, AControl);
end;

function TJvExSplitter.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

// 25.09.2007 - SESS:
// I have done this because TextChanged wasn't fired as expected.
// I still don't shure if this problem is only for this reintroduced
// method because the way LCL treats Caption or will have the same
// problem with other reintroduced methods. So far, I tested some
// other events and seems not.
procedure TJvExSplitter.SetCaption(Value: TCaption);
begin
  inherited Caption := Value;
  TextChanged;
end;

procedure TJvExSplitter.WndProc(var Msg: TLMessage);
begin
  if not DispatchIsDesignMsg(Self, Msg) then
  case Msg.Msg of
    {
    // TODO: do we need this? I think not...
    CM_DENYSUBCLASSING:
      Msg.Result := Ord(GetInterfaceEntry(IJvDenySubClassing) <> nil);
    }
    CM_DIALOGCHAR:
      with TCMDialogChar(Msg) do
        Result := Ord(WantKey(CharCode, KeyDataToShiftState(KeyData), WideChar(CharCode)));
    CM_HINTSHOW:
      with TCMHintShow(Msg) do
        Result := Integer(HintShow(HintInfo^));
    CM_HITTEST:
      with TCMHitTest(Msg) do
        Result := Integer(HitTest(XPos, YPos));
    CM_MOUSEENTER:
      MouseEnter(TControl(Msg.LParam));
    CM_MOUSELEAVE:
      MouseLeave(TControl(Msg.LParam));
    CM_VISIBLECHANGED:
      VisibleChanged;
    CM_ENABLEDCHANGED:
      EnabledChanged;
    // LCL doesn't send this message but left it in case
    CM_TEXTCHANGED:
      TextChanged;
    CM_FONTCHANGED:
      FontChanged;
    CM_COLORCHANGED:
      ColorChanged;
    CM_FOCUSCHANGED:
      FocusChanged(TWinControl(Msg.LParam));
    // LCL doesn't send this message but left it in case
    //CM_PARENTFONTCHANGED:
    //  ParentFontChanged;
    CM_PARENTCOLORCHANGED:
      ParentColorChanged;
    CM_PARENTSHOWHINTCHANGED:
      ParentShowHintChanged;
  else
    inherited WndProc(Msg);
  end;
end;

//============================================================================

end.

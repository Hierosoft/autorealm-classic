program AutoREALM;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, DrawLines, Primitives, ToolObject, SelectionTool,
  LineTool, MapObject, ReplacementOverlay, TextTool, Splash, GraphGrid,
  SelectFont, MatrixMath, About, Rotate, Scale, Flip, Skew, PolygonSides,
  TextSpecialties, Geometry, ChartGrid, MapSettings, SettingsDialog, Snap,
  DeleteView, CustomPrint, movesel, AlignDlg, CreateArray, SaveView, Main,
  AutoName, AutoNameGenerator, SymbolLib, SymbolFile, StreamUtil, NewGroup,
  DefineNewSymbol, Logging, OrderDlg, XMLUnit, HyperlinkProp, CustomHint,
  BitmapProperties, Bezier, CTManager, LocalizedStrings, frmMakeFrom3DUnit,
  ImageCheckListBox, PersistentForm, PoliteSpinEdit, PoliteComboBox, PoliteEdit;


{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'AutoREALM';
  Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


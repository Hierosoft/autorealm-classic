program AutoREALM;

uses
  Forms,
  DrawLines in 'DrawLines.pas',
  Primitives in 'Primitives.pas',
  ToolObject in 'ToolObject.pas',
  SelectionTool in 'SelectionTool.pas',
  LineTool in 'LineTool.pas',
  MapObject in 'MapObject.pas',
  ReplacementOverlay in 'ReplacementOverlay.pas' {ReplacementOverlayForm},
  TextTool in 'TextTool.pas',
  Splash in 'Splash.pas' {SplashForm},
  GraphGrid in 'GraphGrid.pas',
  SelectFont in 'SelectFont.pas' {ChooseFont},
  MatrixMath in 'MatrixMath.pas',
  About in 'About.pas' {AboutForm},
  Rotate in 'Rotate.pas' {RotateForm},
  Scale in 'Scale.pas' {ScaleForm},
  Flip in 'Flip.pas' {FlipForm},
  Skew in 'Skew.pas' {SkewForm},
  PolygonSides in 'PolygonSides.pas' {PolygonSidesForm},
  TextSpecialties in 'TextSpecialties.pas',
  Geometry in 'Geometry.pas',
  ChartGrid in 'ChartGrid.pas' {ChartGridDialog},
  MapSettings in 'MapSettings.pas' {MapSettingsDialog},
  SettingsDialog in 'SettingsDialog.pas' {Settings},
  Snap in 'Snap.pas',
  DeleteView in 'DeleteView.pas' {DeleteViewForm},
  CustomPrint in 'CustomPrint.pas' {CustomPrintDialog},
  movesel in 'movesel.pas' {MoveSelection},
  AlignDlg in 'AlignDlg.pas' {AlignDialog},
  CreateArray in 'CreateArray.pas' {ArrayForm},
  SaveView in 'SaveView.pas' {SaveViewForm},
  Main in 'MAIN.PAS' {MainForm},
  AutoName in 'AutoName.pas' {AutoNameDialog},
  AutoNameGenerator in 'AutoNameGenerator.pas',
  SymbolLib in 'SymbolLib.pas' {SymbolLibraryForm},
  SymbolFile in 'SymbolFile.pas',
  StreamUtil in 'StreamUtil.pas',
  NewGroup in 'NewGroup.pas' {AddSymbolGroup},
  DefineNewSymbol in 'DefineNewSymbol.pas' {NewSymbol},
  Logging in 'Logging.pas',
  OrderDlg in 'OrderDlg.pas' {OrderDialog},
  XMLUnit in 'XMLUnit.pas',
  HyperlinkProp in 'HyperlinkProp.pas' {HyperlinkProperties},
  CustomHint in 'CustomHint.pas',
  BitmapProperties in 'BitmapProperties.pas' {BitmapPropertyDlg},
  Bezier in 'bezier.pas',
  CTManager in 'CTManager.pas' {frmCTManager},
  LocalizedStrings in 'LocalizedStrings.pas',
  frmMakeFrom3DUnit in 'frmMakeFrom3DUnit.pas' {frmMakeFrom3D};

{$R *.RES}

//
// NOTE: This has lots of comments to explain the purpose of autoloading
//       (or not) each form has.  IF YOU CHANGE THE PROJECT OPTIONS FOR
//       ANY FORMS, THIS SECTION WILL BE OVERWRITTEN BY DELPHI!
//       Be forewarned!
//
begin
  try
    Application.Title := 'AutoREALM';

    // We show this right away, so we need it.  We will free it when
    // we're done bringing up the app.
    //
    // NOTE: We're doing something kinda tricky here--the SplashForm
    // is the first created form, and therefore, the Application object
    // would normally treat it as the MainForm.  However, we destroy
    // SplashForm inside the MainForm FormCreate, which transfers the
    // MainForm responsibility to its rightful place, MainForm.
    // Unfortunately, you can't really move the splash form destruction
    // later into the process (i.e. after we're completely loaded), because
    // then it would keep it's MainForm responsibility.
    Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TfrmMakeFrom3D, frmMakeFrom3D);
  SplashForm.Repaint;

    // We load system settings from the registry by loading this dialog
    // (it is a decendant of my TPersistentForm class).
    Application.CreateForm(TSettings, Settings);

    // This form takes part in the toolbar save/restore, and so
    // needs to be available all the time.
    Application.CreateForm(TChooseFont, ChooseFont);

    // We technically don't need this one, but we refer to lots of
    // parameters set in the dialog after we've okayed it--it's easier
    // as written to just accept that and create it normally on loadup.
    Application.CreateForm(TChartGridDialog, ChartGridDialog);

    // This is the main form; we sure as heck need this one!
    // Quite a few things in DrawPrimitives rely on this form being
    // up first (not a clean design, I admit).  Preferably, DrawPrimitives
    // and MapObject should be completely divorced from MainForm: the
    // things that prevent that are 1) easy access to the fill/pattern button
    // controls for creating bitmaps, and 2) default settings such
    // as the current background color, current overlay, etc.
    // The upshot of this is that the SymbolLibrary *must* be loaded
    // after this, although it means our splash screen has already
    // gone away.
    Application.CreateForm(TMainForm, MainForm);

    // Read the above (peverted) logic for why we do this:
    // we want the splash screen up, but we've already destroyed
    // it.  Put it up again for the duration of the Symbol library
    // load.
    Application.CreateForm(TSplashForm, SplashForm);
    // Force it to completely display
    SplashForm.Repaint;

    // This dialog is required to save settings that accompany the
    // map--we write to it dynamically, so we create it on startup.
    Application.CreateForm(TMapSettingsDialog, MapSettingsDialog);

    // Create the symbol library: part of loading this form also builds
    // our "favorites" toolbar.
    Application.CreateForm(TSymbolLibraryForm, SymbolLibraryForm);

    // Like ChartGrid, our CustomPrint code wasn't really written
    // cleanly, and so accesses the dialog outside of the normal
    // lifespan.  Make our lives easy, and create it.
    Application.CreateForm(TCustomPrintDialog, CustomPrintDialog);

    // We keep this one around too, because it is very convienent
    // while creating a map to keep popping names from AutoName to
    // the map--if we recreated it each time, we'd lose the context,
    // and so they'd have to reset their rul file (and thus, regenerate
    // and lose their names).
    Application.CreateForm(TAutoNameDialog, AutoNameDialog);

    // Keep this form so we don't have to keep resetting the properties
    // of multiple hyperlinks we add.
    Application.CreateForm(THyperlinkProperties, HyperlinkProperties);

    // Likewise, retain settings in the Bitmap Property dialog
    Application.CreateForm(TBitmapPropertyDlg, BitmapPropertyDlg);

    // Let's create this form now, so that the user's last
    // rotation settings (including number of divisions) are remembered.
    // This is useful for when a bunch of items need to be rotated by the
    // same amount. (JD 10-10-02)
    Application.CreateForm(TRotateForm, RotateForm);

    // We want this created at the start, because it must keep the color
    // translation profiles in memory and remember the active one
    Application.CreateForm(TfrmCTManager, frmCTManager);

{ All the forms here commented out have been removed to
  help preserve resources while AutoREALM is running.
  Delphi automatically adds things to this list when you add
  forms, so if you want to dynamically create them, comment
  them back out again.  Note that you also need to call Form.Free
  when you're done with it, and that (unlike normal Delphi forms)
  the form will not preserve any of its settings from invocation
  to invocation. }

//  *  Application.CreateForm(TReplacementOverlayForm, ReplacementOverlayForm);
//  *  Application.CreateForm(TAboutForm, AboutForm);
//  *  Application.CreateForm(TScaleForm, ScaleForm);
//  *  Application.CreateForm(TFlipForm, FlipForm);
//  *  Application.CreateForm(TSkewForm, SkewForm);
//  *  Application.CreateForm(TPolygonSidesForm, PolygonSidesForm);
//  *  Application.CreateForm(TDeleteViewForm, DeleteViewForm);
//  *  Application.CreateForm(TMoveSelection, MoveSelection);
//  *  Application.CreateForm(TAlignDialog, AlignDialog);
//  *  Application.CreateForm(TArrayForm, ArrayForm);
//  *  Application.CreateForm(TSaveViewForm, SaveViewForm);
//  *  Application.CreateForm(TAddSymbolGroup, AddSymbolGroup);
//  *  Application.CreateForm(TNewSymbol, NewSymbol);

    // Don't need this anymore...
    SplashForm.Free;

    Application.Run;
  except
    raise;
  end;
end.

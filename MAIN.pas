{+------------------------------------------------------------------------+
 | AutoREALM.   Copyright (c) 2000, Andrew J. Gryc.                       |
 |                                                                        |
 | This program is free software; you can redistribute it and/or modify   |
 | it under the terms of the GNU General Public License as published by   |
 | the Free Software Foundation; either version 2 of the License, or (at  |
 | your option) any later version.                                        |
 |                                                                        |
 | This program is distributed in the hope that it will be useful, but    |
 | WITHOUT ANY WARRANTY; without even the implied warranty of             |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      |
 | General Public License for more details.                               |
 |                                                                        |
 | For a copy of the GNU General Public License, write to the Free        |
 | Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA     |
 | 02111-1307, USA.                                                       |
 +------------------------------------------------------------------------+}
unit Main;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ToolWin, checklst, Spin,
  DrawLines, MapObject, TextTool, ToolObject, LineTool,
  SelectionTool, Primitives, GraphGrid, SelectFont, Rotate, Scale, Flip,
  PoliteSpinEdit, PoliteComboBox, PoliteEdit, ColorButton, Geometry, TextSpecialties,
  ImgList, SettingsDialog, ImageCheckListBox, Registry, Printers, SymbolLib,
  SymbolFile, ShellAPI, IniFiles, DeleteView, AlignDlg, SaveView, AutoName,
  PaintWindow, ExtDlgs, jpeg, OrderDlg, ActnList, TB2Dock, TB2Item,
  TB2Toolbar, TB2ExtItems, TB2MRU, TB2ToolWindow, XDOM_2_3;
const
  MapFileId = $52747541; // AutR (in little-endian format)
  UnitsPerGridTick = 0.01;
  maximum_overlays = 255;

type
  TMainForm = class(TForm)
    ToolImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PopupMenu: TPopupMenu;
    Cut1: TMenuItem;
    PopupCopyItem: TMenuItem;
    PopupPasteItem: TMenuItem;
    PopupDeleteItem: TMenuItem;
    N7: TMenuItem;
    PopupSelectionToolItem: TMenuItem;
    SelectNone1: TMenuItem;
    OverlayImages: TImageList;
    PrinterSetupDialog: TPrinterSetupDialog;
    FillPatternList: TImageList;
    PushPinImages: TImageList;
    N9: TMenuItem;
    AddPushPin: TMenuItem;
    SingleIcon1: TMenuItem;
    ChangeUnitsSubMenu: TMenuItem;
    ShapePopupMenu: TPopupMenu;
    OpenFigureMenuItem: TMenuItem;
    ClosedFigureMenuItem: TMenuItem;
    N10: TMenuItem;
    SuppressMenu: TMenuItem;
    PanTimer: TTimer;
    OverlayColors: TImageList;
    FavoritesImageList: TImageList;
    PushpinPopupMenu: TPopupMenu;
    PushpinRenameMenu: TMenuItem;
    PushPinWaypointMenu: TMenuItem;
    PushpinExportMenu: TMenuItem;
    PushpinImportMenu: TMenuItem;
    N20: TMenuItem;
    MakeWaypointsVisibleMenu: TMenuItem;
    PushPinHistoryClearMenu: TMenuItem;
    N21: TMenuItem;
    PushPointRightClickName: TMenuItem;
    N22: TMenuItem;
    PushPinShowNumberMenu: TMenuItem;
    PushPinShowNoteMenu: TMenuItem;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    RefreshOneShot: TTimer;
    dTop: TTBDock;
    alTools: TActionList;
    aSelection: TAction;
    aOpen: TAction;
    aZoomIn: TAction;
    aPan: TAction;
    aRuler: TAction;
    aFreeHandRuler: TAction;
    aSave: TAction;
    aRepaint: TAction;
    aUndo: TAction;
    aRedo: TAction;
    aReadOnly: TAction;
    aRectangle: TAction;
    aCircle: TAction;
    aPolygon: TAction;
    aArc: TAction;
    aChartGrid: TAction;
    aTextOut: TAction;
    aCurvedText: TAction;
    aHyperLink: TAction;
    aGroup: TAction;
    aUnGroup: TAction;
    aDecompose: TAction;
    aNormalFreeHand: TAction;
    aNormalLine: TAction;
    aNormalPolyline: TAction;
    aNormalCurve: TAction;
    aNormalPolyCurve: TAction;
    aFractalFreeHand: TAction;
    aFractalPolyline: TAction;
    aFractalCurve: TAction;
    aFractalLine: TAction;
    aFractalPolyCurve: TAction;
    tbGeneral: TTBToolbar;
    TBItem1: TTBItem;
    TBItem2: TTBItem;
    TBItem3: TTBItem;
    TBItem4: TTBItem;
    TBItem5: TTBItem;
    TBItem6: TTBItem;
    TBItem7: TTBItem;
    TBItem8: TTBItem;
    TBItem10: TTBItem;
    tbiSelection: TTBItem;
    tbDrawing: TTBToolbar;
    TBItem12: TTBItem;
    TBItem13: TTBItem;
    TBItem14: TTBItem;
    TBItem15: TTBItem;
    TBItem16: TTBItem;
    TBItem17: TTBItem;
    TBItem18: TTBItem;
    TBItem19: TTBItem;
    TBItem20: TTBItem;
    TBItem21: TTBItem;
    TBItem22: TTBItem;
    dLeft: TTBDock;
    dRight: TTBDock;
    dBottom: TTBDock;
    tbEditing: TTBToolbar;
    tbTransform: TTBToolbar;
    tbShape: TTBToolbar;
    tbFractalDrawing: TTBToolbar;
    TBItem23: TTBItem;
    TBItem24: TTBItem;
    TBItem25: TTBItem;
    TBItem26: TTBItem;
    TBItem27: TTBItem;
    TBItem28: TTBItem;
    TBItem29: TTBItem;
    TBItem30: TTBItem;
    TBItem31: TTBItem;
    TBItem32: TTBItem;
    aAlign: TAction;
    aFlip: TAction;
    aSkew: TAction;
    aRotate: TAction;
    aScale: TAction;
    aMove: TAction;
    aRotate90: TAction;
    aRotate45: TAction;
    aArray: TAction;
    aOrder: TAction;
    TBItem33: TTBItem;
    TBItem34: TTBItem;
    TBItem35: TTBItem;
    TBItem36: TTBItem;
    TBItem37: TTBItem;
    TBItem38: TTBItem;
    TBItem39: TTBItem;
    TBItem40: TTBItem;
    TBItem41: TTBItem;
    TBItem42: TTBItem;
    TBSeparatorItem1: TTBSeparatorItem;
    aSendToBack: TAction;
    aSendBackward: TAction;
    aBringForward: TAction;
    aBringToFront: TAction;
    aPaste: TAction;
    aCopy: TAction;
    aCut: TAction;
    aDelete: TAction;
    aScalpel: TAction;
    aGlue: TAction;
    TBItem43: TTBItem;
    TBItem44: TTBItem;
    TBItem45: TTBItem;
    TBItem46: TTBItem;
    TBItem47: TTBItem;
    TBItem48: TTBItem;
    TBItem49: TTBItem;
    TBItem50: TTBItem;
    TBItem51: TTBItem;
    TBItem52: TTBItem;
    TBSeparatorItem2: TTBSeparatorItem;
    TBSeparatorItem3: TTBSeparatorItem;
    TBSeparatorItem4: TTBSeparatorItem;
    TBSeparatorItem5: TTBSeparatorItem;
    TBSeparatorItem6: TTBSeparatorItem;
    TBSeparatorItem7: TTBSeparatorItem;
    TBSeparatorItem8: TTBSeparatorItem;
    tbColor: TTBToolbar;
    tbFractalSettings: TTBToolbar;
    TBControlItem1: TTBControlItem;
    MainColor: TColorButton;
    TBControlItem2: TTBControlItem;
    FillColor: TColorButton;
    TBControlItem3: TTBControlItem;
    FillPattern: TPatternButton;
    TBControlItem4: TTBControlItem;
    OutlineColor: TColorButton;
    TBControlItem5: TTBControlItem;
    BackgroundColor: TColorButton;
    TBControlItem6: TTBControlItem;
    GridColor: TColorButton;
    TBControlItem9: TTBControlItem;
    FractalSettingsGroupBox: TGroupBox;
    AcornImage: TImage;
    RoughnessImage: TImage;
    RoughnessTrackBar: TTrackBar;
    SeedSpin: TPoliteSpinEdit;
    tbIconSettings: TTBToolbar;
    TBControlItem10: TTBControlItem;
    IconSettingsGroupBox: TGroupBox;
    SizeImage: TImage;
    DensityImage: TImage;
    IconSizeBar: TTrackBar;
    DensityBar: TTrackBar;
    tbGraphPaper: TTBToolbar;
    TBControlItem11: TTBControlItem;
    GridGroupBox: TGroupBox;
    NoGraphBtn: TSpeedButton;
    TriangleGraphBtn: TSpeedButton;
    SquareGraphBtn: TSpeedButton;
    HexGraphBtn: TSpeedButton;
    BoldGridImage: TImage;
    GridSizeImage: TImage;
    RotatedHexGraphBtn: TSpeedButton;
    DiamondGraphBtn: TSpeedButton;
    HalfDiamondGraphBtn: TSpeedButton;
    PolarGraphBtn: TSpeedButton;
    GridLabel: TLabel;
    GridSizeBar: TTrackBar;
    BoldUnitCount: TPoliteSpinEdit;
    PrimaryGridStyle: TPoliteComboBox;
    SecondaryGridStyle: TPoliteComboBox;
    edtGridSize: TEdit;
    tbLineStyle: TTBToolbar;
    TBControlItem13: TTBControlItem;
    BeginLineStyle: TPoliteComboBox;
    TBControlItem14: TTBControlItem;
    LineStyleComboBox: TPoliteComboBox;
    TBControlItem15: TTBControlItem;
    EndLineStyle: TPoliteComboBox;
    aPlaceOne: TAction;
    aPlaceSquare: TAction;
    aPlaceDiamond: TAction;
    aPlaceRandom: TAction;
    aDefineSymbol: TAction;
    aSymbolLibrary: TAction;
    tbIconPlacement: TTBToolbar;
    TBItem53: TTBItem;
    TBItem54: TTBItem;
    TBItem55: TTBItem;
    TBItem56: TTBItem;
    TBItem57: TTBItem;
    TBItem58: TTBItem;
    tbSettings: TTBToolbar;
    tbSize: TTBToolbar;
    tbZoom: TTBToolbar;
    aDisplayOverlay: TAction;
    aDisplayGrid: TAction;
    aGravitySnap: TAction;
    aGridSnap: TAction;
    aGravitySnapAlong: TAction;
    aRotateSnap: TAction;
    TBItem11: TTBItem;
    TBItem59: TTBItem;
    TBItem60: TTBItem;
    TBItem61: TTBItem;
    TBItem62: TTBItem;
    TBItem63: TTBItem;
    tbRecentMainMenu: TTBToolbar;
    FileMenuItem: TTBSubmenuItem;
    FileNewMenuItem: TTBItem;
    OpenMenuItem: TTBItem;
    SaveMenuItem: TTBItem;
    SaveAsMenuItem: TTBItem;
    InsertMenuItem: TTBItem;
    N3: TTBSeparatorItem;
    Print1: TTBItem;
    PrintSetup1: TTBItem;
    N1: TTBSeparatorItem;
    MapSettings: TTBItem;
    N4: TTBSeparatorItem;
    ExitMenuItem: TTBItem;
    N2: TTBSeparatorItem;
    SymbolEditFileMenu: TTBSubmenuItem;
    SaveSymbolChanges1: TTBItem;
    cmdCancelEdit: TTBItem;
    EditMenuItem: TTBSubmenuItem;
    UndoMenuItem: TTBItem;
    RedoMenuItem: TTBItem;
    N15: TTBSeparatorItem;
    CutMenuItem: TTBItem;
    CopyMenuItem: TTBItem;
    PasteMenuItem: TTBItem;
    DeleteMenuItem: TTBItem;
    N5: TTBSeparatorItem;
    SelectAll1: TTBItem;
    SelectNoneMenuItem: TTBItem;
    N23: TTBSeparatorItem;
    UseArrowMenuItem: TTBItem;
    UsePanningToolMenuItem: TTBItem;
    ViewMenu: TTBSubmenuItem;
    ZoomInMenuItem: TTBItem;
    ZoomOutMenuItem: TTBItem;
    ShowAllObjectsMenuItem: TTBItem;
    ReadOnlyMenu: TTBItem;
    N11: TTBSeparatorItem;
    SaveCurrentView: TTBItem;
    DeleteView: TTBItem;
    N8: TTBSeparatorItem;
    Transform1: TTBSubmenuItem;
    MoveMenuItem: TTBItem;
    ScaleMenuItem: TTBItem;
    RotateMenuItem: TTBItem;
    SkewMenuItem: TTBItem;
    FlipMenuItem: TTBItem;
    N16: TTBSeparatorItem;
    Align1: TTBItem;
    ArrayMenuItem: TTBItem;
    N6: TTBSeparatorItem;
    SendtoBackMenuItem: TTBItem;
    BringtoFrontMenuItem: TTBItem;
    menuOrder: TTBItem;
    N17: TTBSeparatorItem;
    CloseFigureMenuItem: TTBItem;
    ReverseLineDirection1: TTBItem;
    Toolbar15: TTBSubmenuItem;
    Restoredefaulttoolbars: TTBItem;
    Fractal1: TTBSubmenuItem;
    IconToolbarItem: TTBSubmenuItem;
    TextToolbarItem: TTBItem;
    OptionsMenuItem: TTBSubmenuItem;
    SettingsMenuItem: TTBItem;
    Autoname1: TTBItem;
    N13: TTBSeparatorItem;
    SnaptoGridMenuItem: TTBItem;
    GravitySnapMenuItem: TTBItem;
    HelpMenuItem: TTBSubmenuItem;
    ContentsMenuItem: TTBItem;
    GettingStarted1: TTBItem;
    Search1: TTBItem;
    N18: TTBSeparatorItem;
    HowDoIMenuItem: TTBItem;
    ReadMeMenuItem: TTBItem;
    N14: TTBSeparatorItem;
    AboutMenuItem: TTBItem;
    TBVisibilityToggleItem1: TTBVisibilityToggleItem;
    TBSeparatorItem9: TTBSeparatorItem;
    TBVisibilityToggleItem2: TTBVisibilityToggleItem;
    TBVisibilityToggleItem3: TTBVisibilityToggleItem;
    TBVisibilityToggleItem4: TTBVisibilityToggleItem;
    TBVisibilityToggleItem5: TTBVisibilityToggleItem;
    TBVisibilityToggleItem6: TTBVisibilityToggleItem;
    TBVisibilityToggleItem7: TTBVisibilityToggleItem;
    TBVisibilityToggleItem8: TTBVisibilityToggleItem;
    TBVisibilityToggleItem9: TTBVisibilityToggleItem;
    TBVisibilityToggleItem10: TTBVisibilityToggleItem;
    TBVisibilityToggleItem11: TTBVisibilityToggleItem;
    TBVisibilityToggleItem12: TTBVisibilityToggleItem;
    TBSeparatorItem10: TTBSeparatorItem;
    TBSubmenuItem1: TTBSubmenuItem;
    TBMRUListItem1: TTBMRUListItem;
    MRUList: TTBMRUList;
    twPushPin: TTBToolWindow;
    PushPinList: TImageCheckListBox;
    twOverlay: TTBToolWindow;
    ActiveOverlay: TPoliteComboBox;
    OverlayList: TImageCheckListBox;
    twIconPages: TTBToolWindow;
    SymbolGroupList: TListBox;
    splIcons: TSplitter;
    SymbolIconList: TListView;
    TBVisibilityToggleItem13: TTBVisibilityToggleItem;
    TBVisibilityToggleItem14: TTBVisibilityToggleItem;
    TBVisibilityToggleItem15: TTBVisibilityToggleItem;
    TBVisibilityToggleItem16: TTBVisibilityToggleItem;
    TBVisibilityToggleItem17: TTBVisibilityToggleItem;
    TBControlItem7: TTBControlItem;
    ZoomComboBox: TPoliteComboBox;
    TBItem9: TTBItem;
    TBControlItem8: TTBControlItem;
    Panel1: TPanel;
    lblSizeX: TLabel;
    lblSizeY: TLabel;
    edtXSize: TEdit;
    edtYSize: TEdit;
    cbKeepAspect: TCheckBox;
    aSelectNone: TAction;
    aShowCTManager: TAction;
    TBItem64: TTBItem;
    aTranslateColor: TAction;
    aInverseTranslateColor: TAction;
    TBItem65: TTBItem;
    TBItem66: TTBItem;
    TBSeparatorItem11: TTBSeparatorItem;
    WizardsMenuItem: TTBSubmenuItem;
    mnuColumnsAndRooftops: TTBItem;
    TBSeparatorItem12: TTBSeparatorItem;
    TBSubmenuItem2: TTBSubmenuItem;
    TBItem67: TTBItem;
    TBItem68: TTBItem;
    TBItem69: TTBItem;
    aSetToNormalLines: TAction;
    aSetToFractalLines: TAction;
    aToggleNormalFractalLines: TAction;
    FlipLineStyle: TTBItem;
    aFlipLineStyle: TAction;
    DOMImpl: TDomImplementation;
    XmlToDomParser1: TXmlToDomParser;
    DomToXmlParser1: TDomToXmlParser;
    mnuTransformGridBringforward: TTBItem;
    mnuTransformGridSendbackward: TTBItem;
    mnuTransformPosition: TTBSubmenuItem;
    mnuTranformPositionSendbackward: TTBItem;
    mnuTranformPositionBringforward: TTBItem;
    TBSeparatorItem13: TTBSeparatorItem;
    mnuViewZoom: TTBSubmenuItem;
    mnuViewZoom100: TTBItem;
    mnuViewZoom050: TTBItem;
    mnuViewZoom500: TTBItem;
    mnuViewZoom400: TTBItem;
    mnuViewZoom300: TTBItem;
    mnuViewZoom200: TTBItem;
    mnuViewZoom150: TTBItem;
    mnuViewZoom075: TTBItem;
    mnuViewZoom025: TTBItem;
    mnuViewZoom010: TTBItem;
    mnuViewZoomFit: TTBItem;
    mnuViewZoomCustom: TTBItem;
    mnuDrawing: TTBSubmenuItem;
    mnuDrawingShapes: TTBSubmenuItem;
    mnuDrawingLines: TTBSubmenuItem;
    mnuDrawingFractal: TTBSubmenuItem;
    mnuDrawingText: TTBSubmenuItem;
    mnuDrawingFractalFreehand: TTBItem;
    mnuDrawingFractalLine: TTBItem;
    mnuDrawingFractalPolyline: TTBItem;
    mnuDrawingFractalCurve: TTBItem;
    mnuDrawingFractalPolycurve: TTBItem;
    mnuDrawingLinesPolycurve: TTBItem;
    mnuDrawingLinesCurve: TTBItem;
    mnuDrawingLinesPolyline: TTBItem;
    mnuDrawingLinesLine: TTBItem;
    mnuDrawingLinesFreehand: TTBItem;
    mnuDrawingShapesRosette: TTBItem;
    mnuDrawingShapesArc: TTBItem;
    mnuDrawingShapesPolygon: TTBItem;
    mnuDrawingShapesCircle: TTBItem;
    mnuDrawingShapesRectangle: TTBItem;
    mnuDrawingTextHyperlink: TTBItem;
    mnuDrawingTextCurvedText: TTBItem;
    mnuDrawingTextText: TTBItem;
    mnuDrawingSymbols: TTBSubmenuItem;
    mnuDrawingSymbolsSingle: TTBItem;
    mnuDrawingSymbolsRandom: TTBItem;
    mnuDrawingSymbolsDiamond: TTBItem;
    mnuDrawingSymbolsSquare: TTBItem;
    mnuDefineNewSymbol: TTBItem;
    N19: TTBSeparatorItem;
    SymbolLibrary1: TTBItem;
    mnuDrawingGraph: TTBSubmenuItem;
    TBSeparatorItem14: TTBSeparatorItem;
    mnuDrawingGraphPolar: TTBItem;
    mnuDrawingGraphHalfDiamond: TTBItem;
    mnuDrawingGraphDiamond: TTBItem;
    mnuDrawingGraphTriangle: TTBItem;
    mnuDrawingGraphHexHorz: TTBItem;
    mnuDrawingGraphHexVert: TTBItem;
    mnuDrawingGraphSquare: TTBItem;
    mnuDrawingGraphBlank: TTBItem;
    TBSeparatorItem15: TTBSeparatorItem;
    mnuDrawingSymbolsIconSpread: TTBItem;
    mnuDrawingSymbolsIconSize: TTBItem;
    TBSeparatorItem16: TTBSeparatorItem;
    mnuDrawingFractalRoughness: TTBItem;
    mnuDrawingFractalSeed: TTBItem;
    TBSeparatorItem17: TTBSeparatorItem;
    mnuDrawingGraphGridSize: TTBItem;
    mnuDrawingGraphSecGridSize: TTBItem;
    edtLineThickness: TTBEditItem;
    procedure UpdateGridLabel;
    Function  GetGridPositionFromLabel(LabelValue: Double): Double;
    procedure EnableSymbolEditFunctions(enabled: boolean);
    procedure WhenIdle(Sender: TObject; var Done: Boolean);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure LineStyleComboBoxDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure ActiveOverlayKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OverlayListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure AdhereScrollBars;
    procedure SetScrollBarPositions;
    procedure LoadZoomPercentageBox;

    function FindOverlay(s: string): boolean;
    procedure AddOverlay(s: string);
    procedure DeleteOverlay(n: integer);
    procedure SetDefaultOverlays();
    procedure SwitchToolDispatcher(newtoolindex: integer; newtool: TToolHandler);
    procedure IconBtnClick(Sender: TObject);
    procedure FillColorBtnClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SeedSpinChange(Sender: TObject);
    procedure RoughnessTrackBarChange(Sender: TObject);
    procedure LineStyleComboBoxChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OverlayListClickCheck(Sender: TObject);
    procedure ActiveOverlayChange(Sender: TObject);
    procedure OverlayListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RefreshExtent;
    procedure FormResize(Sender: TObject);
    procedure NoGraphBtnClick(Sender: TObject);
    procedure SquareGraphBtnClick(Sender: TObject);
    procedure HexGraphBtnClick(Sender: TObject);
    procedure TriangleGraphBtnClick(Sender: TObject);
    procedure DiamondGraphBtnClick(Sender: TObject);
    procedure GridSizeBarChange(Sender: TObject);
    procedure SaveMap(filename: string);
    procedure LoadMap(filename: string; tempfile: boolean = false);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure FileNewMenuItemClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure ZoomInMenuItemClick(Sender: TObject);
    procedure ZoomOutMenuItemClick(Sender: TObject);
    procedure ShowAllMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure BackgroundColorChange(Sender: TObject);
    procedure GridColorChange(Sender: TObject);
    function ModifiedHasBeenSaved: boolean;
    procedure BoldUnitCountChange(Sender: TObject);
    procedure IconSizeBarChange(Sender: TObject);
    procedure FontToolbar1Click(Sender: TObject);
    procedure ChartGridBtnClick(Sender: TObject);
    procedure MapSettingsClick(Sender: TObject);
    procedure TextToolbarItemClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure UpdateOverlayImages(os: OverlaySet);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ParseParameters;
    procedure PrintSetup1Click(Sender: TObject);
    procedure PrintMap(usedefaults: boolean);
    procedure Print1Click(Sender: TObject);
    procedure BeginLineStyleChange(Sender: TObject);
    procedure EndLineStyleChange(Sender: TObject);
    procedure FillPatternChange(Sender: TObject);
    procedure MeasurementMenuItemClick(Sender: TObject);
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure WMQueryNewPalette(var msg: TMessage); message WM_QUERYNEWPALETTE;
    procedure WMPaletteChanged(var msg: TMessage); message WM_PALETTECHANGED;
    procedure WMNCLButtonDown(var msg: TMessage); message WM_NCLBUTTONDOWN;
    procedure WMRenderFormat(var msg: TMessage); message WM_RENDERFORMAT;
    procedure WMRenderAllFormats(var msg: TMessage); message WM_RENDERALLFORMATS;
    function AlrightToSaveTo(filename: string): boolean;
    procedure ClearPushPins;
    procedure PushPinMenuItemClick(Sender: TObject);
    procedure PushPinListClickCheck(Sender: TObject);
    procedure RefreshPushPinMenu;
    procedure RefreshPushPins;
    procedure RefreshPushPinDistances(x, y: integer);
    procedure PushPinListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure OpenFigureMenuItemClick(Sender: TObject);
    procedure ClosedFigureMenuItemClick(Sender: TObject);
    procedure SuppressMenuClick(Sender: TObject);
    procedure PanTimerTimer(Sender: TObject);
    function GetVisibleRect: TRect;
    procedure RestoreToolbarPositions;
    procedure SaveToolbarPositions;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildViewList;
    procedure ViewPointClick(Sender: TObject);
    procedure SaveCurrentViewClick(Sender: TObject);
    procedure DeleteViewClick(Sender: TObject);
    procedure RestoreDefaultToolbarsClick(Sender: TObject);
    procedure ContentsMenuItemClick(Sender: TObject);
    procedure GettingStarted1Click(Sender: TObject);
    procedure Search1Click(Sender: TObject);
    procedure OverlayListDblClick(Sender: TObject);
    procedure OutlineColorClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ArrayMenuItemClick(Sender: TObject);
    procedure CloseFigureMenuItemClick(Sender: TObject);
    procedure ReverseLineDirection1Click(Sender: TObject);
    procedure HowDoIMenuItemClick(Sender: TObject);
    procedure ReadMeMenuItemClick(Sender: TObject);
    procedure Autoname1Click(Sender: TObject);
    procedure InsertMenuItemClick(Sender: TObject);
    procedure DoInvalidate;
    procedure HideCrosshair;
    procedure AppDeactivate(Sender: TObject);
    procedure DisableCrosshair(Sender: TObject);

    procedure InvokeSymbolEditor(name: string; sym: Symbol);
    procedure UninvokeSymbolEditor;
    procedure CancelSymbolChanges;
    procedure SaveSymbolChanges;

    procedure cmdCancelEditClick(Sender: TObject);
    procedure RotatedHexGraphBtnClick(Sender: TObject);
    procedure HalfDiamondGraphBtnClick(Sender: TObject);
    procedure PolarGraphBtnClick(Sender: TObject);
    procedure SecondaryGridStyleChange(Sender: TObject);
    procedure PrimaryGridStyleChange(Sender: TObject);
    procedure GridStyleDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure PushPinListClick(Sender: TObject);
    procedure PushPinWaypointMenuClick(Sender: TObject);
    procedure PushPinHistoryClearMenuClick(Sender: TObject);
    procedure PushpinRenameMenuClick(Sender: TObject);
    procedure PushpinExportMenuClick(Sender: TObject);
    procedure PushpinImportMenuClick(Sender: TObject);
    procedure MakeWaypointsVisibleMenuClick(Sender: TObject);
    procedure InvalidatePushPin(idx:integer);
    procedure InvalidatePushPins;
    procedure PushPinShowNumberMenuClick(Sender: TObject);
    procedure PushPinShowNoteMenuClick(Sender: TObject);
    procedure ReadOnlyMenuClick(Sender: TObject);
    procedure SetReadOnlyMode(b:boolean);
    function ExecuteHyperlink(hypertext:string; hyperflags:THyperlinkFlags):string;
    procedure FlashReadOnlyProhibits;
    procedure SetMapGrid(name:string; grid:TGraphGrid);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SymbolGroupListClick(Sender: TObject);
    procedure IconCoolBarResize(Sender: TObject);
    procedure UseArrowMenuItemClick(Sender: TObject);
    procedure UsePanningToolMenuItemClick(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ZoomComboBoxClick(Sender: TObject);
    procedure ZoomComboBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HandleGridSizeChange;
    procedure edtGridSizeExit(Sender: TObject);
    procedure edtGridSizeKeyPress(Sender: TObject; var Key: Char);
    Procedure SetSelectionXSize(Sender: TObject);
    Procedure SetSelectionYSize(Sender: TObject);
    procedure edtXSizeKeyPress(Sender: TObject; var Key: Char);
    procedure edtYSizeKeyPress(Sender: TObject; var Key: Char);
    Function  GetSelectionToolHandler: TSelectionToolHandler;
    procedure FormConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
    procedure RefreshOneShotTimer(Sender: TObject);
    procedure aSelectionExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aZoomInExecute(Sender: TObject);
    procedure aPanExecute(Sender: TObject);
    procedure aRulerExecute(Sender: TObject);
    procedure aFreeHandRulerExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aRepaintExecute(Sender: TObject);
    procedure aUndoExecute(Sender: TObject);
    procedure aRedoExecute(Sender: TObject);
    procedure aReadOnlyExecute(Sender: TObject);
    procedure aRectangleExecute(Sender: TObject);
    procedure aCircleExecute(Sender: TObject);
    procedure aPolygonExecute(Sender: TObject);
    procedure aArcExecute(Sender: TObject);
    procedure aChartGridExecute(Sender: TObject);
    procedure aTextOutExecute(Sender: TObject);
    procedure aCurvedTextExecute(Sender: TObject);
    procedure aHyperLinkExecute(Sender: TObject);
    procedure aGroupExecute(Sender: TObject);
    procedure aUnGroupExecute(Sender: TObject);
    procedure aDecomposeExecute(Sender: TObject);
    procedure aNormalFreeHandExecute(Sender: TObject);
    procedure aNormalLineExecute(Sender: TObject);
    procedure aNormalPolylineExecute(Sender: TObject);
    procedure aNormalCurveExecute(Sender: TObject);
    procedure aNormalPolyCurveExecute(Sender: TObject);
    procedure aFractalFreeHandExecute(Sender: TObject);
    procedure aFractalPolylineExecute(Sender: TObject);
    procedure aFractalCurveExecute(Sender: TObject);
    procedure aFractalLineExecute(Sender: TObject);
    procedure aFractalPolyCurveExecute(Sender: TObject);
    procedure aAlignExecute(Sender: TObject);
    procedure aFlipExecute(Sender: TObject);
    procedure aSkewExecute(Sender: TObject);
    procedure aRotateExecute(Sender: TObject);
    procedure aScaleExecute(Sender: TObject);
    procedure aMoveExecute(Sender: TObject);
    procedure aRotate90Execute(Sender: TObject);
    procedure aRotate45Execute(Sender: TObject);
    procedure aArrayExecute(Sender: TObject);
    procedure aOrderExecute(Sender: TObject);
    procedure aSendToBackExecute(Sender: TObject);
    procedure aSendBackwardExecute(Sender: TObject);
    procedure aBringForwardExecute(Sender: TObject);
    procedure aBringToFrontExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aScalpelExecute(Sender: TObject);
    procedure aGlueExecute(Sender: TObject);
    procedure aPlaceOneExecute(Sender: TObject);
    procedure aPlaceSquareExecute(Sender: TObject);
    procedure aPlaceDiamondExecute(Sender: TObject);
    procedure aPlaceRandomExecute(Sender: TObject);
    procedure aDefineSymbolExecute(Sender: TObject);
    procedure aSymbolLibraryExecute(Sender: TObject);
    procedure aDisplayOverlayExecute(Sender: TObject);
    procedure aDisplayGridExecute(Sender: TObject);
    procedure aGravitySnapExecute(Sender: TObject);
    procedure aGridSnapExecute(Sender: TObject);
    procedure aGravitySnapAlongExecute(Sender: TObject);
    procedure aRotateSnapExecute(Sender: TObject);
    procedure MRUListClick(Sender: TObject; const Filename: String);
    procedure aSelectNoneExecute(Sender: TObject);
    procedure aShowCTManagerExecute(Sender: TObject);
    procedure aTranslateColorExecute(Sender: TObject);
    procedure aInverseTranslateColorExecute(Sender: TObject);
    procedure mnuColumnsAndRooftopsClick(Sender: TObject);
    procedure aSetToNormalLinesExecute(Sender: TObject);
    procedure aSetToFractalLinesExecute(Sender: TObject);
    procedure aToggleNormalFractalLinesExecute(Sender: TObject);
    procedure aFlipLineStyleExecute(Sender: TObject);
    procedure ToolbarResize(Sender: TObject);
    procedure mnuTransformGridSendbackwardClick(Sender: TObject);
    procedure mnuTransformGridBringforwardClick(Sender: TObject);
    procedure mnuViewZoomClick(Sender: TObject);
    procedure doZoom(zoomstr: String);
    procedure mnuDrawingSymbolsIconSizeClick(Sender: TObject);
    procedure mnuDrawingSymbolsIconSpreadClick(Sender: TObject);
    procedure mnuDrawingFractalRoughnessClick(Sender: TObject);
    procedure mnuDrawingFractalSeedClick(Sender: TObject);
    procedure mnuDrawingGraphGridSizeClick(Sender: TObject);
    procedure mnuDrawingGraphSecGridSizeClick(Sender: TObject);
    procedure edtLineThicknessAcceptText(Sender: TObject;
      var NewText: String; var Accept: Boolean);
  end;

const
  crZoom = 5;
  crPan = 6;
  crText = 7;
  crRosette = 8;
  crRosetteClip = 9;
  crCurve1 = 10;
  crCurve2 = 11;
  crCurve3 = 12;
  crCurve4 = 13;
  crCircle = 14;
  crSquare = 15;
  crFreeHand = 16;
  crLine = 17;
  crLine1 = 18;
  crLine2 = 19;
  crPolygon = 20;
  crSngIcon = 21;
  crDiaIcon = 22;
  crSqrIcon = 23;
  crRndIcon = 24;
  crRuler = 25;
  crGlue = 26;
  crGlueHnd = 27;
  crTGlue = 28;
  crTGlueHnd = 29;
  crTScalpelDelete = 30;
  crTScalpelAdd = 31;
  crTScalpelSeparate = 32;
  crHyperlink  = 33;
  crTScalpelAddIntersection = 34;

var
  MainForm: TMainForm;
  PaintBox: TMainForm;
  CurrentColor, CurrentFillColor: TColor;
  HPal: HPALETTE;

  CurrentFilename: string;
  NeedToParseParams: boolean;

  // Variables for using the map to edit symbols.
  InvokedAsSymbolEditor: boolean;
  MapBeforeSymbolEditor: MapCollection;
  CaptionBeforeSymbolEditor: string;
  CurrentlyEditedSymbol: Symbol;
  EditedSymbolContents: DrawPrimitive;

  PopulatedOverlays: OverlaySet;

  ToolDispatcher: TToolHandler;
  FractalLineHandler: TFractalLineToolHandler;
  FractalPolylineHandler: TFractalPolylineToolHandler;
  FractalCurveHandler: TFractalCurveToolHandler;
  NormalLineHandler: TNormalLineToolHandler;
  NormalCircleHandler: TNormalCircleToolHandler;
  NormalArcHandler: TNormalArcToolHandler;
  NormalPolylineHandler: TNormalPolylineToolHandler;
  RectangleHandler: TRectangleToolHandler;
  PolygonHandler: TPolygonToolHandler;
  NormalCurveHandler: TNormalCurveToolHandler;
  FractalPolyCurveHandler: TFractalPolyCurveToolHandler;
  PolyCurveHandler: TPolyCurveToolHandler;
  SelectionHandler: TSelectionToolHandler;
  RulerHandler: TRulerToolHandler;
  FreehandRulerHandler: TFreehandRulerToolHandler;
  IconHandler: TIconToolHandler;
  ZoomHandler: TZoomToolHandler;
  PanHandler: TPanToolHandler;
  PatternIconHandler: TPatternIconToolHandler;
  SprayIconHandler: TSprayIconToolHandler;
  FreehandHandler: TFreehandLineToolHandler;
  FreehandFractalHandler: TFreehandFractalToolHandler;
  TextHandler: TTextToolHandler;
  CurvedTextHandler: TTextCurveToolHandler;
  ChartGridHandler: TChartGridToolHandler;
  HyperlinkHandler: THyperlinkToolHandler;
  GlueHandler: TGlueHandler;
  ScalpelHandler: TScalpelHandler;

  PopupX, PopupY: integer;
  CursorX, CursorY: Coord;
  LastMouseX, LastMouseY: integer;
  LastInsertFilterIndex : integer;
  LastOrderIndex:integer;
  ChangingGridSizeEdit : Boolean;

implementation

uses ReplacementOverlay, Splash, About, Skew, MapSettings, Snap, Clipbrd,
  CustomPrint, movesel, CreateArray, DefineNewSymbol, NewGroup, LocalizedStrings,
  CustomHint, BitmapProperties, CTManager, frmMakeFrom3DUnit;

{$R *.dfm}

{$R Resource.res}

var AnnotateFont:TFont;


procedure TMainForm.AppDeactivate(Sender: TObject);
begin
  HideCrosshair;
end;


function TMainForm.GetVisibleRect: TRect;
var
  i: integer;
begin
  result := BoundsRect;
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TTBDock) then
      begin
        if Controls[i].Align = alTop then result.top := Controls[i].Top + Controls[i].Height;
        if Controls[i].Align = alBottom then result.bottom := Controls[i].Top;
        if Controls[i].Align = alLeft then result.left := Controls[i].Left + Controls[i].Width;
        if Controls[i].Align = alRight then result.right := Controls[i].Left;
      end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  os: OverlaySet;
  oldcursor: TCursor;
begin
  if Map = nil then exit;

  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  os := Map.OverlaysInUse;
  if (os <> PopulatedOverlays) then UpdateOverlayImages(os);

  if IsPaletteDevice then
    begin
      SelectPalette(PaintBox.Canvas.Handle, hPal, false);
      UnrealizeObject(hPal);
      RealizePalette(PaintBox.Canvas.Handle);
    end;
  Map.Draw(Map.CurrentView, true);

  DrawSnapGrid(false);
  RefreshPushPins;
  ToolDispatcher.Paint;          //zifnabbe 2002/08/18

  SetScrollBarPositions;
  LoadZoomPercentageBox;

  Screen.Cursor := oldcursor;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT, VK_CONTROL, VK_MENU:
      begin
        if Assigned(ToolDispatcher) then
          begin
            ToolDispatcher.HideCrosshair;
            ToolDispatcher.MouseMove(Shift, LastMouseX, LastMouseY);
          end;
      end;
  end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Don't interfere with the workings of dialogs that
  // need these keys.
  //
  // Note: We're testing <> Nil for the dynamically created forms,
  // because they won't normally be around unless they're up--testing
  // against Not .Visible would cause an exception.
  if (ActiveControl = OverlayList) or
    (ActiveControl = ChooseFont.TextContent) or
    (AddSymbolGroup <> nil) or
    (NewSymbol <> nil) or
    (SymbolLibraryForm.Visible) then exit;

  case Key of
    VK_BACK:
      begin
        if Assigned(ToolDispatcher) then ToolDispatcher.Backspace;
        Key := 0;
      end;
    VK_ESCAPE:
      begin
        if Assigned(ToolDispatcher) then ToolDispatcher.Escape;
        Key := 0;
      end;
    VK_DELETE:
      begin
        Map.Delete;
        if Assigned(ToolDispatcher) then ToolDispatcher.Refresh;
        Key := 0;
      end;
    VK_SHIFT, VK_CONTROL, VK_MENU:
      begin
        if Assigned(ToolDispatcher) then ToolDispatcher.MouseMove(Shift, LastMouseX, LastMouseY);
      end;
  end;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MainForm.ActiveControl := nil;  //zifnabbe 20/09/2002; unfocusing of overlaylist bug. Is there a better way?
  if Assigned(ToolDispatcher) then
    begin
      if (not ToolDispatcher.MouseDown(Button, Shift, X, Y)) and (Button = mbRight) then
        begin
          Map.CurrentView.Grid.SetMeasurementUnitChecks(ChangeUnitsSubMenu);
          PopupX := X;
          PopupY := Y;
          PopupMenu.Popup(X, Y);
        end;
    end;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(ToolDispatcher) then
    begin
      LastMouseX := X;
      LastMouseY := Y;
      ToolDispatcher.MouseMove(Shift, X, Y);
    end;
  RefreshPushPinDistances(x, y);
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(ToolDispatcher) then
    begin
      ToolDispatcher.HideCrosshair;
      ToolDispatcher.MouseUp(Button, Shift, X, Y);
    end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  Ini: TRegIniFile;

begin
  //zifnabbe 2002/08/18
  Ini := TRegIniFile.Create('Software\'+Application.Title);
  try
    MRUList.SaveToRegIni(Ini, 'RecentFiles');
  finally
    Ini.Free;
  end;

  FractalLineHandler.Free;
  FractalPolylineHandler.Free;
  FractalCurveHandler.Free;
  NormalLineHandler.Free;
  NormalCircleHandler.Free;
  NormalArcHandler.Free;
  PolygonHandler.Free;
  RectangleHandler.Free;
  NormalPolylineHandler.Free;
  NormalCurveHandler.Free;
  SelectionHandler.Free;
  RulerHandler.Free;
  FreehandRulerHandler.Free;
  IconHandler.Free;
  ZoomHandler.Free;
  PanHandler.Free;
  PatternIconHandler.Free;
  SprayIconHandler.Free;
  FreehandHandler.Free;
  FreehandFractalHandler.Free;
  TextHandler.Free;
  CurvedTextHandler.Free;
  ChartGridHandler.Free;
  FractalPolyCurveHandler.Free;
  PolyCurveHandler.Free;
  GlueHandler.Free;
  ScalpelHandler.Free;

  SelectPalette(PaintBox.Canvas.Handle, 0, false);
  DeleteObject(HPal);
  HPal := 0;

  ClearPushPins;

  Map.Free;
  Map := nil;
end;

// Disables all functions we don't want them messing with in a symbol editing
// session.

procedure TMainForm.EnableSymbolEditFunctions(enabled: boolean);
begin
  FileMenuItem.Visible := not enabled;
  SymbolEditFileMenu.Visible := enabled;

  RestoreDefaultToolbars.Enabled := not enabled; // Or restore toolbars, which would be a third spawn
  DeleteView.Enabled := not enabled; // Or Delete views
  SaveCurrentView.Enabled := not enabled; // Or Save Views
  SymbolLibrary1.Enabled := not enabled; // Or modify the symbol library!
  mnuDefineNewSymbol.Enabled := not enabled; // Or create new symbols.

  //zifnabbe 2002/08/18
  aDefineSymbol.Enabled := not enabled;
  aSymbolLibrary.Enabled := not enabled;
end;

// Switches from editing the map to editing a symbol
procedure TMainForm.InvokeSymbolEditor(name: string; sym: Symbol);
begin
  InvokedAsSymbolEditor := true;

  // Hide the symbol library form
  SymbolLibraryForm.Hide;

  // Keep pointer to existing map
  MapBeforeSymbolEditor := Map;
  CurrentlyEditedSymbol := sym;

  // Make backup of symbol being edited if they cancel the edit
  EditedSymbolContents := sym.Objects.CopyContents;

  // Point to new "map", which is symbol.
  Map := sym.Objects;

  // Allow undos on the symbol
  Map.UndoLevels := Settings.UndoLevels.Value;

  // Set appropriate view controls on the symbol: view everything
  Map.ClearSelection(false);
  Map.CurrentView.Canvas := PaintBox.Canvas;
  Map.CurrentView.ActiveOverlays := [0..255];
  Map.CurrentView.VisibleOverlays := [0..255];
  Map.ShowAll(Map.CurrentView, 0.5);

  // Switch the program caption to the new one for editing symbols
  CaptionBeforeSymbolEditor := Caption;
  Caption := Format(res_main_editing_symbol, [Application.Title, name]);

  // Enable/Disable appropriate menu functions
  EnableSymbolEditFunctions(true);

  // Refresh the tool that is in use
  if Assigned(ToolDispatcher) then ToolDispatcher.Refresh;

  // Show the symbol
  Repaint;
end;

// Switches back to editing the map after having edited the symbol
procedure TMainForm.UninvokeSymbolEditor;
begin
  // Cancel symbol editing.
  InvokedAsSymbolEditor := false;

  // Restore the map
  Map := MapBeforeSymbolEditor;
  Map.ClearSelection(true);
  Map.ShowAll(Map.CurrentView, 0.5);

  // Release the undo resources used by the symbol
  CurrentlyEditedSymbol.Objects.UndoLevels := 0;

  // Restore the menus and caption
  Caption := CaptionBeforeSymbolEditor;
  EnableSymbolEditFunctions(false);

  // Refresh the tool that is in use
  if Assigned(ToolDispatcher) then ToolDispatcher.Refresh;

  // Redisplay
  Repaint;

  // Reactivate the symbol library dialog
  SymbolLibraryForm.Show;
end;

// Used to cancel out of symbol edit mode
procedure TMainForm.CancelSymbolChanges;
begin
  // Toss away the edits made on the symbol
  CurrentlyEditedSymbol.Objects.RestoreContents(EditedSymbolContents);

  // Restore the menus: go back to editing the map
  UninvokeSymbolEditor;
end;

procedure TMainForm.SaveSymbolChanges;
begin
  // They're done editing and they want to save it: toss the saved copy
  // we made in case of backup.
  EditedSymbolContents.ClearChain;
  EditedSymbolContents.Free;

  // They've edited the symbol: remove the cached bitmap from the image list so it
  // will be refreshed.
  CurrentLibrary.RemoveCachedImage(CurrentlyEditedSymbol.ImageIndex);

  // Refresh the current group's cached icons so we recreate the new symbol.
  SymbolGroupListClick(self);

  // Restore the menus: go back to editing the map
  UninvokeSymbolEditor;
end;

procedure TMainForm.ParseParameters;
var
  i: integer;
  s: string;
begin
  NeedToParseParams := false;
  if ParamCount = 0 then exit;

  s := UpperCase(ParamStr(1));

  { Print only -- invoked from Explorer right-click menu and "Print".  We
    put this shortcut into the Explorer registry settings for .Aur files
    during installation}
  if (s = '/P') or (s = '-P') then
    begin
      for i := 2 to ParamCount do
        begin
          LoadMap(ParamStr(i));
          PrintMap(true);
        end;
      Application.Terminate;
      exit;
    end;

  { Any other command line parameter is a filename: we can load the first one,
    but since we are an SDI application, not an MDI application, we just spawn
    off the rest of the maps with separate versions of the program. }
  LoadMap(ParamStr(1));
  if (ParamCount > 1) then
    begin
      for i := 2 to ParamCount do
        begin
          s := ParamStr(0) + ' "' + ParamStr(i) + '"';
          WinExec(PChar(s), SW_SHOWNORMAL);
        end;
    end;
end;

procedure TMainForm.RefreshPushPinMenu;
var i,n:integer;
    t:TMenuItem;
begin
  // Remove existing menu items
  n:=AddPushPin.Count;
  while (n>0) do begin
    AddPushPin.Delete(n-1);
    dec(n);
    end;

  for i := 0 to PushPinList.Items.Count - 1 do begin
    // Set index in the push pin listbox
    PushPinList.ImageIndex[i] := i;

    // Create the menu items
    t := TMenuItem.Create(PopupMenu);
    t.ImageIndex := i;
    t.Caption := Map.PushPinName[i];
    t.OnClick := PushPinMenuItemClick;
    AddPushPin.Add(t);
    end;

end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  ToolCanvas: TCanvas;
  dc: HDC;
  Ini:TRegIniFile;
begin
  // Save Default toolbar positions
  TBRegSavePositions(Self, HKEY_CURRENT_USER, 'Software\'+Application.Title + '\Defaults');

  RestoreToolbarPositions;

  //zifnabbe 2002/08/18
  Ini := TRegIniFile.Create('Software\'+Application.Title);
  try
    MRUList.LoadFromRegIni(Ini, 'RecentFiles');
  finally
    Ini.Free;
  end;

  ChangingGridSizeEdit := False;
  PaintBox := self;

  if IsPaletteDevice then
    begin
      HPal := MainColor.GetPalette;
      dc := GetDC(0);
      SelectPalette(dc, HPal, false);
      RealizePalette(dc);
      ReleaseDC(0, dc);
    end;

  AnnotateFont := TFont.Create;
  AnnotateFont.Name   := 'Arial';
  AnnotateFont.Height := -PushPinImages.Height;
  AnnotateFont.Style  := [];

  //  AddFontResource(PChar(ExtractFilePath(ParamStr(0))+'fmapfnt.ttf'));
  CurrentColor := clBlack;
  CurrentFillColor := clWhite;

  Application.OnDeactivate := AppDeactivate;

  Screen.Cursors[crZoom] := LoadCursor(HInstance, 'Zoom');
  Screen.Cursors[crPan] := LoadCursor(HInstance, 'Pan');
  Screen.Cursors[crText] := LoadCursor(HInstance, 'Text');
  Screen.Cursors[crRosette] := LoadCursor(HInstance, 'Rosette');
  Screen.Cursors[crRosetteClip] := LoadCursor(HInstance, 'RoseClip');
  Screen.Cursors[crCurve1] := LoadCursor(HInstance, 'Curve1');
  Screen.Cursors[crCurve2] := LoadCursor(HInstance, 'Curve2');
  Screen.Cursors[crCurve3] := LoadCursor(HInstance, 'Curve3');
  Screen.Cursors[crCurve4] := LoadCursor(HInstance, 'Curve4');
  Screen.Cursors[crCircle] := LoadCursor(HInstance, 'Circle');
  Screen.Cursors[crSquare] := LoadCursor(HInstance, 'Square');
  Screen.Cursors[crFreeHand] := LoadCursor(HInstance, 'Free');
  Screen.Cursors[crLine] := LoadCursor(HInstance, 'Line');
  Screen.Cursors[crLine1] := LoadCursor(HInstance, 'Line1');
  Screen.Cursors[crLine2] := LoadCursor(HInstance, 'Line2');
  Screen.Cursors[crPolygon] := LoadCursor(HInstance, 'Polygon');
  Screen.Cursors[crSngIcon] := LoadCursor(HInstance, 'SngIcon');
  Screen.Cursors[crDiaIcon] := LoadCursor(HInstance, 'DiaIcon');
  Screen.Cursors[crSqrIcon] := LoadCursor(HInstance, 'SqIcon');
  Screen.Cursors[crRndIcon] := LoadCursor(HInstance, 'RndIcon');
  Screen.Cursors[crRuler] := LoadCursor(HInstance, 'Ruler');
  Screen.Cursors[crGlue] := LoadCursor(HInstance, 'Glue');
  Screen.Cursors[crGlueHnd] := LoadCursor(HInstance, 'GlueHnd');
  Screen.Cursors[crTGlue] := LoadCursor(HInstance, 'GlueT');
  Screen.Cursors[crTGlueHnd] := LoadCursor(HInstance, 'GlueHndT');
  Screen.Cursors[crTScalpelDelete] := LoadCursor(HInstance, 'ScplDel');
  Screen.Cursors[crTScalpelAdd] := LoadCursor(HInstance, 'ScplAdd');
  Screen.Cursors[crTScalpelSeparate] := LoadCursor(HInstance, 'ScplSep');
  Screen.Cursors[crHyperlink] := LoadCursor(HInstance, 'Hyperlink');
  Screen.Cursors[crTScalpelAddIntersection] := LoadCursor(HInstance, 'ScplAddI');

  Map := MapCollection.Create(self); (*!!*)
  Map.CurrentView.Canvas := PaintBox.Canvas;

  Map.CurrentView.Grid.FillPopupMenu(MainForm.ChangeUnitsSubMenu, MeasurementMenuItemClick);
  FillPattern.Pattern := 0;

  PopulatedOverlays := [];
  Map.CurrentView.ActiveOverlays := [];
  Map.CurrentView.VisibleOverlays := [];
  for i := 0 to OverlayList.Items.Count - 1 do
    begin
      OverlayList.Checked[i] := true;
      Map.CurrentView.VisibleOverlays := Map.CurrentView.VisibleOverlays + [i];
      Map.CurrentView.ActiveOverlays := Map.CurrentView.ActiveOverlays + [i];
    end;
  ActiveOverlay.ItemIndex := 0;

  for i := 0 to GetNumberLineStyles - 1 do
    begin
      LineStyleComboBox.Items.Add(IntToStr(i));
    end;
  LineStyleComboBox.ItemIndex := 1;

  for i := 0 to GetNumberLineEndStyles - 1 do
    begin
      BeginLineStyle.Items.Add(IntToStr(i));
      EndLineStyle.Items.Add(IntToStr(i));
    end;
  BeginLineStyle.ItemIndex := 0;
  EndLineStyle.ItemIndex := 0;

  edtLineThickness.Text := '1';

  for i := 0 to NumberGridStyles - 1 do
    begin
      PrimaryGridStyle.Items.Add(IntToStr(i));
      SecondaryGridStyle.Items.Add(IntToStr(i));
    end;
  PrimaryGridStyle.ItemIndex := 0;
  SecondaryGridStyle.ItemIndex := 0;

  RefreshPushPinMenu;

  ReadOverlayColors;

  ToolCanvas := PaintBox.Canvas;

  FractalLineHandler := TFractalLineToolHandler.Create(ToolCanvas);
  FractalPolylineHandler := TFractalPolylineToolHandler.Create(ToolCanvas);
  FractalCurveHandler := TFractalCurveToolHandler.Create(ToolCanvas);
  NormalLineHandler := TNormalLineToolHandler.Create(ToolCanvas);
  NormalCircleHandler := TNormalCircleToolHandler.Create(ToolCanvas);
  NormalArcHandler    := TNormalArcToolHandler.Create(ToolCanvas);
  PolygonHandler := TPolygonToolHandler.Create(ToolCanvas);
  RectangleHandler := TRectangleToolHandler.Create(ToolCanvas);
  NormalPolylineHandler := TNormalPolylineToolHandler.Create(ToolCanvas);
  NormalCurveHandler := TNormalCurveToolHandler.Create(ToolCanvas);
  SelectionHandler := TSelectionToolHandler.Create(ToolCanvas);
  RulerHandler := TRulerToolHandler.Create(ToolCanvas);
  FreehandRulerHandler := TFreehandRulerToolHandler.Create(ToolCanvas);
  IconHandler := TIconToolHandler.Create(ToolCanvas);
  ZoomHandler := TZoomToolHandler.Create(ToolCanvas);
  PanHandler := TPanToolHandler.Create(ToolCanvas);
  PatternIconHandler := TPatternIconToolHandler.Create(ToolCanvas);
  SprayIconHandler := TSprayIconToolHandler.Create(ToolCanvas);
  FreehandHandler := TFreehandLineToolHandler.Create(ToolCanvas);
  FreehandFractalHandler := TFreehandFractalToolHandler.Create(ToolCanvas);
  TextHandler := TTextToolHandler.Create(ToolCanvas);
  CurvedTextHandler := TTextCurveToolHandler.Create(ToolCanvas);
  ChartGridHandler := TChartGridToolHandler.Create(ToolCanvas);
  FractalPolyCurveHandler := TFractalPolyCurveToolHandler.Create(ToolCanvas);
  PolyCurveHandler := TPolyCurveToolHandler.Create(ToolCanvas);
  GlueHandler := TGlueHandler.Create(ToolCanvas);
  ScalpelHandler := TScalpelHandler.Create(ToolCanvas);
  HyperlinkHandler := THyperLinkToolHandler.Create(ToolCanvas);

  aSelectionExecute(self);
  SquareGraphBtn.Down := true;
  SquareGraphBtnClick(self);

  SplashForm.Free;

  Map.ClearModified;
  Map.UndoLevels := Settings.UndoLevels.Value;
  BuildViewList;
  UpdateGridLabel;

  Application.OnIdle := WhenIdle;

  DragAcceptFiles(MainForm.Handle, TRUE);
  // a way to have multi-help files : use the name autorlm_(localiszation).hlp
  Application.HelpFile := ExtractFilePath(ParamStr(0)) + res_main_autorlm_hlp;

  InvokedAsSymbolEditor := false;

  NeedToParseParams := true;
end;

procedure TMainForm.WhenIdle(Sender: TObject; var Done: Boolean);
var
  s: string;
begin
  if NeedToParseParams then
    begin
      ParseParameters;
      NeedToParseParams := false;
    end;

  TextToolbarItem.Checked := ChooseFont.Visible;

  //zifnabbe 2002/08/18
  aDisplayGrid.Checked := MapSettingsDialog.DisplayGrid.Checked;
  aDisplayOverlay.Checked := Settings.VisualOverlays.Checked;
  aGridSnap.Checked := MapSettingsDialog.SnapToGrid.Checked;
  aGravitySnap.Checked := MapSettingsDialog.SnapToPoint.Checked;
  aGravitySnapAlong.Checked := MapSettingsDialog.cbSnapTo.Checked;     // JD 8-13-02
  aRotateSnap.Checked       := MapSettingsDialog.cbRotateSnap.Checked; // JD 8-13-02
  aRotateSnap.Enabled    := MapSettingsDialog.cbSnapTo.Checked;     // JD 8-13-02
  MapSettingsDialog.cbRotateSnap.Enabled := aRotateSnap.Enabled;    // JD 8-13-02

  if Assigned(MapSettingsDialog) then
    begin
      SnapToGridMenuItem.Checked := MapSettingsDialog.SnapToGrid.Checked;
      GravitySnapMenuItem.Checked := MapSettingsDialog.SnapToPoint.Checked;
    end;

  if Assigned(Map) then
    begin
      if not InvokedAsSymbolEditor then
        begin
          DeleteView.Enabled := (Map.GetViewPoints <> 0);
        end;

      s := Map.CurrentUndoName;
      UndoMenuItem.Caption := Format(res_main_undo, [s]);
      UndoMenuItem.Enabled := (s <> '');
      aUndo.Enabled := UndoMenuItem.Enabled;
      aUndo.Hint := StripHotKey(UndoMenuItem.Caption);

      s := Map.CurrentRedoName;
      RedoMenuItem.Caption := Format(res_main_redo, [s]);
      RedoMenuItem.Enabled := (s <> '');
      aRedo.Enabled := RedoMenuItem.Enabled;
      aRedo.Hint := StripHotKey(RedoMenuItem.Caption);
    end;
end;

procedure TMainForm.LineStyleComboBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  y: integer;
  st: StyleAttrib;
begin
  with (Control as TCustomComboBox).Canvas do
    begin
      if odFocused in State then
        begin
          Brush.Color := clHighlight;
          FillRect(Rect);
          Pen.Color := clHighlightText;
        end
      else
        begin
          Brush.Color := clWindow;
          FillRect(Rect);
          Pen.Color := clWindowText;
        end;

      y := (Rect.Top + Rect.Bottom) div 2;
      st.Bits := 0;

      if (Control = BeginLineStyle) then
        begin
          st.Line  := LineStyleComboBox.ItemIndex;
          st.First := Index;
          inc(Rect.Left, 4);
        end
      else
        if (Control = EndLineStyle) then
          begin
            st.Line := LineStyleComboBox.ItemIndex;
            st.Last := Index;
            dec(Rect.Right, 4);
          end
        else
          begin
            st.Line := Index;
          end;
      If St.Line = start_numeric_thickness_style
       Then (Control as TCustomComboBox).Canvas.TextOut(Rect.Left + 2,Rect.Top + 2,'Numeric')
       Else DrawLineStyle((Control as TCustomComboBox).Canvas,
             Rect.Left + 4, y, Rect.Right - 4, y, st);
    end;
end;

function TMainForm.FindOverlay(s: string): boolean;
var
  i: integer;
begin
  for i := 0 to OverlayList.Items.Count - 1 do
    begin
      if OverlayList.Items[i] = s then
        begin
          Result := true;
          exit;
        end;
    end;

  Result := false;
end;

procedure TMainForm.AddOverlay(s: string);
var
  n: integer;
begin
  Map.SetUndoPoint(res_main_add_overlay);
  OverlayList.Items.Append(s);
  ActiveOverlay.Items.Append(s);
  n := OverlayList.Items.Count - 1;
  OverlayList.Checked[n] := true;
  Map.CurrentView.VisibleOverlays := Map.CurrentView.VisibleOverlays + [n];
  Map.CurrentView.ActiveOverlays := Map.CurrentView.ActiveOverlays + [n];
  OverlayList.ItemIndex := n;
  ActiveOverlay.ItemIndex := n;
  Map.SetModified(modOverlay);
  UpdateOverlayImages(Map.OverlaysInUse);
end;

procedure TMainForm.DeleteOverlay(n: integer);
begin
  OverlayList.Items.Delete(n);
  ActiveOverlay.Items.Delete(n);
  Map.SetModified(modOverlay);
  UpdateOverlayImages(Map.OverlaysInUse);
end;

procedure TMainForm.OverlayListClickCheck(Sender: TObject);
var
  i: integer;
  OldVisible: OverlaySet;
  Changed: OverlaySet;
begin
  OldVisible := Map.CurrentView.VisibleOverlays;

  Map.CurrentView.VisibleOverlays := [];
  Map.CurrentView.ActiveOverlays := [];

  for i := 0 to OverlayList.Items.Count - 1 do
    begin
      case OverlayList.State[i] of
        cbChecked:
          begin
            Map.CurrentView.ActiveOverlays := Map.CurrentView.ActiveOverlays + [i];
            Map.CurrentView.VisibleOverlays := Map.CurrentView.VisibleOverlays + [i];
          end;
        cbGrayed:
          begin
            Map.CurrentView.VisibleOverlays := Map.CurrentView.VisibleOverlays + [i];
          end;
      end;
    end;

  Map.SetModified(modOverlayState);
  Map.ClearSelection;
  SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).

  // Do some work to avoid a repaint; only repaint if we've actually
  // change a visible overlay, and that overlay actually contains
  // an object.
  Changed := (OldVisible + Map.CurrentView.VisibleOverlays) -
    (OldVisible * Map.CurrentView.VisibleOverlays);

  if (OldVisible <> Map.CurrentView.VisibleOverlays) and
    (Changed * PopulatedOverlays <> []) then DoInvalidate;
end;

procedure TMainForm.ActiveOverlayKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  msg: string;
begin
  if (Key = VK_RETURN) then
    begin
      if not FindOverlay(ActiveOverlay.Text) then
        begin
          if (OverlayList.Items.Count >= maximum_overlays) then
            begin
              msg := Format(res_main_maximum_overlays_reached, [maximum_overlays]);
              Application.MessageBox(pchar(msg), pchar(res_main_maximum_overlay_title), MB_OK);
              ActiveOverlay.ItemIndex := 0;
            end
          else
            begin
              AddOverlay(ActiveOverlay.Text);
            end;
        end;
      Key := 0;
    end;
end;

procedure TMainForm.ActiveOverlayChange(Sender: TObject);
begin
  if FindOverlay(ActiveOverlay.Text) then
    begin
      ActiveOverlay.ItemIndex := ActiveOverlay.Items.IndexOf(ActiveOverlay.Text);

      if (ActiveOverlay.ItemIndex <> -1) then
        begin
          Map.SetOverlay(ActiveOverlay.ItemIndex);
        end;
    end;
end;

procedure TMainForm.OverlayListClick(Sender: TObject);
begin
  ActiveOverlay.ItemIndex:=OverlayList.ItemIndex;
end;

procedure TMainForm.OverlayListDblClick(Sender: TObject);
begin
  ActiveOverlay.ItemIndex := OverlayList.ItemIndex;
  Map.SetOverlay(ActiveOverlay.ItemIndex);
end;

procedure TMainForm.OverlayListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  name, msg: string;
  i: integer;
  response: integer;
begin
  if (Key = VK_DELETE) then
    begin
      HideCrosshair;
      if (OverlayList.ItemIndex >= 0) and (OverlayList.Items.Count > 1) then
        begin
          name := OverlayList.Items[OverlayList.ItemIndex];
          msg := Format(res_main_del_overlay, [name]);

          if Application.MessageBox(PChar(msg), PChar(res_main_sure), MB_YESNO) = IDYES then
            begin

              { If anything is in that overlay, make sure we ask before deleting it...}
              if (OverlayList.ImageIndex[OverlayList.ItemIndex] <> -1) then
                begin
                  msg := Format(res_main_remove_objects, [name]);
                  response := Application.MessageBox(PChar(msg), pchar(res_main_sure), MB_YESNO);
                end
              else
                response := IDYES;

              case response of
                IDYES:
                  begin
                    Map.RemoveOverlay(OverlayList.ItemIndex, 0, true);
                    DeleteOverlay(OverlayList.ItemIndex);
                    DoInvalidate;
                    ToolDispatcher.Refresh;
                  end;
                IDNO:
                  begin
                    // Be a more polite consumer of resources: dynamically
                    // create the form as it is needed
                    Application.CreateForm(TReplacementOverlayForm, ReplacementOverlayForm);
                    ReplacementOverlayForm.OverlaySelection.Items.Clear;

                    for i := 0 to OverlayList.Items.Count - 1 do
                      begin
                        if (i <> OverlayList.ItemIndex) then
                          begin
                            ReplacementOverlayForm.OverlaySelection.Items.Append(OverlayList.Items[i]);
                          end
                        else
                          begin
                            ReplacementOverlayForm.OverlaySelection.Items.Append(OverlayList.Items[i] + res_main_deleting);
                          end;
                      end;

                    if (ReplacementOverlayForm.ShowModal = mrOK) and
                      (OverlayList.ItemIndex <> ReplacementOverlayForm.OverlaySelection.ItemIndex) then
                      begin
                        Map.RemoveOverlay(OverlayList.ItemIndex,
                          ReplacementOverlayForm.OverlaySelection.ItemIndex,
                          false);
                        DeleteOverlay(OverlayList.ItemIndex);
                        DoInvalidate;
                        ToolDispatcher.Refresh;
                      end;

                    // Free our form.
                    ReplacementOverlayForm.Free;
                  end;
                IDCANCEL:
                  begin
                  end;
              end;
            end;
        end;
    end;
end;

procedure TMainForm.FlashReadOnlyProhibits;
var i:integer; // XXX TOM
begin
  for i:=1 to 3 do begin
    aReadOnly.Enabled := false;
    //aReadOnly.Repaint;                //zifnabbe 2002/08/18
    Sleep(50);
    aReadOnly.Enabled := true;
    //aReadOnly.Repaint;                 //zifnabbe 2002/08/18
    Sleep(50);
    end;
end;

procedure TMainForm.SwitchToolDispatcher(newtoolindex: integer; newtool: TToolHandler);
begin
  if ToolDispatcher <> nil then ToolDispatcher.Cancel;

  // Only let them pick tools that are non-destructive if map is read-only.
  if Map.ReadOnly and (not (newtoolindex in [0,1,8,9,22])) then begin
    FlashReadOnlyProhibits;            // "Explain" why we won't set the tool.
    aSelectionExecute(self);
    exit;
    end;

  //zifnabbe 2002/08/18
  aSelection.Checked := (newtoolindex = 0);
  aRuler.Checked := (newtoolindex = 1);
  aFractalLine.Checked := (newtoolindex = 2);
  aFractalPolyLine.Checked := (newtoolindex = 3);
  aFractalCurve.Checked := (newtoolindex = 4);
  aNormalLine.Checked := (newtoolindex = 5);
  aNormalPolyLine.Checked := (newtoolindex = 6);
  aNormalCurve.Checked := (newtoolindex = 7);
  aZoomIn.Checked := (newtoolindex = 8);
  aPan.Checked := (newtoolindex = 9);

  aPlaceOne.Checked := (newtoolindex = 10);
  aPlaceSquare.Checked := (newtoolindex = 11);
  aPlaceDiamond.Checked := (newtoolindex = 12);
  aPlaceRandom.Checked := (newtoolindex = 13);

  aNormalFreeHand.Checked := (newtoolindex = 14);
  aCircle.Checked := (newtoolindex = 15);
  aPolygon.Checked := (newtoolindex = 16);

  aTextOut.Checked := (newtoolindex = 17);
  aCurvedText.Checked := (newtoolindex = 18);

  aFractalFreehand.Checked := (newtoolindex = 19);
  aChartGrid.Checked := (newtoolindex = 20);
  aRectangle.Checked := (newtoolindex = 21);

  aFreehandRuler.Checked := (newtoolindex = 22);

  aNormalPolyCurve.Checked := (newtoolindex = 23);
  aFractalPolyCurve.Checked := (newtoolindex = 24);

  aGlue.Checked := (newtoolindex = 25);
  aScalpel.Checked := (newtoolindex = 26);
  aHyperlink.Checked := (newtoolindex = 27);
  aArc.Checked       := (NewToolIndex = 28);

  ToolDispatcher := newtool;
  ToolDispatcher.Refresh;

  MainForm.Cursor := crDefault;
end;

procedure TMainForm.IconBtnClick(Sender: TObject);
var s: Symbol;
begin
  if (ToolDispatcher <> IconHandler) and
    (ToolDispatcher <> PatternIconHandler) and
    (ToolDispatcher <> SprayIconHandler)
    then aPlaceOneExecute(Sender);

  if (SymbolIconList.SelCount > 0) then begin
    s := Symbol(SymbolIconList.Selected.Data);
    end
  else
    s:=nil;

  // Set the symbol to be inserted into the map
  if (s = nil) then
    TextTool.InsertSymbol := nil
  else
    TextTool.InsertSymbol := s;
end;

procedure TMainForm.SymbolGroupListClick(Sender: TObject);
var grp:SymbolGroup;
    index:integer;
begin
  index := SymbolGroupList.ItemIndex;
  if (index<>-1) then begin
    // If they select a page in the icon group list,
    // go ahead and refresh the icon page to display
    // icons from that group.
    grp:=SymbolGroup(SymbolGroupList.Items.Objects[index]);
    SymbolLibraryForm.SelectGroup(grp);
    end;
end;

procedure TMainForm.ColorBtnClick(Sender: TObject);
begin
  {set CurrentIconColor}
  { 2003/12/01 - J.Friant - somehow the next line was
    deleted and so the function wasn't working properly
    any more. }
  CurrentColor := MainColor.color;
  FillPattern.ForegroundColor := CurrentColor;
  if not Settings.FreezeProperties.Checked then
    begin
      Map.SetColor(CurrentColor);
    end;
end;

procedure TMainForm.FillColorBtnClick(Sender: TObject);
begin
  CurrentFillColor := FillColor.color;
  FillPattern.BackgroundColor := CurrentFillColor;
  if not Settings.FreezeProperties.Checked then
    begin
      Map.SetFillColor(CurrentFillColor);
    end;
end;

procedure TMainForm.OutlineColorClick(Sender: TObject);
var
  attrib: TextAttrib;
begin
  if not Settings.FreezeProperties.Checked then
    begin
      if Map.AnythingSelected then
        begin
          HideCrosshair;
          Map.SetUndoPoint(res_main_color_change_outline);
          attrib.Valid := [tatOutlineColor];
          attrib.FontOutlineColor := OutlineColor.Color;
          Map.SetTextAttrib(Map.CurrentView, attrib);
        end;
    end;
end;

procedure TMainForm.RefreshExtent;
begin
  SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
  ToolDispatcher.Paint;
end;

procedure TMainForm.SeedSpinChange(Sender: TObject);
begin
  if (SeedSpin.Text <> '') then
    begin
      if not Settings.FreezeProperties.Checked then
        begin
          if Map.SetSeed(SeedSpin.Value) then RefreshExtent;
        end;
    end;
end;

procedure TMainForm.RoughnessTrackBarChange(Sender: TObject);
begin
  if not Settings.FreezeProperties.Checked then
    begin
      if Map.SetRoughness(RoughnessTrackBar.Position) then RefreshExtent;
    end;
end;

procedure TMainForm.LineStyleComboBoxChange(Sender: TObject);
var
  style: StyleAttrib;
begin
  if (LineStyleComboBox.ItemIndex <> -1) then
    begin
      BeginLineStyle.Repaint;
      EndLineStyle.Repaint;
      if not Settings.FreezeProperties.Checked then
        begin
          style.bits := $FFFFFFFF;
          style.Line := LineStyleComboBox.ItemIndex;
          Style.FullStyle.Thickness := -1;
          if Map.SetStyle(style) then RefreshExtent;
        end;
    end;
end;

procedure TMainForm.HideCrosshair;
begin
  // If we're destroying the application, don't try to hide the crosshair;
  // we may have already destroyed too much of the app to do this, and it
  // won't matter soon anyway.
  if Application.Terminated then exit;

  if Assigned(ToolDispatcher) then ToolDispatcher.HideCrosshair;
end;

procedure TMainForm.DoInvalidate;
begin
  HideCrosshair;
  Invalidate;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Map = nil then exit;
  Map.CurrentView.SetCoordinateSize(MainForm.Width, MainForm.Height, true);
  DoInvalidate;

  AdhereScrollBars;
end;

procedure TMainForm.SetMapGrid(name:string; grid:TGraphGrid);
begin
  if (Map.CurrentView.Grid.GridType <> grid) then begin
    if Map.ReadOnly then begin
      FlashReadOnlyProhibits;
      exit;
      end
    else begin
      Map.SetUndoPoint(name);
      Map.CurrentView.Grid.GridType := grid;

      if (grid<>gtNone) then begin
        MapSettingsDialog.SetGraphScale(-1, '', 1);
        end;

      DoInvalidate;
      Map.SetModified(modGrid);
      UpdateGridLabel;
      end;
    end;
end;

procedure TMainForm.NoGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(res_main_grid_remove, gtNone);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphBlank.Checked := True
  else
    NoGraphBtn.Down := True;
end;

procedure TMainForm.SquareGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(SquareGraphBtn.Hint, gtSquare);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphSquare.Checked := True
  else
    SquareGraphBtn.Down := True;
end;

procedure TMainForm.HexGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(HexGraphBtn.Hint, gtHex);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphHexVert.Checked := True
  else
    HexGraphBtn.Down := True;
end;

procedure TMainForm.TriangleGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(TriangleGraphBtn.Hint, gtTriangle);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphTriangle.Checked := True
  else
    TriangleGraphBtn.Down := True;
end;

procedure TMainForm.RotatedHexGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(RotatedHexGraphBtn.Hint, gtRotatedHex);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphHexHorz.Checked := True
  else
    RotatedHexGraphBtn.Down := True;
end;

procedure TMainForm.DiamondGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(DiamondGraphBtn.Hint, gtDiamond);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphDiamond.Checked := True
  else
    DiamondGraphBtn.Down := True;
end;

procedure TMainForm.HalfDiamondGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(HalfDiamondGraphBtn.Hint, gtHalfDiamond);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphHalfDiamond.Checked := True
  else
    HalfDiamondGraphBtn.Down := True;
end;

procedure TMainForm.PolarGraphBtnClick(Sender: TObject);
begin
  SetMapGrid(PolarGraphBtn.Hint, gtPolar);
  if ( Sender.ClassName = 'TSpeedButton' ) then
    mnuDrawingGraphPolar.Checked := True
  else
    PolarGraphBtn.Down := True;

  // Regardless of whether we actually changed to polar or not,
  // center the view at the map's center.  This allows a click
  // on the polar button to recenter the map (The Polar grid is the
  // only grid where the orgin matters.)
  Map.CurrentView.Zoom(MakeCoordpoint(0, 0), 1.0);
  DoInvalidate;
end;

procedure TMainForm.BoldUnitCountChange(Sender: TObject);
begin
  if Map.ReadOnly then begin
    FlashReadOnlyProhibits;
    exit;
    end;

  Map.SetUndoPoint(res_main_grid_bold, true);
  Map.CurrentView.Grid.GridBoldUnits := BoldUnitCount.Value;
  DoInvalidate;
  Map.SetModified(modGrid);
end;

procedure TMainForm.PrimaryGridStyleChange(Sender: TObject);
begin
  if Map.ReadOnly then begin
    FlashReadOnlyProhibits;
    exit;
    end;

  Map.SetUndoPoint(res_main_grid_style1, true);
  Map.CurrentView.Grid.PrimaryGridStyle := GridPenStyle(PrimaryGridStyle.ItemIndex);
  DoInvalidate;
  Map.SetModified(modGrid);
end;

procedure TMainForm.SecondaryGridStyleChange(Sender: TObject);
begin
  if Map.ReadOnly then begin
    FlashReadOnlyProhibits;
    exit;
    end;

  Map.SetUndoPoint(res_main_grid_style2, true);
  Map.CurrentView.Grid.SecondaryGridStyle := GridPenStyle(SecondaryGridStyle.ItemIndex);
  DoInvalidate;
  Map.SetModified(modGrid);
end;

procedure TMainForm.GridStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Canvas: TCanvas;
  y: integer;
begin
  Canvas := (Control as TCustomComboBox).Canvas;

  if odFocused in State then
    begin
      Canvas.Brush.Color := clTeal;
      Canvas.FillRect(Rect);
      Canvas.Pen.Color := clWhite;
      Canvas.Font.Color := clWhite;
    end
  else
    begin
      Canvas.Brush.Color := clWhite;
      Canvas.FillRect(Rect);
      Canvas.Pen.Color := clTeal;
      canvas.Font.Color := clTeal;
    end;

  if (Index = 0) then
    begin
      Canvas.Font.Size := 6;
      Canvas.TextRect(Rect, Rect.left,
        (Rect.bottom - Rect.top) div 2 - Canvas.TextHeight(res_main_grid_default) div 2, res_main_grid_default);
    end
  else
    begin
      GridObject.SetGridPenStyle(Canvas, GridPenStyle(Index), gpsDefault);

      y := (Rect.Top + Rect.Bottom) div 2;
      canvas.MoveTo(Rect.left, y);
      canvas.LineTo(Rect.Right, y);
    end;
end;

procedure TMainForm.GridSizeBarChange(Sender: TObject);
begin
  if Map.ReadOnly then begin
    FlashReadOnlyProhibits;
    exit;
    end;

  if (GridSizeBar.Position * UnitsPerGridTick <> Map.CurrentView.Grid.CurrentGridSize) then
    begin
      Map.SetUndoPoint(res_main_grid_resize, true);
      Map.CurrentView.Grid.SetGraphUnits(MapSettingsDialog.UnitComboBox.ItemIndex, GridSizeBar.Position * UnitsPerGridTick);
      DoInvalidate;
      Map.SetModified(modGrid);
      UpdateGridLabel;
    end;
end;

// JD 7-30-02

Function TMainForm.GetGridPositionFromLabel(LabelValue: Double): Double;
Var D: Double;
Begin
  D := LabelValue;
  if GetProfileInt('Intl', 'iMeasure', 1) = 0 then D := D * 2.54;
  case Map.CurrentView.Grid.GridType of
    gtNone, gtPolar:
    begin
      Result := D;
      Exit;
    end;
    gtHex, gtRotatedHex:
    begin
      D := D * 1.7320508;
    end;
    gtTriangle:
    begin
      D := D * 0.8660254;
    end;
  end;
  D := ((30 * 45) / D) / (UnitsPerGridTick * 7);
  Result := D;
End; // TMainForm.GetGridPositionFromLabel

procedure TMainForm.UpdateGridLabel;
var
  length: double;
  block, units: string;
begin
  length := (30 * 45) / (GridSizeBar.Position * UnitsPerGridTick * 7);
  case Map.CurrentView.Grid.GridType of
    gtNone, gtPolar:
      begin
        GridLabel.Caption := '';
        exit;
      end;
    gtSquare, gtDiamond, gtHalfDiamond:
      begin
        block := res_main_grid_squares;
      end;
    gtHex, gtRotatedHex:
      begin
        block := res_main_grid_hexes;
        length := length / 1.7320508;
      end;
    gtTriangle:
      begin
        block := res_main_grid_triangles;
        length := length / 0.8660254;
      end;
  end;

  if GetProfileInt('Intl', 'iMeasure', 1) = 0 then
    begin
      length := length / 2.54;
      units := res_main_grid_per_cm;
    end
  else
    units := res_main_grid_per_inch;

  edtGridSize.Text  := Format('%.2f', [length]);
  GridLabel.Caption := Format('%s%s', [block, units]);
end;

procedure TMainForm.IconSizeBarChange(Sender: TObject);
//var attrib,currattrib:TextAttrib;
begin
  // ----------------------------------------------------------
  // Don't do this anymore, since we can't change the size of
  // anything but the built-in symbols.  Let it just change
  // the size of the pre-placed icons: we show a rectangle for
  // sizing estimation.
  // ----------------------------------------------------------
  //  if not Settings.FreezeProperties.Checked then begin
  //    ToolDispatcher.HideCrosshair;
  //    Map.SetUndoPoint('Icon Resize',true);
  //    attrib.Valid:=[tatIconSize];
  //    attrib.IconSize:=IconSizeBar.Position;
  //    currattrib:=Map.GetTextAttrib;
  //    // All icons item will have an IconSize, so what we're
  //    // really asking is if any icons are selected.  Don't
  //    // repaint if unnecessary.
  //    if tatIconSize in currattrib.Valid then begin
  //      Map.InvalidateSelect(true);
  //      Map.SetTextAttrib(attrib);
  //      RefreshExtent;
  //      end;
  //    end;
  // ----------------------------------------------------------
end;

procedure TMainForm.SaveMap(filename: string);
var
  f          : TFileStream;
  id         : integer;
  version    : integer;
  oldcursor  : TCursor;
  backupname : string;
  Ext        : String;
  D          : TDOMDocument;
  XML        : String;

begin
  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  CurrentFilename := filename;
  Ext := Uppercase(ExtractFileExt(filename));
  if (Ext = '.AUR') Or (Ext = '.AURX') then
    begin
      MRUList.Add(filename);
    end;

  if Settings.MakeBackups.Checked then
    begin
      backupname := ChangeFileExt(filename, '.bak');
      DeleteFile(PChar(backupname));
      RenameFile(filename, backupname);
    end;

  try
    f := TFileStream.Create(filename, fmCreate or fmShareExclusive);
    If Ext = '.AURX' Then
    Begin
      D := Map.GetAsDOMDocument(True);
      DOMToXMLParser1.writeToString(D,'Latin1',XML);
      f.WriteBuffer(PChar(XML)^, Length(XML));
      DOMImpl.freeDocument(D);
      DOMImpl.freeUnusedASModels;
    End
    Else
    Begin
      id := MapFileId;
      version := CURRENT_MAP_VERSION;
      f.WriteBuffer(id, sizeof(id));
      f.WriteBuffer(version, sizeof(version));
      Map.Write(f,true,False,False);
    End;
    f.Free;
    Map.ClearModified;
  except
    else
      ShowMessage(Format(res_main_file_nosave, [filename]));
  end;
  // 2003/05/22 - J.Friant
  // It appears that the view list get's corrupted by the
  // save process, so we'll rebuild it once were finished
  // saving the file (this function is also called by
  // SaveCurrentViewClick after it adds a view, so it
  // seemed like it would do the trick here).
  BuildViewList;
  Screen.Cursor := oldcursor;
end;

procedure TMainForm.LoadMap(filename: string; tempfile: boolean);
var
  f           : TFileStream;
  id          : integer;
  version     : integer;
  oldcursor   : TCursor;
  P           : PChar;
  IsXML       : Boolean;
  D           : TDOMDocument;

begin
  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  Map.UndoLevels := 0;
  f := nil;
  try
    f := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);

    // Check to see if it's an XML file
    // or a zipped XML file (jf 2003/07/31)

    GetMem(P,6);
    StrPCopy(P,'12345');
    F.ReadBuffer(P^,5);
    IsXML := (UpperCase(Trim(StrPas(P))) = '<?XML');
    FreeMem(P,6);
    F.Position := 0;

    If IsXML Then
    Begin
      D := XMLToDOMParser1.streamToDom(F);

      // Turn off the map's read-only attribute to do the load: we'll turn it back
      // on when we're done.
      Map.ReadOnly := false;

      Map.Clear;

      Map.ReadFromDOMDocument(D,True);
      DOMImpl.freeDocument(D);
      DOMImpl.freeUnusedASModels;
    End
    Else
    Begin
      f.ReadBuffer(id, sizeof(id));
      f.ReadBuffer(version, sizeof(version));
      if (id <> MapFileId) then
        begin
          ShowMessage(Format(res_main_file_noload, [filename]));
          f.Free;
          exit;
        end;

      if (version > CURRENT_MAP_VERSION) or (version < MAP_VERSION_3) then
        begin
          ShowMessage(Format(res_main_file_incompatible, [filename]));
          f.Free;
          exit;
        end;

      // Turn off the map's read-only attribute to do the load: we'll turn it back
      // on when we're done.
      Map.ReadOnly := false;

      Map.Clear;

      Map.Read(f, true, False, False, version);
    End;
    f.Free;
  except
    else
      begin
        ShowMessage(Format(res_main_file_noopen, [filename]));
        f.Free;
        exit;
      end;
  end;

  CurrentFilename := filename;

  // If the file is a temporary file, we don't add it to the most recently
  // used list.
  if not tempfile then
    begin
      MRUList.Add(filename);
      Caption := res_application_title + ' - ' + filename;
    end;

  BuildViewList;
  Map.ClearSelection(false);
  Map.RestoreLastView;
  Map.SynchronizeOverlayList(OverlayList);
  SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
  Map.ClearModified;
  Map.UndoLevels := Settings.UndoLevels.Value;

  Map.ReadOnly := aReadOnly.Checked;
  Screen.Cursor := oldcursor;
end;

procedure TMainForm.InsertMenuItemClick(Sender: TObject);
var
   ext: string;
   success: boolean;
   OD : TOpenPictureDialog;
begin
  HideCrosshair;
  OD := TOpenPictureDialog.Create(Self);
  OD.Filter := 'AutoREALM Map (.AuR)|*.AuR|' +                    // 1
               'AutoREALM XML Map (.AuRX)|*.AuRX|' +              // 1a
               'Windows Metafile (.WMF; .EMF)|*.wmf;*.emf|' +     // 2
               'Bitmap (.BMP)|*.bmp|' +                           // 3
               'JPEG Image(*.JPG)|*.jpg';                         // 4
  OD.FilterIndex := LastInsertFilterIndex;
  try
    if OD.Execute then
      begin
        ext := Uppercase(ExtractFileExt(OD.Filename));

        if (ext = '.WMF') or (ext = '.EMF') then begin
          LastInsertFilterIndex := 2;
          success := Map.InsertMetafile(OD.Filename);
          end
        else
          if (ext = '.BMP') then begin
            LastInsertFilterIndex := 3;
            success := Map.InsertBitmap(OD.Filename);
            end
        else
          if (ext = '.JPG') then begin
            LastInsertFilterIndex := 4;
            success := Map.InsertJPEG(OD.FileName);
            end
        else begin { .AUR or .AURX hopefully }
           LastInsertFilterIndex := 1;
           success := Map.InsertMap(OD.Filename);
        end;

        if not success then ShowMessage(Format(res_main_file_noinsert, [OD.Filename]));
      end;
  except
    // Swallow errors; we should be catching them before this point.
  end;
  OD.Free;
end;

function TMainForm.AlrightToSaveTo(filename: string): boolean;
begin
  Result := true;

  if FileExists(filename) then
    begin
      if Application.MessageBox(PChar(Format(res_main_file_exist, [filename])), PChar(res_main_file_exist2), MB_YESNO) = IDNO then
        Result := false;
    end;
end;

procedure TMainForm.cmdCancelEditClick(Sender: TObject);
begin
  CancelSymbolChanges;
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
var
  ext: string;
  bitmap: TBitmap;
  jpgimage : TJPegImage;
  metafile: TMetafile;
begin
  HideCrosshair;
  
  SaveDialog.FileName := ChangeFileExt(ExtractFileName(CurrentFileName),'');
  repeat
    if not SaveDialog.Execute then exit;
  until AlrightToSaveTo(SaveDialog.Filename);

  ext := Uppercase(ExtractFileExt(SaveDialog.Filename));
  if (ext = '.BMP') then
    begin
      BitmapPropertyDlg.JPEGSettings.Visible := false;
      if BitmapPropertyDlg.ShowModal = mrOK then begin
        try
          bitmap := Map.GetBitmap(true, BitmapProperties.UseAreaCurrent,
                                  BitmapProperties.BitmapWidth, BitmapProperties.BitmapHeight);
          bitmap.SaveToFile(SaveDialog.Filename);
          bitmap.Free;
        except
          Application.MessageBox(PChar(res_main_big_bitmap), PChar(res_main_big_bitmap_title), MB_OK);
          end;
        end;
    end
  else
    if (ext = '.JPG') then
      begin
        BitmapPropertyDlg.JPEGSettings.Visible := true;
        if BitmapPropertyDlg.ShowModal = mrOK then begin
          try
            bitmap := Map.GetBitmap(true, BitmapProperties.UseAreaCurrent,
                                BitmapProperties.BitmapWidth, BitmapProperties.BitmapHeight);
            jpgimage := TJPegImage.Create;
            jpgimage.CompressionQuality := BitmapPropertyDlg.JPEGQuality.Position;
            jpgimage.ProgressiveEncoding := BitmapPropertyDlg.JPEGProgressiveEncoding.Checked;
            jpgimage.Smoothing := BitmapPropertyDlg.JPEGImageSmoothing.Checked;
            jpgimage.assign(bitmap);
            jpgimage.SaveToFile(SaveDialog.Filename);
            jpgimage.free;
            bitmap.Free;
          except
            Application.MessageBox(PChar(res_main_big_bitmap), PChar(res_main_big_bitmap_title), MB_OK);
            end;
          end;
      end
  else
    if (ext = '.WMF') or (ext = '.EMF') then
      begin
        metafile := Map.GetMetafile(true);
        metafile.Enhanced := (ext = '.EMF');
        metafile.SaveToFile(SaveDialog.Filename);
        metafile.Free;
      end
    else
      if (ext = '.AUR') Or (ext = '.AURX') then
        begin
          CurrentFilename := SaveDialog.Filename;
          Caption := res_application_title + ' - ' + CurrentFilename;
          SaveMap(SaveDialog.Filename);
        end
      else
        begin { Backup file or weird extension }
          SaveMap(SaveDialog.Filename);
        end;
end;

function TMainForm.ModifiedHasBeenSaved: boolean;
var
  s: string;
begin
  Result := true;

  if Map.Modified then
    begin
      s := res_main_file_changes + #13#10 + Map.ModifiedDescription;
      case Application.MessageBox(PChar(s), PChar(res_main_file_changes2), MB_YESNOCANCEL) of
        IDYES: aSaveExecute(self);
        IDNO:
          begin
          end;
        IDCANCEL: Result := false;
      end;
    end;
end;

procedure TMainForm.FileNewMenuItemClick(Sender: TObject);
var
  oldcursor : TCursor;
begin
  if ModifiedHasBeenSaved then
    begin
      oldcursor := Screen.Cursor;    // Show an hourglass
      Screen.Cursor := crHourglass;  // while we're working
      Map.Clear;
      Map.RemoveAllViews;
      BuildViewList;
      ClearPushPins;
      Caption := res_application_title;
      CurrentFilename := '';
      // 2003/05/22 - J.Friant
      // Check if the user wants to reset the zoom, graph,
      // and color values so that he can start from a
      // clean slate.
      if Application.MessageBox(PChar('Do you want to keep the previous map''s settings?'), PChar('New Map'), MB_YESNO) = IDNO then
      begin
        //
        // Reset the default colors
        BackgroundColor.Color := clWhite;
        Color := BackgroundColor.Color;
        GridColor.Color := clAqua;
        CurrentGridColor := GridColor.Color;
        //
        // Set the grid back
        //
        SetMapGrid(SquareGraphBtn.Hint, gtSquare);
        //
        // Set the zoom to 100%
        //
        Map.CurrentView.SetZoomPercent(100);
        Map.CurrentView.Grid.GridPosition := 0;
        //
        // Reset the overlays to the default
        //
        SetDefaultOverlays;
        //
        // Reset the Icon size and density settings
        //
        IconSizeBar.Position := 48;
        DensityBar.Position := 80;
        // Now we could also reset the following (but since
        // those are user selected, it may be fine to leave
        // them alone):
        //        LineStyleComboBox
        //        MainColor
        //        FillColor
        //        FillPattern
        //        OutlineColor
      end;
      Map.ClearModified;
      Screen.Cursor := oldcursor;
    end;
end;

procedure TMainForm.SetDefaultOverlays();
begin
  // 2003/12/01 - J.Friant
  // This function is called by FileNewClick to reset the
  // list of overlays to the default.  Really the list of
  // default overlays should be in a resource array
  // somewhere, but that's on the to-do list for now. 
  //
  While OverlayList.Items.Count > 0 do
    DeleteOverlay(0);

  AddOverlay('Design');
  AddOverlay('Geographic');
  AddOverlay('Topographic');
  AddOverlay('Political');
  AddOverlay('Settlement');
  AddOverlay('Racial');
  AddOverlay('Treasure');
  AddOverlay('Secret');
  AddOverlay('Notes');
  AddOverlay('Legend');

  ActiveOverlay.ItemIndex := 0;
  OverlayList.ItemIndex := 0;
end;

procedure TMainForm.SelectAll1Click(Sender: TObject);
begin
  Map.SelectAll;
  ToolDispatcher.Refresh;
  DoInvalidate;
end;

procedure TMainForm.ZoomInMenuItemClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  p := ScreenToClient(p);
  ZoomHandler.Zoom(true, p.x, p.y);
end;

procedure TMainForm.ZoomOutMenuItemClick(Sender: TObject);
var
  p: TPoint;
begin
  GetCursorPos(p);
  p := ScreenToClient(p);
  ZoomHandler.Zoom(false, p.x, p.y);
end;

procedure TMainForm.ShowAllMenuItemClick(Sender: TObject);
begin
  Map.ShowAll(Map.CurrentView, 0.5);
  DoInvalidate;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  HideCrosshair;

  // create the about box as needed to conserve resources.
  Application.CreateForm(TAboutForm, AboutForm);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

procedure TMainForm.BackgroundColorChange(Sender: TObject);
begin
  if (Color <> BackgroundColor.Color) then
    begin
      Map.SetUndoPoint(res_main_color_change_backgd);
      HideCrosshair;
      Color := BackgroundColor.Color;
      DoInvalidate;
      Map.SetModified(modBkColor);
    end;
end;

procedure TMainForm.GridColorChange(Sender: TObject);
begin
  if (CurrentGridColor <> GridColor.Color) then
    begin
      Map.SetUndoPoint(res_main_color_change_grid);
      HideCrosshair;
      CurrentGridColor := GridColor.Color;
      DoInvalidate;
      Map.SetModified(modGrid);
    end;
end;


procedure TMainForm.FontToolbar1Click(Sender: TObject);
begin
  ChooseFont.Visible := not ChooseFont.Visible;
  TextToolbarItem.Checked := ChooseFont.Visible;
end;

procedure TMainForm.ChartGridBtnClick(Sender: TObject);
begin
  if ChartGridHandler.AskGrid then
    begin
      SwitchToolDispatcher(20, ChartGridHandler);
    end
  else
    aSelectionExecute(Sender);
end;

procedure TMainForm.MapSettingsClick(Sender: TObject);
var
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  Map.CurrentView.Grid.SaveToStream(m, CURRENT_MAP_VERSION);

  HideCrosshair;
  if MapSettingsDialog.ShowModal = mrCancel then
    begin
      m.Position := 0;
      Map.CurrentView.Grid.LoadFromStream(m, CURRENT_MAP_VERSION);
      Map.ClearModified(modUnitScale);
      Map.ClearModified(modUnitType);
      Map.ClearModified(modComments);
      Map.ClearModified(modSettings);
    end
  else
    DoInvalidate;

  m.Free;
end;

procedure TMainForm.TextToolbarItemClick(Sender: TObject);
begin
  ChooseFont.Visible := not ChooseFont.Visible;
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
begin
  HideCrosshair;
  if Settings.ShowModal = mrOK then
    begin
      Settings.Save;
      Map.UndoLevels := Settings.UndoLevels.Value;

      if Settings.ColorCoded.Checked then
        OverlayList.Images := OverlayColors
      else
        OverlayList.Images := OverlayImages;

      OverlayList.Repaint;
      DoInvalidate;
    end
  else
    Settings.Load;
end;

procedure TMainForm.UpdateOverlayImages(os: OverlaySet);
var
  i: integer;
begin
  if Settings.ColorCoded.Checked then
    OverlayList.Images := OverlayColors
  else
    OverlayList.Images := OverlayImages;

  for i := 0 to OverlayList.Items.Count - 1 do
    begin
      if i in os then
        OverlayList.ImageIndex[i] := i mod OverlayImages.Count
      else
        OverlayList.ImageIndex[i] := -1;
    end;

  PopulatedOverlays := os;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  if ModifiedHasBeenSaved then Application.Terminate;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ModifiedHasBeenSaved;
end;

procedure TMainForm.PrintSetup1Click(Sender: TObject);
var
  OldSetting: TPrinterOrientation;
begin
  HideCrosshair;
  OldSetting := Printer.Orientation;
  PrinterSetupDialog.Execute;
  Map.Landscape := (Printer.Orientation = poLandscape);
  Printer.Orientation := OldSetting;
end;

procedure TMainForm.PrintMap(usedefaults: boolean);
var
  oldcursor: TCursor;
  MapView, DecomposeView: ViewPoint;
  StartCoord, CurrentCoord: CoordRect;
  i, j: integer;
  OldColor: TColor;
  horz, vert: integer;
  OldSetting: TPrinterOrientation;
  center: CoordPoint;
begin
  OldSetting := Printer.Orientation;
  if Map.Landscape then
    Printer.Orientation := poLandscape
  else
    Printer.Orientation := poPortrait;

  if CurrentFilename = '' then
    Printer.Title := res_main_print_new_file
  else
    Printer.Title := res_application_title + ' : ' + CurrentFilename;

  if usedefaults then
    begin
      MapView := ViewPoint.Create(Map.CurrentView, true);
      horz := 1;
      vert := 1;
    end
  else
    begin
      MapView := ViewPoint.Create(CustomPrintDialog.ViewRect,
        Printer.PageWidth, Printer.PageHeight,
        Map.CurrentView.Grid);
      horz := CustomPrintDialog.TileHeight;
      vert := CustomPrintDialog.TileWidth;
      MapView.VisibleOverlays := CustomPrintDialog.VisibleOverlays;
    end;

  MapView.Canvas := Printer.Canvas;
  MapView.GetCoordinateRect(StartCoord);

  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;

  if Settings.DecomposePrint.Checked then
    begin
      DecomposeView := ViewPoint.Create(CustomPrintDialog.ViewRect,
        Printer.PageWidth, Printer.PageHeight,
        Map.CurrentView.Grid);
      DecomposeView.Canvas := PaintBox.Canvas; (*!!*)
      // Zoom closer to make the decompose have finer detail
      Center := AvePoints(CustomPrintDialog.ViewRect.TopLeft,
        CustomPrintDialog.ViewRect.BottomRight);
      DecomposeView.Zoom(Center, 0.1);
      Map.SetUndoPoint('');
      Map.DecomposeForPrintout(DecomposeView);
      DecomposeView.Free;
    end;


  Printer.BeginDoc;

  for j := 0 to horz - 1 do
    begin
      CurrentCoord.Top := StartCoord.Top + j * (StartCoord.Bottom - StartCoord.Top);
      CurrentCoord.Bottom := StartCoord.Bottom + j * (StartCoord.Bottom - StartCoord.Top);

      for i := 0 to vert - 1 do
        begin
          CurrentCoord.Left := StartCoord.Left + i * (StartCoord.Right - StartCoord.Left);
          CurrentCoord.Right := StartCoord.Right + i * (StartCoord.Right - StartCoord.Left);

          MapView.SetCoordinateRect(CurrentCoord);

          if (i <> 0) or (j <> 0) then Printer.NewPage;

          { Fill background color }
          OldColor := Printer.Canvas.Brush.Color;
          Printer.Canvas.Brush.Color := Color;
          Printer.Canvas.FillRect(Printer.Canvas.ClipRect);
          Printer.Canvas.Brush.Color := OldColor;

          { Draw the map }
          Map.Draw(MapView, false);

        end;
    end;

  if Settings.DecomposePrint.Checked then Map.Undo;

  Printer.EndDoc;

  MapView.Free;

  Screen.Cursor := oldcursor;
  Printer.Orientation := OldSetting;
end;

procedure TMainForm.Print1Click(Sender: TObject);
begin
  HideCrosshair;
  if CustomPrintDialog.ShowModal = mrOk then
    begin
      PrintMap(false);
    end;
end;

procedure TMainForm.BeginLineStyleChange(Sender: TObject);
var
  style: StyleAttrib;
begin
  if (BeginLineStyle.ItemIndex <> -1) then
    begin
      if not Settings.FreezeProperties.Checked then
        begin
          style.bits := $FFFFFFFF;
          style.First := BeginLineStyle.ItemIndex;
          Style.FullStyle.Thickness := -1;
          if Map.SetStyle(style) then RefreshExtent;
        end;
    end;
end;

procedure TMainForm.EndLineStyleChange(Sender: TObject);
var
  style: StyleAttrib;
begin
  if (EndLineStyle.ItemIndex <> -1) then
    begin
      if not Settings.FreezeProperties.Checked then
        begin
          style.bits := $FFFFFFFF;
          style.Last := EndLineStyle.ItemIndex;
          Style.FullStyle.Thickness := -1;
          if Map.SetStyle(style) then RefreshExtent;
        end;
    end;
end;

procedure TMainForm.FillPatternChange(Sender: TObject);
var
  style: StyleAttrib;
begin
  if (FillPattern.Pattern <> -1) then
    begin
      if not Settings.FreezeProperties.Checked then
        begin
          style.bits := $FFFFFFFF;
          style.Fill := FillPattern.Pattern;
          Style.FullStyle.Thickness := -1;
          if Map.SetStyle(style) then RefreshExtent;
        end;
    end;
end;

procedure TMainForm.MeasurementMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    begin
      Map.CurrentView.Grid.SetMeasurementUnits((Sender as TMenuItem).Tag);
    end;
end;

procedure TMainForm.WMQueryNewPalette(var msg: TMessage);
begin
  if IsPaletteDevice and (HPal <> 0) then
    begin
      SelectPalette(PaintBox.Canvas.Handle, HPal, true);
      UnrealizeObject(HPal);
      RealizePalette(PaintBox.Canvas.Handle);
    end;
end;

procedure TMainForm.WMPaletteChanged(var msg: TMessage);
begin
  if IsPaletteDevice then
    begin
      UpdateColors(PaintBox.Canvas.Handle);
    end;
end;

procedure TMainForm.WMDropFiles(var msg: TMessage);
var
  lpszFile: array[0..MAX_PATH] of char;
  cFiles: integer;
  qfh: THandle;
  i: integer;
  s: string;
  cx,cy:Coord;

begin
  qfh := msg.WParam;
  cFiles := DragQueryFile(qfh, $FFFFFFFF, nil, 0);

  if Map.ReadOnly then begin
    FlashReadOnlyProhibits;
    end
  else begin
    Map.CurrentView.ScreenToCoord(Mouse.CursorPos.X,Mouse.CursorPos.Y,CX,CY);

//    Map.CurrentView.ScreenToCoord(Mouse.CursorPos.X,Mouse.CursorPos.Y, cx,cy);
    ApplySnaps(false,cx,cy);

    Map.StartAdding(res_main_hyperlink_drop);

    for i := 0 to cFiles - 1 do
      begin
        DragQueryFile(qfh, i, lpszFile, sizeof(lpszFile));

        s := lpszFile;

        // Convert to relative path name if possible, and make sure filename is
        // double-quoted.
        s:='"' + ExtractRelativePath(ExtractFilePath(CurrentFilename),s) + '"';

        Map.AddObject(HyperlinkPrimitive.Create(cx,cy, s, [hyperExecute]));
        cy := cy + HyperlinkBullet.Height;
      end;

    Map.EndAdding;
    // Clear the selection so that changes to the hyperlink dialog don't hose
    // up our just placed links.
    Map.ClearSelection;
    end;

  DragFinish(qfh);
end;

procedure TMainForm.ClearPushPins;
var
  i: integer;
begin
  // If they close the toolbar, don't try reading from it.
  if PushPinList <> nil then
    begin
      for i := 0 to PushPinList.Items.Count - 1 do begin
        Map.PushPinHistoryClear(i);
        end;
      PushPinList.Invalidate;
    end;
end;

procedure TMainForm.PushPinMenuItemClick(Sender: TObject);
var
  cx, cy: Coord;
  pt, oldpt: CoordPoint;
  index: integer;
  pinx, piny: integer;
  rect: TRect;
begin
  index := (Sender as TMenuItem).ImageIndex;

  // push pin not found?
  if index=-1 then exit;

  Map.CurrentView.ScreenToCoord(PopupX, PopupY+1, cx, cy);
  pt.X := cx;
  pt.Y := cy;

  if Map.PushPinPlaced(index) then
    begin
      // Invalidate the map where the old pushpin was
      oldpt := Map.PushPinPoint[index];

      Map.CurrentView.CoordToScreen(oldpt.X, oldpt.Y, pinx, piny);
      rect.Left := pinX;
      rect.Right := pinX + PushPinImages.Width;
      rect.Bottom := pinY;
      rect.Top := pinY - PushPinImages.Height;
      InvalidateRect(MainForm.Handle, @rect, true);
    end;

  // Attempt to assign the new pushpin.  We may not be able to
  // because the map is read-only.
  Map.PushPinPoint[index] := pt;

  // If we were successful, then go ahead and check the pushpin in the list
  // so we show the distance.
  if Map.PushPinPlaced(index) then begin
    PushPinList.Checked[index] := true;
    RefreshPushPins;
    end;
end;

procedure TMainForm.InvalidatePushPin(idx:integer);
var rect: TRect;
  pt: CoordPoint;
  pinx, piny: integer;
  s:string;
  i:integer;
  textw,texth:integer;
begin
  ToolDispatcher.HideCrosshair;
  // Invalidate all the pushpins (waypoints and the primary) shown
  // on the map.  When we redraw, they will reflect our current
  // checked state.
  PaintBox.Canvas.Font := AnnotateFont;

  for i:=0 to Map.PushPinHistoryCount[idx]-1 do begin
    pt := Map.PushPinHistoryPoint[idx,i];
    s := Map.PushPinAnnotation[idx,i];

    Map.CurrentView.CoordToScreen(pt.X, pt.Y, pinx, piny);
    GetPopupSize(PaintBox.Canvas, s, textw,texth);

    rect.Left := pinX;
    rect.Right := pinX + PushPinImages.Width + textw;
    rect.Top := pinY - PushPinImages.Height;
    rect.Bottom := rect.Top + texth;
    // We invalidate either to the bottom of the pin, or the bottom of the text,
    // whichever is lower.
    if (rect.Bottom < pinY) then rect.Bottom:=pinY;

    InvalidateRect(MainForm.Handle, @rect, true);
    end;
end;

procedure TMainForm.InvalidatePushPins;
var i:integer;
begin
  for i:=0 to Map.PushPinCount-1 do begin
    InvalidatePushPin(i);
    end;
end;

procedure TMainForm.PushPinListClickCheck(Sender: TObject);
var
  idx: integer;
begin
  idx:=PushPinList.ItemIndex;
  if (idx<0) then exit;

  if (not Map.PushPinPlaced(idx)) then
    begin
      if PushPinList.Checked[idx] then
        begin
          ShowMessage(res_main_pushpin);
          PushPinList.Checked[idx] := false;
        end;
    end
  else
    InvalidatePushPin(idx);
end;

procedure TMainForm.RefreshPushPins;
var
  i,j,n: integer;
  pt: CoordPoint;
  pinx, piny: integer;
  s:string;
begin
  if PushPinList <> nil then
    begin
      PaintBox.Canvas.Font := AnnotateFont;
      for i := 0 to PushPinList.Items.Count - 1 do
        begin
          if Map.PushPinPlaced(i) and PushPinList.Checked[i] then begin
             // If showing waypoints, then show all the push pins.
             if PP_WaypointsVisible in Map.PushPinFlags then
               n:= Map.PushPinHistoryCount[i] - 1
             else
               n:= 0;

             for j:=0 to n do begin
                pt := Map.PushPinHistoryPoint[i,j];
                s := Map.PushPinAnnotation[i,j];
                Map.CurrentView.CoordToScreen(pt.X, pt.Y, pinx, piny);
                PushPinImages.Draw(PaintBox.Canvas, pinx, piny - PushPinImages.Height, i);

                // Show annotation (waypoint and/or note) next to push pin.
                if (s<>'') then begin
                  ShowPopupBox(PaintBox.Canvas,
                               pinx + PushPinImages.Width,
                               piny - PushPinImages.Height, s);
                  end;
                end;
            end;
        end;
    end;
end;

procedure TMainForm.RefreshPushPinDistances(x, y: integer);
var
  i: integer;
  r: TRect;
begin
  Map.CurrentView.ScreenToCoord(x, y, CursorX, CursorY);
  //  ApplySnaps(cursorx,cursory);

  if PushPinList <> nil then
    begin
      for i := 0 to PushPinList.Items.Count - 1 do
        begin
          if Map.PushPinPlaced(i) and PushPinList.Checked[i] then
            begin
              r.Left := 15;
              r.Right := PushPinList.Width - r.Left - 1;

              // If enough pushpins are off the screen, the scroll bar will
              // be showing.  Adjust the clip rectangle to ignore the scroll bar
              // area on the listbox's right.
              if (PushPinList.Items.Count*PushPinList.ItemHeight > PushPinList.Height) then begin
                r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL);
                end;
              r.Top := (i - PushPinList.TopIndex) * PushPinList.ItemHeight;
              r.Bottom := r.Top + PushPinList.ItemHeight;
              PushPinListDrawItem(PushPinList, i, r, [odChecked]);
            end;
        end;
    end;
end;

procedure TMainForm.PushPinListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  pt: CoordPoint;
  a, d: double;
  s: string;
begin
  PushPinList.OnDrawItem := nil;
  PushPinList.DrawItem(index, Rect, State);

  if Map.PushPinPlaced(index) and PushPinList.Checked[index] then
    begin
      pt := Map.PushPinPoint[index];
      d := Distance(pt.X, pt.Y, cursorx, cursory);
      a := Angle(pt.X, pt.Y, cursorx, cursory);
      s := Format('%.1f�   %f %s', [a, (Map.CurrentView.Grid.GraphUnitConvert *
          Map.CurrentView.Grid.GraphScale) * d,
        Map.CurrentView.Grid.GraphUnits]);
    end
  else
    s := '';

  PushPinList.Canvas.Font.Color := clWindowText;
  PushPinList.Canvas.Brush.Color := clWindow;
  PushPinList.Canvas.FillRect(Rect);
  PushPinList.Canvas.TextRect(Rect, Rect.Left, Rect.Top, s);

  PushPinList.OnDrawItem := PushPinListDrawItem;
end;


procedure TMainForm.OpenFigureMenuItemClick(Sender: TObject);
begin
  ToolDispatcher.CreateOpenFigure;
end;

procedure TMainForm.ClosedFigureMenuItemClick(Sender: TObject);
begin
  ToolDispatcher.CreateClosedFigure;
end;

procedure TMainForm.SuppressMenuClick(Sender: TObject);
begin
  ToolDispatcher.CreateOpenFigure;
  Settings.AskForShapeClosure.Checked := false;
end;

procedure TMainForm.PanTimerTimer(Sender: TObject);
var
  pt: TPoint;
begin
  pt := ScreenToClient(Mouse.CursorPos);
  ToolDispatcher.MouseMove([], pt.X, pt.Y);
end;

procedure TMainForm.RestoreDefaultToolbarsClick(Sender: TObject);
begin
  TBRegLoadPositions(Self, HKEY_CURRENT_USER, 'Software\'+Application.Title + '\Defaults');
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveToolbarPositions;
end;

procedure TMainForm.RestoreToolbarPositions;
begin
  TBRegLoadPositions(Self, HKEY_CURRENT_USER, 'Software\'+Application.Title);
end;

procedure TMainForm.SaveToolbarPositions;
begin
  TBRegSavePositions(Self, HKEY_CURRENT_USER, 'Software\'+Application.Title);
end;

procedure TMainForm.BuildViewList;
var
  i, n, col: integer;
  NewItem: TTBCustomItem;
  count: integer;
  nextitem: integer;
  tmpView: ViewPoint;
  s: string;
begin
  with tbRecentMainMenu do
    begin
      col := Items.IndexOf(ViewMenu);
      if (col >= 0) then
        begin
          n := Items[col].Count - 1;
          while not(Items[col].Items[n] is TTBSeparatorItem) do
            begin
              Items[col].Delete(n);
              dec(n);
            end;

          count := Map.GetViewpoints;
          Items[col].Items[n].Visible := (Count <> 0);

          i := Map.FindViewPoint('');
          if i <> -1 then
            begin
              NewItem := TTBCustomItem.Create(Self);
              NewItem.Caption := '&0 ' + LastViewName;
              NewItem.Hint := res_main_view_saved;
              NewItem.OnClick := ViewPointClick;
              NewItem.Tag := i;
              Items[col].Add(NewItem);
            end;
          nextitem := 1;

          for i := 0 to Count - 1 do
            begin
              tmpView := Map.GetViewPoint(i);

              if (tmpView.Name <> '') then
                begin
                  NewItem := TTBCustomItem.Create(Self);
                  s := IntToStr(nextitem);
                  s := copy(s, 1, length(s) - 1) + '&' + copy(s, length(s), 1);
                  NewItem.Caption := s + ' ' + StringReplace(tmpView.Name, '&', '&&', [rfReplaceAll]);
                  NewItem.Hint := res_main_view_saved;
                  NewItem.OnClick := ViewPointClick;
                  //if ((nextitem + 5) mod 23) = 22 then NewItem.Break := mbBarBreak;       // zifnabbe 2002/08/18
                  NewItem.Tag := i;
                  Items[col].Add(NewItem);
                  inc(nextitem);
                end;
            end;
        end;
    end;
end;

procedure TMainForm.ViewPointClick(Sender: TObject);
var
  view: ViewPoint;
begin
  if (Sender is TTBCustomItem) then
    begin
      view := Map.GetViewPoint(TMenuItem(Sender).Tag);
      if view <> nil then
        begin
          Map.SetCurrentViewPoint(view);
          Map.SetModified(modViewport);
          DoInvalidate;
        end;
    end;
end;

procedure TMainForm.SaveCurrentViewClick(Sender: TObject);
var
  i: integer;
  view: ViewPoint;
begin
  Application.CreateForm(TSaveViewForm, SaveViewForm);
  SaveViewForm.ViewCombo.Items.Clear;

  for i := 0 to Map.GetViewpoints - 1 do
    begin
      view := Map.GetViewPoint(i);
      if (view.Name <> '') then
        SaveViewForm.ViewCombo.Items.Add(View.Name)
      else
        SaveViewForm.ViewCombo.Items.Add(LastViewName);
    end;

  HideCrosshair;
  if SaveViewForm.ShowModal = mrOk then
    begin
      Map.SetUndoPoint(res_main_view_save);
      Map.SaveViewPoint(Map.GetCurrentViewPoint, SaveViewForm.ViewCombo.Text);
      Map.SetModified(modViews);
      BuildViewList;
    end;

  SaveViewForm.Free;
end;

procedure TMainForm.DeleteViewClick(Sender: TObject);
var
  i, index: integer;
  view: ViewPoint;
begin
  Application.CreateForm(TDeleteViewForm, DeleteViewForm);

  DeleteViewForm.ViewListBox.Items.Clear;

  for i := 0 to Map.GetViewpoints - 1 do
    begin
      view := Map.GetViewPoint(i);
      if (view.Name <> '') then
        DeleteViewForm.ViewListBox.Items.Add(View.Name)
      else
        DeleteViewForm.ViewListBox.Items.Add(LastViewName);
    end;

  HideCrosshair;
  if DeleteViewForm.ShowModal = mrOk then
    begin
      index := DeleteViewForm.ViewListBox.ItemIndex;
      if index <> -1 then
        begin
          Map.SetUndoPoint(res_main_view_delete);
          Map.DeleteViewPoint(index);
          BuildViewList;
        end;
    end;

  DeleteViewForm.Free;
end;

procedure TMainForm.ContentsMenuItemClick(Sender: TObject);
begin
  Application.HelpJump('Contents');
end;

procedure TMainForm.GettingStarted1Click(Sender: TObject);
begin
  Application.HelpJump('Getting_Started');
end;

procedure TMainForm.Search1Click(Sender: TObject);
begin
  Application.HelpCommand(HELP_PARTIALKEY, Integer(PChar('')));
end;

procedure TMainForm.HowDoIMenuItemClick(Sender: TObject);
begin
  Application.HelpJump('How_Do_I');
end;

procedure TMainForm.ReadMeMenuItemClick(Sender: TObject);
var
  s: string;
begin
  s := 'write "' + ExtractFilePath(ParamStr(0)) + res_main_readme + '"';
  WinExec(PChar(s), SW_SHOWNORMAL);
end;

procedure TMainForm.ArrayMenuItemClick(Sender: TObject);
var
  x, y    : Coord;
  hc, vc  : integer;
  valid   : boolean;
  Ellipse : Boolean;  // JD 7-30-02
  Rotate  : Boolean;
  ex      : Coord;
  ey      : Coord;

begin
  Application.CreateForm(TArrayForm, ArrayForm);

  repeat
    try
      valid := true;
      HideCrosshair;

      if ArrayForm.ShowModal = mrOk then
        with ArrayForm do
          begin
            hc := HorzCount.Value;
            vc := VertCount.Value;
            x  := StrToFloat(HorzSpace.Text);
            y  := StrToFloat(VertSpace.Text);
            x  := Map.CurrentView.Grid.Convert(x, HorzUnitCombo.ItemIndex);
            y  := Map.CurrentView.Grid.Convert(y, VertUnitCombo.ItemIndex);

            // JD 7-30-02

            ex := StrToFloat(edtHRadius.Text);
            ey := StrToFloat(edtVRadius.Text);
            ex := Map.CurrentView.Grid.Convert(ex, cbEllipseHUnits.ItemIndex);
            ey := Map.CurrentView.Grid.Convert(ey, cbEllipseVUnits.ItemIndex);
            Ellipse := cbEllipse.Checked;
            Rotate  := cbRotate.Checked;
            Map.CreateArray(hc, vc, x, y, HorzBetween.Checked, VertBetween.Checked,
                            Ellipse,Rotate,ex,ey);
          end;
    except
      MessageBeep(MB_OK);
      valid := false;
    end;
  until valid;

  ArrayForm.Free;
end;

procedure TMainForm.CloseFigureMenuItemClick(Sender: TObject);
begin
  if Map.AnythingSelected then
    begin
      Map.SetUndoPoint(res_main_figure_close);
      HideCrosshair;
      Map.CloseSelectedFigures;
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    end;
end;

procedure TMainForm.ReverseLineDirection1Click(Sender: TObject);
begin
  if Map.AnythingSelected then
    begin
      Map.SetUndoPoint(res_main_line_reverse);
      HideCrosshair;
      Map.ReverseSelected(False);
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    end;
end;

procedure TMainForm.Autoname1Click(Sender: TObject);
begin
  HideCrosshair;
  if AutoNameDialog.Visible then
    AutoNameDialog.Hide
  else
    AutoNameDialog.Show;
end;

procedure TMainForm.DisableCrosshair(Sender: TObject);
begin
  HideCrosshair;
end;

procedure TMainForm.WMNCLButtonDown(var msg: TMessage);
begin
  // Fix the irritating bug in the Delphi Window manager that
  // allows a maximized window to be repositioned by dragging it.
  // (Note that the Delphi 4 IDE has the same stupid bug!)
  //
  // We just ignore clicks inside the caption if the window is
  // maximized.  Any other non-client area we process like normal.

  if (msg.wParam = HTCAPTION) and (WindowState = wsMaximized) then
    begin
      exit;
    end;

  DefWindowProc(MainForm.Handle, msg.Msg, msg.WParam, msg.LParam);
end;


// Support for delayed clipboard rendering.
// Please refer to MapObject.Copy for more details.

procedure TMainForm.WMRenderFormat(var msg: TMessage);
var
  Data: THandle;
  Bitmap: TBitmap;
  Metafile: TMetaFile;
  Palette: HPALETTE;
  Format: Word;
  rect:TRect;
begin
  Bitmap := nil;
  Metafile := nil;
  try
    // Delayed clipboard rendering cannot use the Delphi
    // Clipboard object, since we only want to call
    // SetClipboardData, but not OpenClipboard.
    case msg.WParam of
      CF_BITMAP:
        begin
          rect:=Map.Extent(Map.CurrentView,false);
          Bitmap := Map.GetBitmap(false, true, rect.right-rect.left, rect.bottom-rect.top);
          Palette := 0;
          Format := CF_BITMAP;
          Bitmap.SaveToClipboardFormat(Format, Data, Palette);
          SetClipboardData(Format, Data);
          if Palette <> 0 then SetClipboardData(CF_PALETTE, Palette);
        end;
      CF_METAFILEPICT:
        begin
          metafile := Map.GetMetafile(false);
          Palette := 0;
          Format := CF_METAFILEPICT;
          metafile.SaveToClipboardFormat(Format, Data, Palette);
          SetClipboardData(Format, Data);
          if Palette <> 0 then SetClipboardData(CF_PALETTE, Palette);
        end;
    end;

  finally
    Bitmap.Free;
    Metafile.Free;
  end;
end;


// Support for delayed clipboard rendering.
// Please refer to MapObject.Copy for more details.

procedure TMainForm.WMRenderAllFormats(var msg: TMessage);
begin
  // At this stage in the game, we've already freed our map.
  // I'm being extremely lazy here, but if they've copyed delayed
  // rendered formats to the clipboard and close down AutoREALM,
  // we're not going to make those formats available.
  //
  // Failure to call our SetClipboardData function for the
  // formats we posted means that Windows will assume those
  // formats are no longer available, which is fine by us.
  //
  // If you do want to do preserve the delayed formats after
  // AutoREALM's termination, you'll need to make sure we haven't
  // freed up the map before this message is called and uncomment
  // out the below code.

  // // We just "cheat", and call our own RenderFormat message
  // // for our supported formats.
  // msg.WParam := CF_BITMAP;
  // WMRenderFormat(msg);
  //
  // msg.WParam := CF_METAFILEPICT;
  // WMRenderFormat(msg);
end;

procedure TMainForm.PushPinListClick(Sender: TObject);
begin
  // When a new push pin is selected:
  // Update the checkbox list's fly-over hint
  PushPinList.Hint := StripHotKey(Map.PushPinName[PushPinList.ItemIndex]);
  // Update the right-click menu's "caption": the empty menu item with
  // the name and pushpin graphic.
  PushPointRightClickName.Caption := PushPinList.Hint;
  PushPointRightClickName.ImageIndex := PushPinList.ItemIndex;
end;

procedure TMainForm.PushPinWaypointMenuClick(Sender: TObject);
var pnote:string;
begin
  if (PushPinList.ItemIndex<0) then begin
    Application.MessageBox(PChar(res_pushpinselecterror),
                           PChar(res_pushpinselectcaption), MB_OK);
    exit;
  end;

  // Require the push pin to be placed before we can convert it into
  // a way point.
  if (not Map.PushPinPlaced(PushPinList.ItemIndex)) then begin
    Application.MessageBox(PChar(res_pushpinwaypointerror),
                           PChar(res_pushpinwaypointcaption), MB_OK);
    exit;
  end;

  // Get the note, and use the current push pin location as the position.
  pnote:='';
  if InputQuery(res_pushpinwaypointcaption, res_pushpinwaypointprompt, pnote) then begin
    Map.PushPinHistoryNote[PushPinList.ItemIndex,0] := pnote;
    Map.PushPinHistoryPointAdd(PushPinList.ItemIndex, Map.PushPinPoint[PushPinList.ItemIndex], '');
  end;

  InvalidatePushPin(PushPinList.ItemIndex);
end;

procedure TMainForm.PushPinHistoryClearMenuClick(Sender: TObject);
var s:string;
begin
  if (PushPinList.ItemIndex<0) then begin
    Application.MessageBox(PChar(res_pushpinselecterror),
                           PChar(res_pushpinselectcaption), MB_OK);
    exit;
  end;

  s:=Format(res_pushpinclearcaption, [StripHotKey(Map.PushPinName[PushPinList.ItemIndex])] );
  if (Application.MessageBox(PChar(s),
                             PChar(res_pushpinclearprompt),
                             MB_YESNO) = IDYES) then begin
    Map.PushPinHistoryClear(PushPinList.ItemIndex);
    Map.Invalidate;
  end;
end;

procedure TMainForm.PushpinRenameMenuClick(Sender: TObject);
var pname:string;
begin
  if (PushPinList.ItemIndex<0) then begin
    Application.MessageBox(PChar(res_pushpinselecterror),
                           PChar(res_pushpinselectcaption), MB_OK);
    exit;
  end;

  pname:=Map.PushPinName[PushPinList.ItemIndex];
  if InputQuery(res_pushpinnamecaption, res_pushpinnameprompt, pname) then begin
    Map.PushPinName[PushPinList.ItemIndex] := pname;
    // Rebuild the menu with the new name
    RefreshPushPinMenu;
  end;
end;

procedure TMainForm.PushpinExportMenuClick(Sender: TObject);
begin
  Clipboard.AsText := Map.PushPinText;
end;

procedure TMainForm.PushpinImportMenuClick(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_TEXT) then begin
    InvalidatePushPins;
    try
      Map.PushPinText := Clipboard.AsText;
    except
      on e:Exception do begin
        Application.MessageBox(PChar(e.Message), PChar(res_PushPinPaste), MB_OK);
        end;
      end;
    InvalidatePushPins;
    end;
end;

procedure TMainForm.MakeWaypointsVisibleMenuClick(Sender: TObject);
begin
  InvalidatePushPins;

  if (PP_WaypointsVisible in Map.PushPinFlags) then
    Map.PushPinFlags := Map.PushPinFlags - [PP_WaypointsVisible]
  else
    Map.PushPinFlags := Map.PushPinFlags + [PP_WaypointsVisible];

  InvalidatePushPins;
end;

procedure TMainForm.PushPinShowNumberMenuClick(Sender: TObject);
begin
  InvalidatePushPins;

  if (PP_ShowNumber in Map.PushPinFlags) then
    Map.PushPinFlags := Map.PushPinFlags - [PP_ShowNumber]
  else
    Map.PushPinFlags := Map.PushPinFlags + [PP_ShowNumber];

  InvalidatePushPins;
end;

procedure TMainForm.PushPinShowNoteMenuClick(Sender: TObject);
begin
  InvalidatePushPins;

  if (PP_ShowNote in Map.PushPinFlags) then
    Map.PushPinFlags := Map.PushPinFlags - [PP_ShowNote]
  else
    Map.PushPinFlags := Map.PushPinFlags + [PP_ShowNote];

  InvalidatePushPins;
end;

procedure TMainForm.SetReadOnlyMode(b:boolean);
begin
  Map.ReadOnly := b;

  // If they set the map to read-only, set the cursor to the arrow.
  if b then begin
    aSelectionExecute(Self);
  end;
end;

procedure TMainForm.ReadOnlyMenuClick(Sender: TObject);
begin
  ReadOnlyMenu.Checked := not ReadOnlyMenu.Checked;
  aReadOnly.Checked := ReadOnlyMenu.Checked;

  SetReadOnlyMode(aReadOnly.Checked);
end;

function TMainForm.ExecuteHyperlink(hypertext:string; hyperflags:THyperlinkFlags):string;
var p,err:integer;
    buffer:array[0..1023] of char;
    docname,params,Ext: string;
begin
  // What we return is displayed when the link is clicked.

  if (hyperExecute in hyperflags) then begin
    // Assume success.
    Result:='';
    Ext := Uppercase(ExtractFileExt(hypertext));
    if (Ext = '.AUR') Or (Ext = '.AURX') then begin
      // If they want to open a map, save this one first, and open in
      // the same window.
      if ModifiedHasBeenSaved then LoadMap(hypertext);
      end
    else begin
      // To allow passing arguments to an exe (an additional niceity),
      // parse into a program and arguments if there is a non-quoted space.
      if (copy(hypertext,1,1)='"') then begin        // Starting Quote?  Look for ending quote.
        delete(hypertext,1,1);     // Delete first quote
        p:=pos('"', hypertext);    // Look for ending quote
        if (p=0) then begin
          docname:=hypertext;      // No ending quote: use the whole name.
          params:='';
          end
        else begin
          docname:=copy(hypertext,1,p-1);       // get document minus trailing quote
          params:=TrimLeft(copy(hypertext,p+1,length(hypertext)));    // Get remainder
          end;
        end
      else begin                   // No quote?  Separate by first space
        p:=pos(' ',hypertext);
        if (p=0) then begin
          docname:=hypertext;      // No space: use the whole name.
          params:='';
          end
        else begin
          docname:=copy(hypertext,1,p-1);       // get document before space
          params:=TrimLeft(copy(hypertext,p+1,length(hypertext)));    // Get remainder
          end;
        end;

      // This will associate a data file with it's appropriate program,
      // open a browser window for a URL, or just plain execute an exe.
      err := ShellExecute(0, 'open', PChar(docname), PChar(params), nil, SW_SHOWNORMAL);
      if (err<32) then begin
        // We had an error: call the system function to format the
        // error message
        if (err=ERROR_FILE_NOT_FOUND) then begin
          if params='' then
            Result := Format(res_main_hyperlink_doc_not_found, [docname])
          else
            Result := Format(res_main_hyperlink_exe_not_found, [docname, params]);

          // Add help for double-quote use with spaces in filenames.
          if (pos(' ',hypertext)<>0) then begin
            Result := Result + Format(res_main_hyperlink_hints, [hypertext]);
            end;
          end
        else begin
          FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,GetLastError(),
                      0,buffer,sizeof(buffer),nil);
          Result:=buffer;
          end;

        end;
      end;
    end;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var dx,dy:integer;
    z:real;
begin
  // Allow Alt+Wheel to rotate selection
  if (ssAlt in Shift) then begin
    if (Map.AnythingSelected) then begin
      Map.SetUndoPoint(res_seltool_undo_rotate, true);
      // Rotate 5 degrees per mouse wheel click
      z := (5 * WheelDelta)/WHEEL_DELTA;
      Map.RotateSelection(z);
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
      end;
    Handled := true;
    exit;
    end;

  // Allow Ctrl+Wheel to size selection
  if (ssCtrl in Shift) then begin
    if (Map.AnythingSelected) then begin
      Map.SetUndoPoint(res_seltool_undo_strech, true);
      if (WheelDelta < 0) then
        z := 1.0 - 0.2*(-WheelDelta/WHEEL_DELTA)  // Rotated back: shrink selection
      else
        z := 1.0 + 0.2*(WheelDelta/WHEEL_DELTA);  // Rotated forward: grow selection

      Map.ScaleSelection(z,z);
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
      end;
    Handled := true;
    exit;
    end;

  // Use Shift+Wheel to scroll horizontally,
  // and normal mouse wheel to scroll vertically.
  if (ssShift in Shift) then begin
    dx := WheelDelta;
    dy := 0;
    end
  else begin
    dx := 0;
    dy := WheelDelta;
    end;

  // Actually do the scrolling.  Map.Pan takes scroll amount in pixels.
  HideCrosshair;
  if (ToolDispatcher<>nil) then begin
    // Stop any tools being used--they may not respond well to having the screen
    // yanked out from under them.
    ToolDispatcher.Cancel;
    end;

  Map.Pan(dx,dy);
  RefreshExtent;          // Reset selection rectangle coordinates

  // If we are using the selection tool, we have the selection rectangle
  // that needs to be redrawn via a paint operation (it cannot be erased via an xor).
  // To do this without making the wheel too slow, we set a one-shot so that when
  // we're done scrolling the wheel, we come back and repaint.
  if (ToolDispatcher = SelectionHandler) then begin
    RefreshOneShot.Enabled := false;
    RefreshOneShot.Enabled := true;
    end;

  Handled := true;
end;

procedure TMainForm.IconCoolBarResize(Sender: TObject);
begin
  // Make sure we keep the icon toolbar window at the correct width--that is,
  // it needs to fill up the remainder of the right half of the coolbar window.
  // (Note: MinHeight is correct here--the coolbar is oriented Horizontally, not
  //  vertically, but MinHeight and MinWidth are not remapped by the CoolBar.)
  // "6" is the width of the internal band borders (no SYSTEM_METRIC for this one).
//  IconCoolBar.Bands[1].MinHeight := IconCoolBar.Width-IconCoolBar.Bands[0].MinHeight - 6;  //zifnabbe 2002/08/18
end;

procedure TMainForm.UseArrowMenuItemClick(Sender: TObject);
begin
  aSelectionExecute(Sender);
end;

procedure TMainForm.UsePanningToolMenuItemClick(Sender: TObject);
begin
  aPanExecute(Self);
end;

//-----------------------------------------------------------------
// Set the position of the scroll bars adjacent to the drawing surface
//-----------------------------------------------------------------
procedure TMainForm.AdhereScrollBars;
var r:TRect;
begin
  // We use independent scrollbars because the ones that come for free with the form
  // unfortunately appear non-adjacent to the drawing area (they're "underneath" the
  // other toolbars).

  // Place the scrollbars in the correct screen locations
  r := GetVisibleRect;

  HorzScrollBar.Width := r.Right - r.Left - VertScrollBar.Width;
  HorzScrollBar.Left := r.Left;
  HorzScrollBar.Top := r.Bottom - HorzScrollBar.Height;

  VertScrollBar.Height := r.Bottom - r.Top;
  VertScrollBar.Top := r.Top;
  VertScrollBar.Left := r.Right - VertScrollBar.Width;

end;

//-----------------------------------------------------------------
// Set the position and thumb size of the scroll bars
//-----------------------------------------------------------------
procedure TMainForm.SetScrollBarPositions;
var r:TRect;
    fullmap:CoordRect;
    fullwidth,fullheight:Coord;
    viewwidth,viewheight:Coord;
    viewport: CoordRect;
    {screencenter:CoordPoint;}
    thumb_size,p:Integer;
    relative_position,past_edge_modifier:Single;
    {horzhadfocus,verthadfocus:boolean;}
begin
  // Set the position and page size
  // Get the full coordinates of all the items on the map
  fullmap := Map.CoordExtent(true);

  fullwidth := fullmap.Right - fullmap.Left;
  fullheight:= fullmap.Bottom - fullmap.Top;

  // If there aren't any objects, we don't have any basis for computing
  // scroll positions.  Just set them to the defaults: normal thumb size
  // in the center of the scroll area.
  if (fullwidth = 0.0) or (fullheight = 0.0) then begin
    HorzScrollBar.Position := 500;
    HorzScrollBar.PageSize := 0;
    VertScrollBar.Position := 500;
    VertScrollBar.PageSize := 0;
    exit;
    end;

  // Get the current screen viewport in the coordinate system
  r := GetVisibleRect;
  Map.CurrentView.ScreenToCoord(r.Left,r.Top,     viewport.Left,viewport.Top);
  Map.CurrentView.ScreenToCoord(r.Right,r.Bottom, viewport.Right,viewport.Bottom);

  viewwidth := viewport.Right - viewport.Left;
  viewheight := viewport.Bottom - viewport.Top;

  // Compute the coordinate at the center of the screen
  {screencenter.X := (viewport.Right + viewport.Left)/2;}
  {screencenter.Y := (viewport.Bottom + viewport.Top)/2;}

  // Position and pagesize are set so the full scale (Min and Max) is from 0..1000

  // Don't allow the scrollbar windows to keep focus while we're editing the page size.
  // Setting the page size causes an artifact on the scrollbars.  The blinking
  // "thumb focus indicator" doesn't change size properly, and will retain the original
  // size.  Our hack for this: temporarily move the focus off the bar that we're changing.
  {horzhadfocus := HorzScrollBar.Focused;}
  {verthadfocus := VertScrollBar.Focused;}

  {if HorzScrollBar.Focused then VertScrollBar.SetFocus;}

  //////////////////////////////////////////////////////////////////////////////

  // Set horizontal position
  past_edge_modifier := 0;
  // convert the viewport position to a distance relative to the map left
  relative_position := abs(viewport.Left - fullmap.Left);
  if ( viewport.Left < fullmap.Left ) then
  begin
    // Now that we've reached the map's left edge, further scrolling is
    // actually making the map bigger (or will be if the user adds any objects
    // at this position).
    past_edge_modifier := relative_position;
    relative_position := 0;
  end
  else
  begin
    if ( viewport.Right > fullmap.Right ) then
    begin
      // Going right, we calculate the adjustment slightly different: we only
      // decrease the page size and leave the position alone.
      past_edge_modifier := viewport.Right - fullmap.Right;
    end;
  end;

  // Set horizontal thumb size
  thumb_size := Trunc(1000 * (viewwidth - past_edge_modifier) / fullwidth);
  // Pagesize property is not good with getting out of range values--it tends to
  // ignore subsequent ones.  To prevent problems, limit it to valid values
  // before setting.
  if (thumb_size<1) then thumb_size:=1;
  if (thumb_size>1000) then thumb_size:=1000;
  HorzScrollBar.PageSize := thumb_size;

  // Set horizontal position
  p := Trunc(1000 * relative_position / fullwidth);
  HorzScrollBar.Position := p;

  //////////////////////////////////////////////////////////////////////////////

  // Move focus off Vertical while we're changing it: read note about this hack above.
  {if VertScrollBar.Focused then HorzScrollBar.SetFocus;}

  // Set vertical position
  past_edge_modifier := 0;
  // convert the viewport position to a distance relative to the map top
  relative_position := abs(viewport.Top - fullmap.Top);
  if ( viewport.Top < fullmap.Top ) then
  begin
    // Now that we've reached the map's top edge, further scrolling is
    // actually making the map bigger (or will be if the user adds any objects
    // at this position).
    past_edge_modifier := relative_position;
    relative_position := 0;
  end
  else
  begin
    if ( viewport.Bottom > fullmap.Bottom ) then
    begin
      // A sligtly different adjustment must be made if we're past the bottom
      // map edge since we need to compensate for the size of the scroll bar thumb.
      past_edge_modifier := viewport.Bottom - fullmap.Bottom;
    end;
  end;

  // Set vertical thumb size
  thumb_size := Trunc(1000 * (viewheight - past_edge_modifier) / fullheight);
  // Pagesize property is not good with getting out of range values--it tends to
  // ignore subsequent ones.  To prevent problems, limit it to valid values
  // before setting.
  if (thumb_size<1) then thumb_size:=1;
  if (thumb_size>1000) then thumb_size:=1000;
  VertScrollBar.PageSize := thumb_size;

  // Set vertical position
  p := Trunc(1000 * relative_position / fullheight);
  VertScrollBar.Position := p;

  // Set focus back to scrollbar that might have had it before
  {if horzhadfocus then HorzScrollBar.SetFocus;}
  {if verthadfocus then VertScrollBar.SetFocus;}

  // This stops the scrollbar blinking with the size of
  // the slider left over from the previous map. (hackish)
  MainForm.ActiveControl := Nil;

end;

procedure TMainForm.ScrollBarScroll(Sender: TObject;
                                        ScrollCode: TScrollCode; var ScrollPos: Integer);
var horz:boolean;
    fullmap:CoordRect;
    fullwidth,fullheight:Coord;
    adj:Coord;
    dx,dy:Coord;
    sx,sy:integer;
    fulltravel:integer;
begin
  // Was it the vertical or horizontal scrollbar?
  horz := (Sender=HorzScrollBar);

  if (ScrollCode = scEndScroll) then begin
    // Once they stop scrolling, we get this event.  Use it to clean up the display;
    // redraw any misaligned fills, and more importantly, if they're using draft panning
    // make sure we redraw objects with the real drawing.
    Map.CurrentView.QuickDraw:=QuickDraw_None;
    Invalidate;
    exit;
    end
  else begin
    // For any other type of sustained scrolling, use the same settings as we do for
    // panning mode: if the user wants a quick redraw, we don't fill in a lot of details
    // until they let go of the scroll thumb.
    if Settings.DraftPanning.Checked then
      Map.CurrentView.QuickDraw:=QuickDraw_All
    else
      Map.CurrentView.QuickDraw:=QuickDraw_None;
    end;

  // Get the full coordinates of all the items on the map
  fullmap := Map.CoordExtent(true);
  fullwidth := fullmap.Right - fullmap.Left;
  fullheight:= fullmap.Bottom - fullmap.Top;

  // If there aren't any objects, we don't have any basis for computing
  // scroll positions.
  if (fullwidth = 0.0) or (fullheight = 0.0) then exit;

  if horz then begin
    // Figure out delta in thumb position
    adj := HorzScrollBar.Position - ScrollPos;
    // Convert to 1/1000th of the map travel
    fulltravel := HorzScrollBar.Max - HorzScrollBar.Min - HorzScrollBar.PageSize;
    if (fulltravel <= 0) then exit;

    dx := adj*(fullwidth/fulltravel);
    dy := 0;
    end
  else begin
    // Figure out delta in thumb position
    adj := VertScrollBar.Position - ScrollPos;
    // Convert to 1/1000th of the map travel
    fulltravel := VertScrollBar.Max - VertScrollBar.Min - VertScrollBar.PageSize;
    if (fulltravel <= 0) then exit;

    dx := 0;
    dy := adj*(fullheight/fulltravel);
    end;

  // 2003/04/22 - j.friant
  // When the adjustment factor is 0, the screen coordinates
  // get set to [0,0] and that messes up the scroll bars,
  // so we won't pan when adj is equal to 0
  if (adj <> 0) then
    begin
    // Convert from coordinates to pixels
    Map.CurrentView.DeltaCoordToScreen(dx,dy,sx,sy);

    HideCrosshair;
    // Pan the number of pixels required
    Map.Pan(sx,sy);
    end;

  SetScrollBarPositions;
end;

procedure TMainForm.ZoomComboBoxClick(Sender: TObject);
begin
  doZoom(ZoomComboBox.Text);
end;

procedure TMainForm.ZoomComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // If they hit the Enter key in the zoom combo box, process the text
  // they typed as an arbitrary zoom level.
  if (Key=VK_RETURN) then begin
    Key := 0;
    ZoomComboBoxClick(Sender);
  end;
end;

procedure TMainForm.LoadZoomPercentageBox;
begin
  ZoomComboBox.Text := IntToStr(Map.CurrentView.GetZoomPercent) + '%';
{
  case Map.CurrentView.GetZoomPercent of
    10: mnuViewZoom010.Checked := True;
    25: mnuViewZoom025.Checked := True;
    50: mnuViewZoom050.Checked := True;
    75: mnuViewZoom025.Checked := True;
    100: mnuViewZoom100.Checked := True;
    150: mnuViewZoom150.Checked := True;
    200: mnuViewZoom200.Checked := True;
    300: mnuViewZoom300.Checked := True;
    400: mnuViewZoom400.Checked := True;
    500: mnuViewZoom500.Checked := True;
  else
    mnuViewZoomCustom.Checked := True;
  end;
}
end;

procedure TMainForm.HandleGridSizeChange;
Var GridSize,GridPos: Double;
begin
  If Not ChangingGridSizeEdit Then
  Begin
    ChangingGridSizeEdit := True;
    if Map.ReadOnly then
    begin
      FlashReadOnlyProhibits;
      exit;
    end;

    GridSize := StrToFloat(edtGridSize.Text);
    GridPos  := GetGridPositionFromLabel(GridSize);
    if (GridSize > 0) And
       (MapSettingsDialog <> Nil) And
       (GridPos * UnitsPerGridTick <> Map.CurrentView.Grid.CurrentGridSize) then
    begin
      Map.SetUndoPoint(res_main_grid_resize, true);
      Map.CurrentView.Grid.SetGraphUnits(MapSettingsDialog.UnitComboBox.ItemIndex, GridPos * UnitsPerGridTick);
      DoInvalidate;
      Map.SetModified(modGrid);
    end;
    ChangingGridSizeEdit := False;
  End;
end;

procedure TMainForm.edtGridSizeExit(Sender: TObject);
begin
  HandleGridSizeChange;
end;

procedure TMainForm.edtGridSizeKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 Then HandleGridSizeChange;
end;

Procedure TMainForm.SetSelectionXSize(Sender: TObject);
// Retries can be necessary because fonts along the edges of the selection will
// automatically rescale and can cause the resulting size to be different than what
// was desired.  Retrying it causes the size to "home in" on the desired size.
Const MaxTries = 20;
Var
  C   : CoordRect;
  X,Y : Double;
  D   : Double;
  S   : Double;
  I   : Integer;

Begin
  If (Map <> Nil) And Map.AnythingSelected Then
  Begin
    C := Map.CoordExtent(False);
    X := Abs(C.Right - C.Left) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    Y := Abs(C.Bottom - C.Top) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    S := StrToFloat(edtXSize.Text);
    D := Y;
    If X <> 0 Then
    Begin
      If cbKeepAspect.Checked Then D := D * S / X;
      Map.SetUndoPoint(res_main_set_x_size);
      I := 0;
      While (S <> X) And (I < MaxTries) And (Y <> 0) Do
      Begin
        Map.ScaleSelection(S / X,D / Y);
        C := Map.CoordExtent(False);
        X := Abs(C.Right - C.Left) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
        Y := Abs(C.Bottom - C.Top) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
        Inc(I);
      End; // While
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    End;
  End;
End; // TMainForm.SetSelectionXSize

Procedure TMainForm.SetSelectionYSize(Sender: TObject);
// Retries can be necessary because fonts along the edges of the selection will
// automatically rescale and can cause the resulting size to be different than what
// was desired.  Retrying it causes the size to "home in" on the desired size.
Const MaxTries = 20;
Var
  C   : CoordRect;
  X,Y : Double;
  D   : Double;
  S   : Double;
  I   : Integer;

Begin
  If (Map <> Nil) And Map.AnythingSelected Then
  Begin
    C := Map.CoordExtent(False);
    X := Abs(C.Right - C.Left) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    Y := Abs(C.Bottom - C.Top) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
    S := StrToFloat(edtYSize.Text);
    D := X;
    If Y <> 0 Then
    Begin
      If cbKeepAspect.Checked Then D := D * S / Y;
      Map.SetUndoPoint(res_main_set_y_size);
      I := 0;
      While (S <> Y) And (I < MaxTries) And (X <> 0) Do
      Begin
        Map.ScaleSelection(D / X,S / Y);
        C := Map.CoordExtent(False);
        X := Abs(C.Right - C.Left) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
        Y := Abs(C.Bottom - C.Top) * Map.CurrentView.Grid.GraphUnitConvert * Map.CurrentView.Grid.GraphScale;
        Inc(I);
      End; // While
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    End;
  End;
End; // TMainForm.SetSelectionYSize

procedure TMainForm.edtXSizeKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 Then SetSelectionXSize(Self);
end;

procedure TMainForm.edtYSizeKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 Then SetSelectionYSize(Self);
end;

Function TMainForm.GetSelectionToolHandler: TSelectionToolHandler;
Begin
  Result := SelectionHandler;
End; // TMainForm.GetSelectionToolHandler

procedure TMainForm.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  // Make sure that if toolbars are docked or undocked, that we replace the
  // scroll bars where they belong.
  AdhereScrollBars;
end;

procedure TMainForm.RefreshOneShotTimer(Sender: TObject);
begin
  Invalidate;
  RefreshOneShot.Enabled := false;
end;

{ **** TActions **** zifnabbe 2002/08/18}

procedure TMainForm.aSelectionExecute(Sender: TObject);
begin
  SwitchToolDispatcher(0, SelectionHandler);
  SelectionHandler.Paint;
end;

procedure TMainForm.aOpenExecute(Sender: TObject);
begin
  if ModifiedHasBeenSaved then
    begin
      HideCrosshair;
      if OpenDialog.Execute then LoadMap(OpenDialog.Filename);
    end;
end;

procedure TMainForm.aZoomInExecute(Sender: TObject);
begin
  SwitchToolDispatcher(8, ZoomHandler);
end;

procedure TMainForm.aPanExecute(Sender: TObject);
begin
  SwitchToolDispatcher(9, PanHandler);
end;

procedure TMainForm.aRulerExecute(Sender: TObject);
begin
  SwitchToolDispatcher(1, RulerHandler);
end;

procedure TMainForm.aFreeHandRulerExecute(Sender: TObject);
begin
  SwitchToolDispatcher(22, FreehandRulerHandler);
end;

procedure TMainForm.aSaveExecute(Sender: TObject);
begin
  if InvokedAsSymbolEditor then
    SaveSymbolChanges
  else
    begin
      if (CurrentFilename = '') then
        begin
          HideCrosshair;
          repeat
            if not SaveDialog.Execute then exit;
          until AlrightToSaveTo(SaveDialog.Filename);

          CurrentFilename := SaveDialog.Filename;
        end;

      SaveMap(CurrentFilename);
    end;
end;

procedure TMainForm.aRepaintExecute(Sender: TObject);
begin
  DoInvalidate;
end;

procedure TMainForm.aUndoExecute(Sender: TObject);
var
  oldcursor: TCursor;
begin
  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  if Map.Undo then
    begin
      DoInvalidate;
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    end;
  Screen.Cursor := oldcursor;
end;


procedure TMainForm.aRedoExecute(Sender: TObject);
var
  oldcursor: TCursor;
begin
  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  if Map.Redo then
    begin
      DoInvalidate;
      SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
    end;
  Screen.Cursor := oldcursor;
end;

procedure TMainForm.aReadOnlyExecute(Sender: TObject);
begin
  (Sender as TAction).Checked := not (Sender as TAction).Checked;
  SetReadOnlyMode((Sender as TAction).Checked);
end;

procedure TMainForm.aRectangleExecute(Sender: TObject);
begin
  SwitchToolDispatcher(21, RectangleHandler);
end;

procedure TMainForm.aCircleExecute(Sender: TObject);
begin
  SwitchToolDispatcher(15, NormalCircleHandler);
end;

procedure TMainForm.aPolygonExecute(Sender: TObject);
begin
  if PolygonHandler.AskSides then
    begin
      SwitchToolDispatcher(16, PolygonHandler);
    end
  else
    aSelectionExecute(Sender);
end;

procedure TMainForm.aArcExecute(Sender: TObject);
begin
  SwitchToolDispatcher(28, NormalArcHandler);
end;

procedure TMainForm.aChartGridExecute(Sender: TObject);
begin
  if ChartGridHandler.AskGrid then
    begin
      SwitchToolDispatcher(20, ChartGridHandler);
    end
  else
    aSelectionExecute(Sender);
end;

procedure TMainForm.aTextOutExecute(Sender: TObject);
begin
  HideCrosshair;

  if (not ChooseFont.Visible) then ChooseFont.Show;

  SwitchToolDispatcher(17, TextHandler);
end;

procedure TMainForm.aCurvedTextExecute(Sender: TObject);
begin
  HideCrosshair;

  if (not ChooseFont.Visible) then ChooseFont.Show;

  SwitchToolDispatcher(18, CurvedTextHandler);
end;

procedure TMainForm.aHyperLinkExecute(Sender: TObject);
begin
  SwitchToolDispatcher(27, HyperlinkHandler);
end;

procedure TMainForm.aGroupExecute(Sender: TObject);
begin
  Map.Group;
end;

procedure TMainForm.aUnGroupExecute(Sender: TObject);
begin
  Map.Ungroup;
end;

procedure TMainForm.aDecomposeExecute(Sender: TObject);
var
  oldcursor: TCursor;
begin
  oldcursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Map.Decompose;
  ToolDispatcher.Refresh;
  Screen.Cursor := oldcursor;
end;

procedure TMainForm.aNormalFreeHandExecute(Sender: TObject);
begin
  SwitchToolDispatcher(14, FreehandHandler);
end;

procedure TMainForm.aNormalLineExecute(Sender: TObject);
begin
  SwitchToolDispatcher(5, NormalLineHandler);
end;

procedure TMainForm.aNormalPolylineExecute(Sender: TObject);
begin
  SwitchToolDispatcher(6, NormalPolylineHandler);
end;

procedure TMainForm.aNormalCurveExecute(Sender: TObject);
begin
  SwitchToolDispatcher(7, NormalCurveHandler);
end;

procedure TMainForm.aNormalPolyCurveExecute(Sender: TObject);
begin
  SwitchToolDispatcher(23, PolycurveHandler);
end;

procedure TMainForm.aFractalFreeHandExecute(Sender: TObject);
begin
  SwitchToolDispatcher(19, FreehandFractalHandler);
end;

procedure TMainForm.aFractalPolylineExecute(Sender: TObject);
begin
  SwitchToolDispatcher(3, FractalPolylineHandler);
end;

procedure TMainForm.aFractalCurveExecute(Sender: TObject);
begin
  SwitchToolDispatcher(4, FractalCurveHandler);
end;

procedure TMainForm.aFractalLineExecute(Sender: TObject);
begin
  SwitchToolDispatcher(2, FractalLineHandler);
end;

procedure TMainForm.aFractalPolyCurveExecute(Sender: TObject);
begin
  SwitchToolDispatcher(24, FractalPolycurveHandler);
end;

procedure TMainForm.aAlignExecute(Sender: TObject);
var
  s: string;
  hi, vi, wdi, hgi: integer;
begin
  HideCrosshair;

  Application.CreateForm(TAlignDialog, AlignDialog);

  with AlignDialog do
    begin
      if (ShowModal = mrOk) and
        ((Horizontal.ItemIndex <> 0) or (Vertical.ItemIndex <> 0) or
        (Heights.ItemIndex <> 0) or (Widths.ItemIndex <> 0)) then
        begin
          hi := Horizontal.ItemIndex;
          vi := Vertical.ItemIndex;
          wdi := Widths.ItemIndex;
          hgi := Heights.ItemIndex;
          s := '';
          if (hi <> 0) then
            s := ' ' + StripHotKey(Horizontal.Caption) + ' ' + StripHotKey(Horizontal.Items.Strings[hi]);

          if (vi <> 0) then
            begin
              if (s <> '') then s := s + ', ';
              s := s + StripHotKey(Vertical.Caption) + ' ' + StripHotKey(Vertical.Items.Strings[vi]);
            end;

          if (wdi <> 0) then
            begin
              if (s <> '') then s := s + ', ';
              s := s + StripHotKey(Widths.Caption) + ' ' + StripHotKey(Widths.Items.Strings[wdi]);
            end;

          if (hgi <> 0) then
            begin
              if (s <> '') then s := s + ', ';
              s := s + StripHotKey(Heights.Caption) + ' ' + StripHotKey(Heights.Items.Strings[hgi]);
            end;

          Map.SetUndoPoint(s);
          HideCrosshair;
          Map.AlignSelection(AlignType(hi), AlignType(vi), AlignType(wdi), AlignType(hgi));
          ToolDispatcher.Refresh;
        end;
    end;

  AlignDialog.Free;
end;

procedure TMainForm.aFlipExecute(Sender: TObject);
begin
  HideCrosshair;

  Application.CreateForm(TFlipForm, FlipForm);

  if FlipForm.ShowModal = mrOk then
    with FlipForm do
      begin
        Map.SetUndoPoint(aFlip.hint);
        HideCrosshair;
        Map.FlipSelection(Horizontal.Checked, Vertical.Checked);
        ToolDispatcher.Refresh;
      end;

  FlipForm.Free;
end;

procedure TMainForm.aSkewExecute(Sender: TObject);
begin
  HideCrosshair;

  Application.CreateForm(TSkewForm, SkewForm);

  if SkewForm.ShowModal = mrOk then
    with SkewForm do
      begin
        Map.SetUndoPoint(aSkew.hint);
        HideCrosshair;
        Map.SkewSelection(HorizontalSkew.Value / 100, VerticalSkew.Value / 100);
        ToolDispatcher.Refresh;
      end;

  SkewForm.Free;
end;

procedure TMainForm.aRotateExecute(Sender: TObject);
var
  Degrees: double;
begin
  HideCrosshair;

  if RotateForm.ShowModal = mrOk then
    begin
      Degrees := -RotateForm.HeadingControl.Angle;
      Map.SetUndoPoint(Format(res_main_rotate_degrees, [Degrees]));
      HideCrosshair;
      Map.RotateSelection(Degrees);
      ToolDispatcher.Refresh;
    end;
end;

procedure TMainForm.aScaleExecute(Sender: TObject);
begin
  HideCrosshair;

  Application.CreateForm(TScaleForm, ScaleForm);

  if ScaleForm.ShowModal = mrOk then
    with ScaleForm do
      begin
        Map.SetUndoPoint(aScale.hint);
        HideCrosshair;
        if UniformRadioBtn.Checked then
          Map.ScaleSelection(UniformScaling.Value / 100, UniformScaling.Value / 100)
        else
          Map.ScaleSelection(HorizontalScaling.Value / 100, VerticalScaling.Value / 100);
        ToolDispatcher.Refresh;
      end;

  ScaleForm.Free;
end;

procedure TMainForm.aMoveExecute(Sender: TObject);
var
  x, y, a, d: Coord;
begin
  HideCrosshair;

  Application.CreateForm(TMoveSelection, MoveSelection);

  if MoveSelection.ShowModal = mrOk then
    with MoveSelection do
      begin
        try
          if Polar.Checked then
            begin
              a := -StrToFloat(Angle.Text);
              d := StrToFloat(Dist.Text);
              d := Map.CurrentView.Grid.Convert(d, UnitCombo.ItemIndex);
              a := a * pi / 180.0;
              x := d * cos(a);
              y := d * sin(a);
            end
          else
            begin
              x := StrToFloat(OffsetX.Text);
              y := StrToFloat(OffsetY.Text);
              x := Map.CurrentView.Grid.Convert(x, UnitCombo.ItemIndex);
              y := Map.CurrentView.Grid.Convert(y, UnitCombo.ItemIndex);
            end;
        except
          MoveSelection.Free;
          exit;
        end;
        Map.SetUndoPoint(res_main_move_sel);
        HideCrosshair;
        Map.MoveSelection(x, y);

        ToolDispatcher.Refresh;
      end;

  MoveSelection.Free;
end;

procedure TMainForm.aRotate90Execute(Sender: TObject);
begin
  if Map.AnythingSelected then
    begin
      Map.SetUndoPoint(aRotate90.hint);
      HideCrosshair;
      Map.RotateSelection(90);
      ToolDispatcher.Refresh;
    end;
end;

procedure TMainForm.aRotate45Execute(Sender: TObject);
begin
  if Map.AnythingSelected then
    begin
      Map.SetUndoPoint(aRotate45.hint);
      HideCrosshair;
      Map.RotateSelection(45);
      ToolDispatcher.Refresh;
    end;
end;

procedure TMainForm.aArrayExecute(Sender: TObject);
begin
  ArrayMenuItemClick(Sender);
end;

procedure TMainForm.aOrderExecute(Sender: TObject);
var undoname:string;
begin
  HideCrosshair;

  Application.CreateForm(TOrderDialog, OrderDialog);

  with OrderDialog do
    begin
      TopmostObject.ItemIndex := LastOrderIndex;
      if (ShowModal = mrOk) then begin
          undoname := TopmostObject.Items[TopmostObject.ItemIndex];
          Map.SetUndoPoint(Caption + StripHotKey(undoname));
          HideCrosshair;
          Map.OrderSelection(OrderType(TopmostObject.ItemIndex));
          ToolDispatcher.Refresh;
          LastOrderIndex := TopmostObject.ItemIndex;
        end;
    end;

  OrderDialog.Free;
end;

procedure TMainForm.aSendToBackExecute(Sender: TObject);
begin
  Map.SendToBack;
end;

procedure TMainForm.aSendBackwardExecute(Sender: TObject);
begin
  Map.SendBackward;
end;

procedure TMainForm.aBringForwardExecute(Sender: TObject);
begin
  Map.BringForward;
end;

procedure TMainForm.aBringToFrontExecute(Sender: TObject);
begin
  Map.BringToFront;
end;

procedure TMainForm.aPasteExecute(Sender: TObject);
begin
  Map.Paste;
  tbiSelection.Click;
end;

procedure TMainForm.aCopyExecute(Sender: TObject);
begin
  Map.Copy;
end;

procedure TMainForm.aCutExecute(Sender: TObject);
begin
  Map.Cut;
  ToolDispatcher.Refresh;
  DoInvalidate;
end;

procedure TMainForm.aDeleteExecute(Sender: TObject);
begin
  Map.Delete;
  ToolDispatcher.Refresh;
end;

procedure TMainForm.aScalpelExecute(Sender: TObject);
begin
  SwitchToolDispatcher(26, ScalpelHandler);
end;

procedure TMainForm.aGlueExecute(Sender: TObject);
begin
  SwitchToolDispatcher(25, GlueHandler);
end;

procedure TMainForm.aPlaceOneExecute(Sender: TObject);
begin
  SwitchToolDispatcher(10, IconHandler);
end;

procedure TMainForm.aPlaceSquareExecute(Sender: TObject);
begin
  SwitchToolDispatcher(11, PatternIconHandler);
  PatternIconHandler.Square := true;
end;

procedure TMainForm.aPlaceDiamondExecute(Sender: TObject);
begin
  SwitchToolDispatcher(12, PatternIconHandler);
  PatternIconHandler.Square := false;
end;

procedure TMainForm.aPlaceRandomExecute(Sender: TObject);
begin
  SwitchToolDispatcher(13, SprayIconHandler);
end;

procedure TMainForm.aDefineSymbolExecute(Sender: TObject);
begin
  if (not Map.AnythingSelected) then begin
    if (Application.MessageBox(pchar(res_main_define_empty_symbol),
                           pchar(res_main_warning), MB_YESNO) = IDNO) then exit;
    end;

  Application.CreateForm(TNewSymbol, NewSymbol);
  NewSymbol.ShowModal;
  NewSymbol.Free;
end;

procedure TMainForm.aSymbolLibraryExecute(Sender: TObject);
begin
  if SymbolLibraryForm.Visible and SymbolLibraryForm.Active then
    SymbolLibraryForm.Hide
  else
    SymbolLibraryForm.Show;
end;

procedure TMainForm.aDisplayOverlayExecute(Sender: TObject);
begin
  Settings.VisualOverlays.Checked := not Settings.VisualOverlays.Checked;
  DoInvalidate;
end;

procedure TMainForm.aDisplayGridExecute(Sender: TObject);
begin
  MapSettingsDialog.DisplayGrid.Checked := not MapSettingsDialog.DisplayGrid.Checked;
  MapSettingsDialog.SnapToGrid.Checked  := MapSettingsDialog.DisplayGrid.Checked;
  DrawSnapGrid(true);
end;

procedure TMainForm.aGravitySnapExecute(Sender: TObject);
begin
  MapSettingsDialog.SnapToPoint.Checked := not MapSettingsDialog.SnapToPoint.Checked;
end;

procedure TMainForm.aGridSnapExecute(Sender: TObject);
begin
  MapSettingsDialog.SnapToGrid.Checked := not MapSettingsDialog.SnapToGrid.Checked;
end;

procedure TMainForm.aGravitySnapAlongExecute(Sender: TObject);
begin
  MapSettingsDialog.cbSnapTo.Checked := not MapSettingsDialog.cbSnapTo.Checked;
  MapSettingsDialog.cbRotateSnap.Enabled := MapSettingsDialog.cbSnapTo.Checked;
end;

procedure TMainForm.aRotateSnapExecute(Sender: TObject);
begin
  MapSettingsDialog.cbRotateSnap.Checked := not MapSettingsDialog.cbRotateSnap.Checked;
end;

procedure TMainForm.MRUListClick(Sender: TObject; const Filename: String);
begin
  if ModifiedHasBeenSaved then
    begin
      LoadMap(filename);
    end;
end;

procedure TMainForm.aSelectNoneExecute(Sender: TObject);
begin
  Map.ClearSelection;
  ToolDispatcher.Refresh;
  DoInvalidate;
end;

procedure TMainForm.aShowCTManagerExecute(Sender: TObject);
begin
  HideCrosshair;
  if frmCTManager.ShowModal = mrOk then
  begin
    HideCrosshair;
    ToolDispatcher.Refresh;
  end;
end;

procedure TMainForm.aTranslateColorExecute(Sender: TObject);
begin
  Map.SetUndoPoint(res_ct_translate);
  frmCTManager.TranslateSelectionColors;
  DoInvalidate;
end;

procedure TMainForm.aInverseTranslateColorExecute(Sender: TObject);
begin
  Map.SetUndoPoint(res_ct_inv_translate);
  frmCTManager.InverseTranslateSelectionColors;
  DoInvalidate;
end;

procedure TMainForm.mnuColumnsAndRooftopsClick(Sender: TObject);
begin
  If frmMakeFrom3D.ShowModal = mrOK Then
  Begin
    Map.SetUndoPoint(res_mapobj_o_add);
    frmMakeFrom3D.ExportSurface;
    tbiSelection.Click;
  End;
end;

procedure TMainForm.aSetToNormalLinesExecute(Sender: TObject);
begin
  Map.SetFractalStateSelected(Primitives.fsSetNormal);
end;

procedure TMainForm.aSetToFractalLinesExecute(Sender: TObject);
begin
  Map.SetFractalStateSelected(Primitives.fsSetFractal);
end;

procedure TMainForm.aToggleNormalFractalLinesExecute(Sender: TObject);
begin
  Map.SetFractalStateSelected(Primitives.fsFlipFractal);
end;

procedure TMainForm.aFlipLineStyleExecute(Sender: TObject);
begin
  if Map.AnythingSelected then
  begin
    Map.SetUndoPoint(res_main_line_flip);
    HideCrosshair;
    Map.ReverseSelected(true);
    SelectionHandler.Refresh;        // Reset the red selection rectangle (displayed or not).
  end;
end;

procedure TMainForm.ToolbarResize(Sender: TObject);
begin
  AdhereScrollBars;
end;

procedure TMainForm.mnuTransformGridSendbackwardClick(Sender: TObject);
begin
  Map.CurrentView.Grid.sendGridBackward;
  Map.Invalidate;
end;

procedure TMainForm.mnuTransformGridBringforwardClick(Sender: TObject);
begin
  Map.CurrentView.Grid.bringGridForward;
  Map.Invalidate;
end;

procedure TMainForm.mnuViewZoomClick(Sender: TObject);
var
  menu_item : TTBItem;
begin
  menu_item := Sender as TTBItem;
  doZoom(menu_item.Caption);
end;

procedure TMainForm.doZoom(zoomstr: String);
var
  percent   : single;
  err       : integer;
begin
  // Remove trailing '%' if it's there.
  if (zoomstr<>'') and (zoomstr[length(zoomstr)]='%') then begin
    delete(zoomstr,length(zoomstr),1);
  end;

  val(zoomstr,percent,err);

  if (percent=0.0) or (err<>0) then
    Map.ShowAll(Map.CurrentView, 0.5) // Do a Show all if we select "Fit"
  else
    Map.CurrentView.SetZoomPercent(percent);

  DoInvalidate;
end;

procedure TMainForm.mnuDrawingSymbolsIconSizeClick(Sender: TObject);
var
  msg: String;
  size:  Integer;
begin
  msg := 'Size (';
  msg := msg + IntToStr(IconSizeBar.Min);
  msg := msg + ' to ';
  msg := msg + IntToStr(IconSizeBar.Max);
  msg := msg + ')';
  size := StrToIntDef(InputBox('Enter an Icon Size', msg, IntToStr(IconSizeBar.Position)), 0);
  if ( size >= IconSizeBar.Min ) and ( size <= IconSizeBar.Max ) then
    IconSizeBar.Position := size;
end;

procedure TMainForm.mnuDrawingSymbolsIconSpreadClick(Sender: TObject);
var
  msg: String;
  val:  Integer;
begin
  msg := 'Density (';
  msg := msg + IntToStr(DensityBar.Min);
  msg := msg + ' to ';
  msg := msg + IntToStr(DensityBar.Max);
  msg := msg + ')';
  val := StrToIntDef(InputBox('Enter the Icon Spread', msg, IntToStr(DensityBar.Position)), 0);
  if ( val >= DensityBar.Min ) and ( val <= DensityBar.Max ) then
    DensityBar.Position := val;
end;

procedure TMainForm.mnuDrawingFractalRoughnessClick(Sender: TObject);
var
  msg: String;
  val: Integer;
begin
  msg := 'Roughness (';
  msg := msg + IntToStr(RoughnessTrackBar.Min);
  msg := msg + ' to ';
  msg := msg + IntToStr(RoughnessTrackBar.Max);
  msg := msg + ')';
  val := StrToIntDef(InputBox('Enter the Fractal Roughness', msg, IntToStr(RoughnessTrackBar.Position)), 0);
  if ( val >= RoughnessTrackBar.Min ) and ( val <= RoughnessTrackBar.Max ) then
  begin
    RoughnessTrackBar.Position := val;
    RoughnessTrackBarChange(Sender);
  end;
end;

procedure TMainForm.mnuDrawingFractalSeedClick(Sender: TObject);
var
  msg: String;
  val: Integer;
begin
  msg := 'Fractal Random Seed (';
  msg := msg + IntToStr(SeedSpin.MinValue);
  msg := msg + ' to ';
  msg := msg + IntToStr(SeedSpin.MaxValue);
  msg := msg + ')';
  val := StrToIntDef(InputBox('Enter the Fractal Seed', msg, IntToStr(SeedSpin.Value)), 0);
  if ( val >= SeedSpin.MinValue ) and ( val <= SeedSpin.MaxValue ) then
  begin
    SeedSpin.Value := val;
    SeedSpinChange(Sender);
  end;
end;

procedure TMainForm.mnuDrawingGraphGridSizeClick(Sender: TObject);
var
  msg: String;
  val: Double;
  min: Double;
  max: Double;
begin
  if Map.ReadOnly then
    FlashReadOnlyProhibits
  else
  begin
    min := 0; //GridSizeBar.Min * UnitsPerGridTick;
    max := GridSizeBar.Max * UnitsPerGridTick;
    msg := 'Squares Per Inch (';
    msg := msg + FloatToStr(min);
    msg := msg + ' to ';
    msg := msg + FloatToStr(max);
    msg := msg + ')';
    val := StrToFloatDef(InputBox('Enter the Grid Size', msg, edtGridSize.Text), 0.0);
    if ( val >= min ) and ( val <= max ) then
    begin
      edtGridSize.Text := FloatToStr(val);
      HandleGridSizeChange;
    end;
  end;
end;

procedure TMainForm.mnuDrawingGraphSecGridSizeClick(Sender: TObject);
var
  msg: String;
  val: Integer;
begin
  msg := 'Grid Size (';
  msg := msg + IntToStr(BoldUnitCount.MinValue);
  msg := msg + ' to ';
  msg := msg + IntToStr(BoldUnitCount.MaxValue);
  msg := msg + ')';
  val := StrToIntDef(InputBox('Enter the Secondary Grid Size', msg, IntToStr(BoldUnitCount.Value)), 0);
  if ( val >= BoldUnitCount.MinValue ) and ( val <= BoldUnitCount.MaxValue ) then
  begin
    BoldUnitCount.Value := val;
    BoldUnitCountChange(Sender);
  end;
end;

procedure TMainForm.edtLineThicknessAcceptText(Sender: TObject;
  var NewText: String; var Accept: Boolean);
Var
  Style     : StyleAttrib;
  Thickness : Coord;
  I         : Integer;

begin
  If LineStyleComboBox.ItemIndex = start_numeric_thickness_style Then
  Begin
    If Not Settings.FreezeProperties.Checked Then
    Begin
      Val(NewText,Thickness,I);
      If I = 0 Then
      Begin
        Style.Bits := $FFFFFFFF;
        Style.FullStyle.Thickness  := Thickness;
        Style.FullStyle.SThickness := 0;
        If Map.SetStyle(Style) Then RefreshExtent;
        Accept := True;
      End
      Else Accept := False;
    End
    Else Accept := False;
  End
  Else Accept := False;
end;

end.

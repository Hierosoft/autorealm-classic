�
 TSYMBOLLIBRARYFORM 0�  TPF0TSymbolLibraryFormSymbolLibraryFormLeftTopuWidthHeight�CaptionSymbol LibraryColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenterShowHint	OnCreate
FormCreate	OnDestroyFormDestroyOnShowFormShowPixelsPerInch`
TextHeight 	TSplitter	Splitter1Left� Top WidthHeightzCursorcrHSplit  TPanelPanel1Left TopzWidth�Height AlignalBottom
BevelOuterbvNoneTabOrder  TButton	btnDefineLeftPTopWidthAHeightHint-Define selection on current map as new symbolCaption&DefineTabOrderOnClickbtnDefineClick  TButton	btnRevertLeftpTopWidthAHeightHint$Revert ALL symbols to saved versionsCaption&Reload AllTabOrderOnClickbtnRevertClick  TButtonbtnAddGroupLeft� TopWidthAHeightHintCreate a new groupCaption
&Add GroupTabOrderOnClickbtnAddGroupClick  TButtonbtnDelGroupLeft(TopWidthAHeightHintDelete a groupCaption
Del &GroupTabOrderOnClickbtnDelGroupClick  TButton
btnSaveAllLeft�TopWidthAHeightHintSave all symbolsCaption	&Save AllTabOrderOnClickbtnSaveAllClick  TButton	btnInsertLeftTopWidthAHeightHintInsert selected symbol into mapCaption&InsertDefault	TabOrder OnClickbtnInsertClick  TButton	btnCancelLeft� TopWidthAHeightHintClose symbol library windowCancel	Caption&CloseTabOrderOnClickbtnCancelClick   TPanelPanel2Left Top Width� HeightzAlignalLeft
BevelOuterbvNoneTabOrder 	TTreeView
SymbolTreeLeft Top Width� HeightzHintSymbols in libraryAlignalClientIndentSortTypestTextTabOrder OnChangeSymbolTreeChange   TPanelPanel3Left� Top WidthHeightzAlignalClientColorclWindowTabOrder 	TPaintBoxLargeSymbolLeftTop� Width
Height� HintPicture of selected symbolAlignalClientColorclWhiteParentColorOnPaintLargeSymbolPaint  TPanelPanel4LeftTopWidth
Height� AlignalTop
BevelInnerbvRaised
BevelOuter	bvLoweredTabOrder  TLabelLabel3LeftTop@Width1HeightCaption	&Comments  TLabelLabel2LeftTop$WidthHeightCaption&Group  TLabelLabel1LeftTopWidthHeightCaption&NameFocusControlSymbolNameText  TButtonbtnEditLeftTop� WidthQHeightHint8Edit symbol attributes: Name, Group, Comments, FavoritesCaption&Edit PropertiesTabOrder OnClickbtnEditClick  TButton	btnDeleteLeft� Top� WidthAHeightHintDelete selected symbolCaptionDe&leteTabOrderOnClickbtnDeleteClick  TButton
btnEditPicLeft`Top� WidthYHeightHint&Edit the symbol's drawn representationCaptionEdit &DrawingTabOrderOnClickbtnEditPicClick  	TCheckBoxchkFavoritesLeft@Top|Width� HeightCaptionSort Symbol &First in ListEnabledTabOrder  	TComboBoxGroupNameListLeft@Top Width� HeightHintGroup of selected symbolStylecsDropDownListEnabled
ItemHeightSorted	TabOrder  TEditSymbolNameTextLeft@TopWidth� HeightHintName of selected symbolEnabledTabOrder  TMemoSymbolCommentsTextLeft@Top8Width� HeightAHintComments for selected symbol.Enabled
ScrollBarsssBothTabOrder     
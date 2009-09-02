;NSIS Modern User Interface
;Basic Example Script
;Written by Joost Verburg

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Phone Directory"
  OutFile "setup.exe"

  ;Default installation folder
  InstallDir "$LOCALAPPDATA\Phone Directory"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\Phone Directory" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

;--------------------------------
;Variables

  Var StartMenuFolder

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "..\LICENSE.txt"
;  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY

  ;Start Menu Folder Page Configuration
  !define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU" 
  !define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\Phone Directory" 
  !define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

  !insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "-Main Program" SecMain

  SetOutPath "$INSTDIR"

  !system 'upx303w\upx.exe --best "..\dist\build\pdirectory\pdirectory.exe" -opdirectory.exe'

  File "pdirectory.exe"
  File "..\LICENSE.txt"
  File "..\README.txt"
  File "dlls\wxc-msw2.8.10-0.11.1.2.dll"

  SetOutPath "$INSTDIR\data\images\"

  File "..\data\images\pdirectory.ico"
  
  ;Store installation folder
  WriteRegStr HKCU "Software\Phone Directory" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Phone Directory.lnk" "$INSTDIR\pdirectory.exe"
  
  !insertmacro MUI_STARTMENU_WRITE_END

  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "DisplayName" "Phone Directory"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "UninstallString" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "Publisher" "Michael Steele"
  WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "URLInfoAbout" "http://www.michaelsteele.us/phonedirectory/"
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "NoModify" 1
  WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory" "NoRepair" 1

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
;  LangString DESC_SecMain ${LANG_ENGLISH} "Phone Directory application."

  ;Assign language strings to sections
;  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
;    !insertmacro MUI_DESCRIPTION_TEXT ${SecMain} $(DESC_SecMain)
;  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  Delete "$INSTDIR\pdirectory.exe"
  Delete "$INSTDIR\LICENSE.txt"
  Delete "$INSTDIR\README.txt"
  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\wxc-msw2.8.10-0.11.1.2.dll"

  Delete "$INSTDIR\data\images\pdirectory.ico"

  RMDir "$INSTDIR\data\images"
  RMDir "$INSTDIR\data"

  RMDir "$INSTDIR"

  !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
    
  Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
  Delete "$SMPROGRAMS\$StartMenuFolder\Phone Directory.lnk"
  RMDir "$SMPROGRAMS\$StartMenuFolder"

  DeleteRegKey /ifempty HKCU "Software\Phone Directory"

  DeleteRegKey HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\Phone Directory"

SectionEnd

;--------------------------------
;Version Information

  VIProductVersion "1.0.0.0"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "Phone Directory"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "Comments" "Create a nicely formatted directory of phone numbers"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "Michael Steele"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalTrademarks" ""
  VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "Copyright (C) 2009 Michael Steele"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "Phone Directory"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "1.0.0"
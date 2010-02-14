Name    "Erlyvideo"       ; The name of the installation
OutFile "Erlyvideo-1.4.2.exe"   ; The name of the unistaller file to write
InstallDir "C:\Program Files\Erlyvideo"

Page directory
Page instfiles
XPStyle on
RequestExecutionLevel admin

!define UNINST_REG "Software\Microsoft\Windows\CurrentVersion\Uninstall\$(^Name)"
!define UNINST_EXE "ErlyvideoUninstall.exe"
!define SERVICE_PARAM "System\CurrentControlSet\Services\$(^Name)\Parameters"
 
Section "Install"
  SetOutPath $INSTDIR

  WriteUninstaller "$INSTDIR\uninstall.exe"
  WriteRegStr HKLM "${UNINST_REG}" "DisplayName" "Erlyvideo"
  WriteRegStr HKLM "${UNINST_REG}" "DisplayVersion" "1.4.2"
  WriteRegStr HKLM "${UNINST_REG}" "InstallLocation" "$INSTDIR"
  WriteRegStr HKLM "${UNINST_REG}" "Published" "Erlyvideo"
  WriteRegStr HKLM "${UNINST_REG}" "UninstallString" "$INSTDIR\${UNINST_EXE}"
  WriteRegStr HKLM "${UNINST_REG}" "URLInfoAbout" "http://erlyvideo.org"
  WriteRegDWORD HKLM "${UNINST_REG}" "NoModify" 1
  WriteRegDWORD HKLM "${UNINST_REG}" "NoRepair" 1

  File /r ..\*.*
  ExecWait 'erlsrv.exe add Erlyvideo -onfail restart -env "lib;deps;plugins" -workdir "C:\Program Files\Erlyvideo" -name ems' $0
  DetailPrint "Erlyvideo service returned $0"
;  WriteRegStr HKLM "${SERVICE_PARAM}" "Application" "$INSTDIR\Erlyvideo.exe"
;  WriteRegStr HKLM "${SERVICE_PARAM}" "AppParameters" ""
;  WriteRegStr HKLM "${SERVICE_PARAM}" "AppDirectory" "$INSTDIR"
  createShortCut "$SMPROGRAMS\ErlyvideoUninstall.lnk" "$INSTDIR\uninstall.exe"
  nsSCM::Start /NOUNLOAD "Erlyvideo"
SectionEnd
 
; Set prompt text for uninstall window
UninstallText "This will uninstall Squid. Press 'Uninstall' to continue."
 
; Define steps to unistall everything installed.
Section "Uninstall"
  nsSCM::Stop /NOUNLOAD "$(^Name)"
  nsSCM::Remove /NOUNLOAD "$(^Name)"
  Processes::KillProcess "$(^Name)"
  DeleteRegKey HKLM "${UNINST_REG}"
  DeleteRegKey HKLM "System\CurrentControlSet\Services\$(^Name)"
  Delete "$INSTDIR\uninstall.exe"
  Delete "$SMPROGRAMS\ErlyvideoUninstall.lnk"
  DetailPrint "$(^Name) removing"
  RMDir /r /REBOOTOK $INSTDIR
SectionEnd
 
; eof

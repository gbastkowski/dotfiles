## 1. Emacs Command Placement

- [x] 1.1 Identify the general utilities file in `emacs/` for new interactive commands
- [x] 1.2 Add the new command definition in the chosen utilities file

## 2. Org Region Export Command

- [x] 2.1 Implement an interactive function that validates org-mode and an active region
- [x] 2.2 Convert the active region to Markdown using `org-export-string-as` with `ox-md`
- [x] 2.3 Copy the Markdown output to the system clipboard using `kill-new`

## 3. User Feedback and Validation

- [x] 3.1 Provide clear user-facing errors for missing region or non-org buffers
- [x] 3.2 Verify the command exists without adding a default keybinding
